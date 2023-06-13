use fmt;
use getopt;
use os;
use strconv;
use bufio;
use io;
use crypto::sha512;
use hash;
use strings;
use encoding::hex;
use encoding::base64;
use unix::tty;

type config = struct {
	site: str,
	master: str,
	hashname: str,
	length: int,
	iterations: int,
	revision: int,
	addition: str,
	stdout: bool,
	linebreak: bool,
	verbose: bool,
	debug: bool,
};

export fn main() void = {
	const cmd = getopt::parse(os::args,
		"deterministic password generator",
		('m', "master", "master password"),
		('H', "hash", "hash algorithm"),
		('n', "length", "password length, not counting addition"),
		('i', "iter", "minimum number of iterations"),
		('r', "revision", "add this number to the site"),
		('a', "addition", "add this suffix to the site"),
		('s', "print result to stdout"),
		('l', "add a line break to the result"),
		('v', "print out the config and maybe other stuff too"),
		('d', "print out more debug"),
		"site",
	);
	defer getopt::finish(&cmd);

	if (len(cmd.args) == 0) {
		getopt::printusage(os::stderr, "paz", cmd.help...)!;
		return;
	};

	if (len(cmd.args) > 1) {
		fmt::fprintln(os::stderr, "Too many arguments!")!;
		getopt::printusage(os::stderr, "paz", cmd.help...)!;
		return;
	};

	let cfg = create_default_config();
	for (let i = 0z; i < len(cmd.opts); i += 1) {
		const opt = cmd.opts[i];
		switch (opt.0) {
		case 'm' =>
			cfg.master = opt.1;
		case 'H' =>
			cfg.hashname = opt.1;
		case 'n' =>
			cfg.length = strconv::stoi(opt.1)!;
		case 'i' =>
			cfg.iterations = strconv::stoi(opt.1)!;
		case 'r' =>
			cfg.revision = strconv::stoi(opt.1)!;
		case 'a' =>
			cfg.addition = opt.1;
		case 's' =>
			cfg.stdout = true;
		case 'l' =>
			cfg.linebreak = true;
		case 'v' =>
			cfg.verbose = true;
		case 'd' =>
			cfg.debug = true;
		case => abort();
		};
	};

	cfg.site = cmd.args[0];

	if (cfg.verbose) print_config(cfg)!;

	if (len(cfg.master) == 0) {
		fmt::error("Password: ")!;
		bufio::flush(os::stdout)!;
		const tty = match (tty::open()) {
		case let file: io::file =>
			yield file;
		case let err: tty::error =>
			fmt::fatal("Error opening tty:", tty::strerror(err));
		};
		defer io::close(tty)!;
		const termios = tty::termios_query(tty)!;
		defer tty::termios_restore(&termios);
		tty::noecho(&termios)!;
		cfg.master = strings::fromutf8(bufio::scanline(os::stdin)! as []u8)!;
		fmt::println()!;
	};
	if (len(cfg.master) == 0) {
		fmt::fatal("Error: missing master password");
	};

	const start = strings::join(":", cfg.master, cfg.site);
	const hash = sha512::sha512();
	defer hash::close(&hash);
	const result = iterate(start, &hash, cfg.iterations, cfg.length, cfg.debug)!;
	fmt::println(*result)!;
};

fn iterate(in: str, hash: *hash::hash, iterations: int, plength: int, debug: bool) (*str | io::error) = {
	const a = strings::toutf8(in);
	const b: [sha512::SIZE]u8 = [0...];
	const c: [sha512::SIZE * 2]u8 = [0...];

	let n = 0;
	for (true) {
		hash::reset(hash);
		io::writeall(hash, a)?;
		hash::sum(hash, b);
		const length = do_encode(b, &c)?;
		a = c[..length];
		if (debug) {
			fmt::printfln("{}\t{}", n, strings::fromutf8(a[..plength])!)?;
		};

		n += 1;
		if (n < iterations) {
			continue;
		};

		if (!(a[0] >= 'a' && a[0] <= 'z')) {
			if (debug) {
				fmt::println("does not start with lowercase")?;
			};
			continue;
		};

		let containsUpper = false;
		let containsNumeral = false;
		for (let i = 1; !(containsUpper && containsNumeral) && i < plength; i += 1) {
			if ((a[i] >= 'A' && a[i] <= 'Z')) {
				containsUpper = true;
			} else if ((a[i] >= '0' && a[i] <= '9')) {
				containsNumeral = true;
			};
		};

		if (!containsUpper) {
			if (debug) {
				fmt::println("does not contain uppercase")?;
			};
			continue;
		} else if (!containsNumeral) {
			if (debug) {
				fmt::println("does not contain numeral")?;
			};
			continue;
		};


		break;
	};

	return &strings::fromutf8(a[..plength])!;
};

fn do_encode(in: []u8, out: []u8) (size | io::error) = {
	const buf = bufio::fixed(out, io::mode::WRITE);
	//io::seek(&buf, 0, io::whence::SET)?;
	const length = base64::encodedsize(base64::encode(&buf, &base64::std_encoding, in)?);
	io::close(&buf)?;
	for (let i = 0z; i < length; i += 1) {
		const x = out[i];
		if (x == '+') {
			out[i] = '9';
		} else if (x == '/') {
			out[i] = '8';
		} else if (x == '=') {
			out[i] = 'A';
		};
	};
	return length;
};

fn create_default_config() config = {
	return config {
		site = "",
		master = "",
		hashname = "sha512",
		length = 15,
		iterations = 10,
		revision = 0,
		addition = "",
		stdout = false,
		linebreak = false,
		verbose = false,
		debug = false,
	};
};

fn print_config(cfg: config) (void | io::error) = {
	fmt::fprintfln(os::stderr, "site: {}", cfg.site)?;
	fmt::fprintfln(os::stderr, "hashname: {}", cfg.hashname)?;
	fmt::fprintfln(os::stderr, "length: {}", cfg.length)?;
	fmt::fprintfln(os::stderr, "iterations: {}", cfg.iterations)?;
	fmt::fprintfln(os::stderr, "revision: {}", cfg.revision)?;
	fmt::fprintfln(os::stderr, "addition: {}", cfg.addition)?;
	fmt::fprintfln(os::stderr, "stdout: {}", cfg.stdout)?;
	fmt::fprintfln(os::stderr, "linebreak: {}", cfg.linebreak)?;
};

//@test fn hash_and_encode() void = {
//	const start = strings::toutf8("master:site");
//	const hashed: [sha512::SIZE]u8 = [0...];
//	do_sha512(start, &hashed)!;
//	const encoded: [128]u8 = [0...];
//	const length = do_encode(hashed, &encoded)!;
//	fmt::printfln("Length {}", length)!;
//	const result = strings::fromutf8(encoded[..length])!;
//	hex::encode(os::stdout, encoded[..length])!;
//	fmt::println("")!;
//	fmt::println(result)!;
//	fmt::println("hej")!;
//	assert(strings::compare(result,
//		       "SCBKYVQA1mR8s8bnJKMrQngCwRO6C862gbe4sVk2C6sWHs005mJKC7AnEmmKF5e0lK49OpbSUOY8LWf3yIi8YQAA") == 0,
//	       "string is wrong");
//};
