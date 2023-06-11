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
		case => abort();
		};
	};

	cfg.site = cmd.args[0];

	if (cfg.verbose) print_config(cfg)!;

	const start = strings::toutf8(strings::join(":", cfg.master, cfg.site));
	fmt::println(strings::fromutf8(start)!)!;
	fmt::printfln("Length {}", len(start))!;
	const hashed: [sha512::SIZE]u8 = [0...];
	do_sha512(start, &hashed)!;
	const encoded: [128]u8 = [0...];
	const length = do_encode(hashed, &encoded)!;
	fmt::printfln("Length {}", length)!;
	const result = strings::fromutf8(encoded[..length])!;
	hex::encode(os::stdout, encoded[..length])!;
	fmt::println("")!;
	fmt::println(result)!;
	fmt::println("hej")!;
};

fn do_sha512(in: []u8, out: []u8) (void | io::error) = {
	const hash = sha512::sha512();
	io::writeall(&hash, in)?;
	hash::sum(&hash, out);
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

@test fn hash_and_encode() void = {
	const start = strings::toutf8("master:site");
	const hashed: [sha512::SIZE]u8 = [0...];
	do_sha512(start, &hashed)!;
	const encoded: [128]u8 = [0...];
	const length = do_encode(hashed, &encoded)!;
	fmt::printfln("Length {}", length)!;
	const result = strings::fromutf8(encoded[..length])!;
	hex::encode(os::stdout, encoded[..length])!;
	fmt::println("")!;
	fmt::println(result)!;
	fmt::println("hej")!;
	assert(strings::compare(result,
		       "SCBKYVQA1mR8s8bnJKMrQngCwRO6C862gbe4sVk2C6sWHs005mJKC7AnEmmKF5e0lK49OpbSUOY8LWf3yIi8YQAA") == 0,
	       "string is wrong");
};
