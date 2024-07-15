open Paz.Hashing

let usage_msg = "paz [opts] site"

let verbose = ref false

let linebreak = ref false

let sites = ref []

let master = ref ""

let hash = ref "SHA512"

let min_iterations = ref 10

let length = ref 15

let addition = ref ""

let username = ref ""

let strategy = ref "default"

let revision = ref 0

let anon_site_fun site = sites := site :: !sites

let speclist =
        [("-v", Arg.Set verbose, "Print options to stderr");
         ("-H", Arg.Set_string hash, "Hash function (SHA512, MD5)");
         ("-i", Arg.Set_int min_iterations, "Miniumum number of hash iterations");
         ("-n", Arg.Set_int length, "Password length");
         ("-r", Arg.Set_int revision, "Password revision");
         ("-l", Arg.Set linebreak, "Append a linebreak to password");
         ("-a", Arg.Set_string addition, "Append this string to the password");
         ("-u", Arg.Set_string username, "Login username");
         ("-S", Arg.Set_string strategy, "Password strategy");
         ("-m", Arg.Set_string master, "Specify master password")]

let for_username s = match s with
        | "" -> ""
        | s -> " for " ^ s

let echo t =
        let tio = Unix.tcgetattr Unix.stdin in
        tio.c_echo <- t;
        Unix.tcsetattr Unix.stdin Unix.TCSANOW tio

let get_password () = match !master with
        | "" -> (
                (Printf.fprintf stderr "Password%s: %!" (for_username !username));
                 echo false;
                 master := read_line ();
                 echo true;
                 !master)
        | s -> s

let opt_zero n = if n = 0 then None else Some n

let run () =
        let site = (match !sites with
                | [] -> ""
                | h :: _ -> h) in
        let source = make_source_str site (get_password ()) (opt_zero !revision) in
        let hashtype = parse_hashtype !hash in
        let ending = if !linebreak then "\n" else "" in
        print_endline @@ (make_password source hashtype !min_iterations !length) ^ !addition ^ ending

let () =
        Arg.parse speclist anon_site_fun usage_msg;
        run ();

