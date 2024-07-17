let usage_msg = "paz [opts] site"

let verbose = ref false

let linebreak = ref false

let sites = ref []

let master = ref ""

let hash = ref ""

let min_iterations = ref 0

let length = ref 0

let addition = ref ""

let username = ref ""

let strategy = ref ""

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


let run () =
        let module M = Paz.Maybe in
        let module P = Paz.Params in
        let module H = Paz.Hashing in
        let (cli_params : Paz.Params.incomplete_params) =
                { verbose = M.not !verbose;
                  linebreak = M.not !linebreak;
                  master = M.empty !master;
                  site = (match !sites with
                        | [] -> None
                        | h :: _ -> Some h);
                  hash = H.parse_hashtype !hash;
                  min_iterations = M.zero !min_iterations;
                  length = M.zero !length;
                  addition = M.empty !addition;
                  username = M.empty !username;
                  strategy = M.empty !strategy;
                  revision = M.zero !revision;
                } in
        let params = P.finalize @@ P.merge cli_params P.defaults in
        print_endline ((H.make_password
                        params.source
                        params.hash
                        params.min_iterations
                        params.length)
                       ^ params.ending)

let () =
        Arg.parse speclist anon_site_fun usage_msg;
        run ();

