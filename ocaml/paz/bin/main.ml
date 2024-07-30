let usage_msg = "paz [opts] site"
let ini_path = Unix.getenv "HOME" ^ "/.pazrc"
let verbose = ref false
let linebreak = ref false
let bishop = ref false
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
  [
    ("-v", Arg.Set verbose, "Print options to stderr");
    ("-H", Arg.Set_string hash, "Hash function (SHA512, MD5)");
    ("-i", Arg.Set_int min_iterations, "Miniumum number of hash iterations");
    ("-n", Arg.Set_int length, "Password length");
    ("-r", Arg.Set_int revision, "Password revision");
    ("-l", Arg.Set linebreak, "Append a linebreak to password");
    ("-b", Arg.Set bishop, "Print a drunken bishop thingy");
    ("-a", Arg.Set_string addition, "Append this string to the password");
    ("-u", Arg.Set_string username, "Login username");
    ("-S", Arg.Set_string strategy, "Password strategy");
    ("-m", Arg.Set_string master, "Specify master password");
  ]

let run_password (cli_params : Paz.Params.user_params) =
  let module P = Paz.Params in
  let module I = Paz.Ini in
  let module H = Paz.Hashing in
  let module B = Paz.Bishop in
  let merged =
    cli_params
    |> P.merge (I.read_section ini_path (Option.get cli_params.site))
    |> P.merge (I.read_section ini_path "DEFAULT")
    |> P.merge P.defaults
  in
  let _ = if !verbose then P.print_params merged else () in
  let params = P.finalize merged in
  let _ =
    if !bishop then
      params.master ^ "\n" |> H.get_hasher H.SHA512 |> B.drunken_walk |> B.print
    else ()
  in
  print_endline
    (H.make_password params.source params.hash params.min_iterations
       params.length
    ^ params.ending)

let run_list_sites () =
  let sites = Paz.Ini.read_sections ini_path in
  List.iter print_endline sites

let run () =
  let module M = Paz.Maybe in
  let module P = Paz.Params in
  let module H = Paz.Hashing in
  let (cli_params : P.user_params) =
    {
      linebreak = M.not !linebreak;
      master = M.empty !master;
      site = (match !sites with [] -> None | h :: _ -> Some h);
      hash = H.parse_hashtype !hash;
      min_iterations = M.zero !min_iterations;
      length = M.zero !length;
      addition = M.empty !addition;
      username = M.empty !username;
      strategy = M.empty !strategy;
      revision = M.zero !revision;
    }
  in
  match cli_params.site with
  | Some _ -> run_password cli_params
  | None -> run_list_sites ()

let () =
  Arg.parse speclist anon_site_fun usage_msg;
  run ()
