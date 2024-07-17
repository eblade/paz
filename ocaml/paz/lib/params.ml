type incomplete_params =
        { verbose : bool option;
          linebreak : bool option;
          site : string option;
          master : string option;
          hash : Hashing.hashtype option;
          min_iterations : int option;
          length : int option;
          addition : string option;
          username : string option;
          strategy : string option;
          revision : int option;
        }

type params =
        { verbose : bool;
          source : string;
          hash : Hashing.hashtype;
          min_iterations : int;
          length : int;
          username : string option;
          strategy : string;
          revision : int option;
          ending : string;
        }

let defaults =
        { verbose = Some false;
          linebreak = Some false;
          site = None;
          master = None;
          hash = Some Hashing.SHA512;
          min_iterations = Some 10;
          length = Some 15;
          addition = Some "";
          username = None;
          strategy = Some "default";
          revision = None;
        }

let pick a b = match a with
        | Some _ -> a
        | None -> b

let merge (a : incomplete_params) (b : incomplete_params) =
        { verbose = pick a.verbose b.verbose;
          linebreak = pick a.linebreak b.linebreak;
          site = pick a.site b.site;
          master = pick a.master b.master;
          hash = pick a.hash b.hash;
          min_iterations = pick a.min_iterations b.min_iterations;
          length = pick a.length b.length;
          addition = pick a.addition b.addition;
          username = pick a.username b.username;
          strategy = pick a.strategy b.strategy;
          revision = pick a.revision b.revision;
        } 

let make_source_str site master opt_revision =
        master ^ ":" ^ site ^ (match opt_revision with
        | Some revision -> Int.to_string revision
        | None -> "")

let finalize (p : incomplete_params) =
        let g = Option.get in
        { verbose = g p.verbose;
          source = make_source_str
                (g p.site)
                (Password.get_password
                 p.master
                 p.username
                 (g p.strategy))
                p.revision;
          hash = g p.hash;
          min_iterations = g p.min_iterations;
          length = g p.length;
          username = p.username;
          strategy = g p.strategy;
          revision = p.revision;
          ending = (g p.addition) ^ (
                    if (g p.linebreak)
                    then "\n" else "")
        } 
