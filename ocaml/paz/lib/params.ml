type incomplete_params =
        { linebreak : bool option;
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
        { source : string;
          hash : Hashing.hashtype;
          min_iterations : int;
          length : int;
          username : string option;
          strategy : string;
          revision : int option;
          ending : string;
        }

let defaults =
        { linebreak = Some false;
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
        { linebreak = pick a.linebreak b.linebreak;
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
        { source = make_source_str
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

let print_params (p : incomplete_params) =
        let print = Printf.fprintf stderr "%s = %s\n" in
        let pt name maybe = match maybe with
                | Some x -> (print name x)
                | None -> (print name "(unset)") in
        let m f maybe = match maybe with
                | Some x -> Some (f x)
                | None -> None in
        pt "site" p.site;
        pt "hash" @@ m Hashing.get_hashname p.hash;
        pt "min_iterations" @@ m Int.to_string p.min_iterations;
        pt "length" @@ m Int.to_string p.length;
        pt "username" p.username;
        pt "strategy" p.strategy;
        pt "revision" @@ m Int.to_string p.revision;
        pt "addition" p.addition;
