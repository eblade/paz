type user_params =
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
          master : string;
          hash : Hashing.hashtype;
          min_iterations : int;
          length : int;
          username : string option;
          strategy : string;
          revision : int option;
          ending : string;
        }

let empty =
        { linebreak = None;
          site = None;
          master = None;
          hash = None;
          min_iterations = None;
          length = None;
          addition = None;
          username = None;
          strategy = None;
          revision = None;
        }

let defaults =
        { empty with
          linebreak = Some false;
          hash = Some Hashing.SHA512;
          min_iterations = Some 10;
          length = Some 15;
          addition = Some "";
          strategy = Some "default";
        }

let apply (p : user_params) key value =
        let lower = String.lowercase_ascii in
        let trueness t = match lower t with
        | "true" -> true
        | "false" -> false
        | "yes" -> true
        | "no" -> false
        | _ -> raise (Invalid_argument ("Not true/false " ^ key ^ " = " ^ value)) in
        match lower key with
        | "linebreak" -> { p with linebreak = Some (trueness value) }
        | "hash" -> { p with hash = Hashing.parse_hashtype value }
        | "min-iterations" -> { p with min_iterations = Some (int_of_string value) }
        | "length" -> { p with length = Some (int_of_string value) }
        | "addition" -> { p with addition = Some value }
        | "username" -> { p with username = Some value }
        | "strategy" -> { p with strategy = Some value }
        | "revision" -> { p with revision = Some (int_of_string value) }
        | _ -> raise (Invalid_argument ("Not a valid param " ^ key ^ " = " ^ value))

(* prefer b, so the function may be piped *)
let merge (a : user_params) (b : user_params) =
        let pick a b = match b with
                | Some _ -> b
                | None -> a in
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


let finalize (p : user_params) =
        let g = Option.get in
        let master = Password.get_password p.master p.username (g p.strategy) in
        let source =
                master ^ ":" ^ (g p.site) ^ (match p.revision with
                | Some revision -> Int.to_string revision
                | None -> "") in
        { source = source;
          master = master;
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

let print_params (p : user_params) =
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
