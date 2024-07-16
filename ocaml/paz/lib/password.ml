let for_username s = match s with
        | "" -> ""
        | s -> " for " ^ s

let echo t =
        let module U = Unix in
        let tio = U.tcgetattr U.stdin in
        tio.c_echo <- t;
        U.tcsetattr U.stdin U.TCSANOW tio

let get_password maybe username = match maybe with
        | None -> (
                let s = ref "" in
                (Printf.fprintf stderr "Password%s: %!" (for_username username));
                 echo false;
                 s := read_line ();
                 echo true;
                 !s)
        | Some s -> s
