let for_username s = match s with
        | None -> ""
        | Some s -> " for " ^ s

let echo t =
        let module U = Unix in
        let tio = U.tcgetattr U.stdin in
        tio.c_echo <- t;
        U.tcsetattr U.stdin U.TCSANOW tio

let get_password master username strategy = match master with
        | None -> (
                let s = ref "" in
                (Printf.fprintf stderr "Password%s (%s): %!" (for_username username) strategy);
                 echo false;
                 s := read_line ();
                 echo true;
                 !s)
        | Some s -> s
