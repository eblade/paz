let enough_iterations required actual =
        actual >= required

let check_rules s =
        let is_in_range a z c =
                let a' = Char.code a
                and z' = Char.code z
                and c' = Char.code c in
                c' >= a' && c' <= z' in
        let has_in_range a z = String.exists @@ is_in_range a z
        and first_in_range a z s = is_in_range a z @@ String.get s 0 in
        let fs = [ has_in_range 'A' 'Z';
                   has_in_range 'a' 'z';
                   has_in_range '0' '9';
                   first_in_range 'a' 'z' ] in
        List.fold_left (&&) true @@ List.map (fun f -> f(s)) fs
