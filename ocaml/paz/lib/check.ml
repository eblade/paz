let enough_iterations required actual =
        actual >= required

let check_rules s =
        let is_in_range a z c =
                let a' = Char.code a in
                let z' = Char.code z in
                let c' = Char.code c in
                c' >= a' && c' <= z' in
        let has_in_range a z =
                String.exists @@ is_in_range a z in
        let first_in_range a z s = is_in_range a z @@ String.get s 0 in
        let fs = [ has_in_range 'A' 'Z';
                   has_in_range 'a' 'z';
                   has_in_range '0' '9';
                   first_in_range 'a' 'z' ] in
        List.fold_left (&&) true @@ List.map (function f -> f(s)) fs
