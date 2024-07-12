let enough_iterations required actual =
        actual >= required

let has_in_range a z =
        let p c = ((Char.code c) >= a) && ((Char.code c) <= z) in
        String.exists p

let is_lower c =
        let a = Char.code 'a' in
        let z = Char.code 'z' in
        let n = Char.code c in
        n >= a && n <= z

let has_upper = has_in_range (Char.code 'A') (Char.code 'Z')
let has_lower = has_in_range (Char.code 'a') (Char.code 'z')
let has_number = has_in_range (Char.code '0') (Char.code '9')
let starts_with_lower s = is_lower @@ String.get s 0
let check_rules s = has_upper s && has_lower s && has_number s && starts_with_lower s
