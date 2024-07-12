let make_alphabet_part start stop =
        let start_code = Char.code start in
        let stop_code = Char.code stop in
        let length = stop_code - start_code + 1 in
        List.init length (function x -> x + start_code)
        |> List.map Char.chr
        |> List.to_seq
        |> String.of_seq

let base64_sgp_alphabet_string =
        let upper = make_alphabet_part 'A' 'Z' in
        let lower = make_alphabet_part 'a' 'z' in
        let nums = make_alphabet_part '0' '9' in
        upper ^ lower ^ nums ^ "98"

let clean_padding = String.map (function c -> if c = '=' then 'A' else c)
