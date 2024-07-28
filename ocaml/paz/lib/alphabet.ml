let base64_sgp_alphabet_string =
	let make_range start stop =
		let start_code = Char.code start in
		let stop_code = Char.code stop in
		let length = stop_code - start_code + 1 in
		List.init length ((+) start_code)
		|> List.map Char.chr
		|> List.to_seq
		|> String.of_seq in
        let upper = make_range 'A' 'Z'
        and lower = make_range 'a' 'z'
        and nums = make_range '0' '9' in
        upper ^ lower ^ nums ^ "98"

let clean_padding = String.map (function | '=' -> 'A' | c -> c)
