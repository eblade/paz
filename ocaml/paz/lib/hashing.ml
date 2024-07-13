type hashtype =
        | SHA512
        | SHA256
        | MD5

let parse_hashtype s = match (String.uppercase_ascii s) with
        | "SHA512" -> SHA512
        | "SHA256" -> SHA256
        | "MD5" -> MD5
        | _ -> raise (Invalid_argument ("Unsupported hash function: " ^ s))

let get_hashname x = match x with
        | SHA512 -> "SHA512"
        | SHA256 -> "SHA256"
        | MD5 -> "MD5"

let get_hasher x =
        let module D = Digest in
        match x with
        | SHA512 -> (function s -> Sha512.string s |> Sha512.to_bin)
        | SHA256 -> (function s -> Sha256.string s |> Sha256.to_bin)
        | MD5 -> D.MD5.string

let make_source_str site master opt_revision =
        master ^ ":" ^ site ^ (match opt_revision with
        | Some revision -> Int.to_string revision
        | None -> "")

let make_password source hashtype min_iterations password_length =
        let hasher = get_hasher hashtype in
        let check_iterations = Check.enough_iterations min_iterations in
        let cut = function s -> String.sub s 0 password_length in
        let alphabet = Base64.make_alphabet Alphabet.base64_sgp_alphabet_string in
        let rec iterate iteration s =
                let hash = hasher s
                |> Base64.encode_string ~alphabet:alphabet
                |> Alphabet.clean_padding in
                let result = cut hash in
                if (check_iterations iteration && Check.check_rules result)
                then result
                else (iterate (iteration + 1) hash) in
        iterate 1 source