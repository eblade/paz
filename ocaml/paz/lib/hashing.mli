type hashtype = SHA512 | SHA256 | MD5
val parse_hashtype : string -> hashtype option
val get_hashname : hashtype -> string
val make_password : string -> hashtype -> int -> int -> string
