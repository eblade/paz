let zero n = if n = 0 then None else Some n

let empty s = if s = "" then None else Some s

let not t = if t then Some true else None
