type direction = NW | NE | SW | SE

let int_to_direction i = match i with
        | 0 -> NW
        | 1 -> NE
        | 2 -> SW
        | 3 -> SE
        | _ -> raise (Invalid_argument "bad direction")

let xlim = 17
let ylim = 9
let arsz = xlim * ylim
let symbols = " .o+=*BOX@%&#/^SE"
let translate l =
        let f n = String.get symbols n in
        Seq.map f l
        |> String.of_seq
let init = List.init arsz (function _ -> 0)

let print board =
        let s = List.to_seq board in
        let put = Printf.fprintf stderr "%s\n" in
        let put_row () =
                put ("|" ^ (translate (Seq.take xlim s)) ^ "|") in
        let rec put_rows n = (if n < arsz
        then (put_row (); put_rows (n + xlim))
        else ()) in
        put "+--[ RandomArt ]--+";
        put_rows 0;
        put "+-----------------+"
