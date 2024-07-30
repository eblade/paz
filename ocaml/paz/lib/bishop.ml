type direction = NW | NE | SW | SE

let int_to_direction = function
  | 0 -> NW
  | 1 -> NE
  | 2 -> SW
  | 3 -> SE
  | _ -> raise (Invalid_argument "bad direction")

let xlim = 17
let ylim = 9
let arsz = xlim * ylim

let print board =
  let symbols = " .o+=*BOX@%&#/^SE" in
  let translate l =
    Array.map (String.get symbols) l |> Array.to_seq |> String.of_seq
  in
  let put = Printf.fprintf stderr "%s\n" in
  let rec put_rows start =
    if start < arsz then (
      let s = translate (Array.sub board start xlim) in
      put ("|" ^ s ^ "|");
      put_rows (start + xlim))
    else ()
  in
  put "\n+--[ RandomArt ]--+";
  put_rows 0;
  put "+-----------------+"

let drunken_walk (s : string) =
  let l = s |> String.to_seq |> Seq.map Char.code |> List.of_seq in
  let board = Array.make arsz 0 in
  let move pos dir =
    let limit min max x = Int.min max @@ Int.max min x in
    let x = pos mod xlim and y = pos / xlim in
    let x', y' =
      match dir with
      | NW -> (x - 1, y - 1)
      | NE -> (x + 1, y - 1)
      | SW -> (x - 1, y + 1)
      | SE -> (x + 1, y + 1)
    in
    (limit 0 (ylim - 1) y' * xlim) + limit 0 (xlim - 1) x'
  in
  (* stage is a group 0-3 of 2 bits in an 8 bit int (ascii code).
     it wil give the direction to move (of the above four) *)
  let rec walk pos stage l =
    match (stage, l) with
    | _, [] -> ()
    | 4, _ :: xs -> walk pos 0 xs
    | stage, x :: xs ->
        let pos' = move pos @@ int_to_direction (Int.logand x 3) in
        board.(pos') <- (board.(pos') + if pos' = pos then 0 else 1);
        walk pos' (stage + 1) (Int.shift_right x 2 :: xs)
  in
  walk 76 0 l;
  board
