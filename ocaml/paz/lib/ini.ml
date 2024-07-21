let read_sections filename =
        let f = open_in filename in
        let sw = String.starts_with ~prefix:"[" in
        let ew = String.ends_with ~suffix:"]" in
        let make_section s = (if (sw s && ew s)
                then Some (String.sub s 1 ((String.length s) - 2))
                else None) in
        let rec collect l =
                match input_line f with
                | line -> (collect (match make_section line with
                        | Some "DEFAULT" -> l
                        | Some section -> (section :: l)
                        | None -> l))
                | exception End_of_file -> close_in f; List.rev l in
        List.sort compare @@ collect []
