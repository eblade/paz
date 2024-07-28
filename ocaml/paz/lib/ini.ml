let parse_title s = 
        let sw = String.starts_with ~prefix:"["
        and ew = String.ends_with ~suffix:"]" in
        (if (sw s && ew s)
                then Some (String.sub s 1 ((String.length s) - 2))
                else None)

let read_sections filename =
        if Sys.file_exists filename
        then let f = open_in filename in
        let rec collect l =
                match input_line f with
                | line -> collect (match parse_title line with
                        | Some "DEFAULT" -> l
                        | Some section -> (section :: l)
                        | None -> l)
                | exception End_of_file -> close_in f; List.rev l in
        List.sort compare @@ collect []
        else []

let read_section filename wanted_section =
        if Sys.file_exists filename
        then let f = open_in filename in
        let parse_param line = 
                match List.map String.trim (String.split_on_char '=' line) with
                | key :: value :: [] -> Some (key, value)
                | _ -> None in
        let rec collect inside params =
                match input_line f with
                | line -> (match parse_title line with
                        | Some section -> collect (section = wanted_section) params
                        | None -> if inside
                                then match parse_param line with
                                | Some (key, value) -> collect true (Params.apply params key value)
                                | None -> collect true params
                                else collect false params)
                | exception End_of_file -> close_in f; params in
        collect false Params.empty
        else Params.empty
