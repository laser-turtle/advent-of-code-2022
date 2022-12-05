open! Core

let priority ch =
    if Char.is_lowercase ch then (
        Char.to_int ch - Char.to_int 'a' + 1
    ) else ( 
        Char.to_int ch - Char.to_int 'A' + 27
    )

let _ =
    "./03-2/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:(fun l -> Char.Set.of_list (String.to_list l))
    |> List.chunks_of ~length:3
    |> List.map ~f:(function
        | [s1; s2; s3] -> 
            Char.Set.inter s1 (Char.Set.inter s2 s3)
            |> Char.Set.choose_exn
        | _ -> failwith "bad list"
    )
    |> List.map ~f:priority
    |> List.fold_left ~init:0 ~f:(+)
    |> Printf.printf "%d\n%!"
