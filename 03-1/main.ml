open! Core

let priority ch =
    if Char.is_lowercase ch then (
        Char.to_int ch - Char.to_int 'a' + 1
    ) else ( 
        Char.to_int ch - Char.to_int 'A' + 27
    )

let _ =
    "./03-1/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:(fun line ->
        let mid = String.length line / 2 in
        String.sub line ~pos:0 ~len:mid,
        String.sub line ~pos:mid ~len:mid
    )
    |> List.map ~f:(fun (a, b) ->
        let a = Char.Set.of_list (String.to_list a) in
        let b = Char.Set.of_list (String.to_list b) in
        Char.Set.inter a b |> Char.Set.choose_exn
    )
    |> List.map ~f:priority
    |> List.fold_left ~init:0 ~f:(+)
    |> Printf.printf "%d\n%!"
