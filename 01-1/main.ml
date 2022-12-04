open! Core

let _ =
    "./01-1/input.txt"
    |> In_channel.read_lines
    |> List.group ~break:(fun a b -> String.(a = "" || b = ""))
    |> List.filter ~f:(function
        | [""] -> false
        | _ -> true
    )
    |> List.map ~f:(fun lst ->
            lst
            |> List.map ~f:Int.of_string
            |> List.fold_left ~init:0 ~f:(+)
    )
    |> List.max_elt ~compare:Int.compare
    |> (function
       | None -> "No elves"
       | Some v -> Int.to_string v
    )
    |> print_endline
;;
