open! Core

let _ =
    "./01-2/input.txt"
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
    |> List.sort ~compare:(fun a b -> Int.compare b a)
    |> (fun l -> List.take l 3)
    |> List.fold_left ~init:(0) ~f:(+) 
    |> Printf.printf "%d\n"
;;
