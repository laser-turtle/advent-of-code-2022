open! Core

type t = Rock
       | Paper
       | Scissors

let of_string = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | s -> failwith s

let score = function (*them, you*)
    | Rock, Rock -> 3 + 1
    | Paper, Paper -> 3 + 2
    | Scissors, Scissors -> 3 + 3
    | Rock, Paper -> 6 + 2
    | Rock, Scissors -> 0 + 3
    | Paper, Rock -> 0 + 1
    | Paper, Scissors -> 6 + 3
    | Scissors, Rock -> 6 + 1
    | Scissors, Paper -> 0 + 2

let _ =
    "./02-1/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(function
        | [them; you] -> of_string them, of_string you
        | _ -> failwith "should have two moves"
    )
    |> List.map ~f:score
    |> List.fold_left ~init:0 ~f:(+)
    |> Printf.printf "%d\n"
