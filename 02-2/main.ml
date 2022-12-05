open! Core

type t = Rock
       | Paper
       | Scissors

type outcome = Win
             | Tie
             | Lose
       
let outcome = function
    | "X" -> Lose
    | "Y" -> Tie
    | "Z" -> Win
    | s -> failwith s

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

let fixup them you = 
    let them = of_string them in
    let you = outcome you in
    let you =
        match them, you with
        | Rock, Win -> Paper
        | Paper, Win -> Scissors
        | Scissors, Win -> Rock
        | a, Tie -> a
        | Rock, Lose -> Scissors
        | Paper, Lose -> Rock
        | Scissors, Lose -> Paper
    in
    them, you

let _ =
    "./02-2/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(function
        | [them; you] -> fixup them you
        | _ -> failwith "should have two moves"
    )
    |> List.map ~f:score
    |> List.fold_left ~init:0 ~f:(+)
    |> Printf.printf "%d\n"
