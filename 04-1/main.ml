open! Core

let to_interval line =
    match String.split ~on:'-' line with
    | [left; right] -> Int.of_string left, Int.of_string right
    | _ -> failwith "Bad interval"

let parse line =
    match String.split ~on:',' line with
    | [left; right] -> to_interval left, to_interval right
    | _ -> failwith "Bad line"

let overlaps ((a1, b1), (a2, b2)) =
    a1 <= a2 && b1 >= b2
    || a2 <= a1 && b2 >= b1

let _ =
    "./04-1/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:parse
    |> List.filter ~f:overlaps
    |> List.length
    |> Printf.printf "%d\n%!"
