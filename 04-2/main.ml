open! Core

let to_interval line =
    match String.split ~on:'-' line with
    | [left; right] -> Int.of_string left, Int.of_string right
    | _ -> failwith "Bad interval"

let parse line =
    match String.split ~on:',' line with
    | [left; right] -> to_interval left, to_interval right
    | _ -> failwith "Bad line"

let overlaps ((x1, x2), (y1, y2)) =
    if x1 <= y1 then y1 <= x2 else x1 <= y2

let _ =
    "./04-2/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:parse
    |> List.filter ~f:overlaps
    |> List.length
    |> Printf.printf "%d\n%!"
