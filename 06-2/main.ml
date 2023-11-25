open! Core

exception Found of int

let find_unique s : int =
    try
        for i=13 to String.length s - 1 do
            let sub = String.sub ~pos:(i-13) ~len:14 s in
            let set = Char.Set.of_list (String.to_list sub) in
            if Char.Set.length set = 14 then
                raise (Found i)
        done;
        0
    with Found i -> i+1
;;

let _ =
    "./06-2/input.txt"
    |> In_channel.read_lines
    |> List.hd_exn
    |> find_unique
    |> Printf.printf "%d\n%!"
