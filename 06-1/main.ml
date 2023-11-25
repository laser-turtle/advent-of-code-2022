open! Core

exception Found of int

let find_unique s : int =
    try
        for i=3 to String.length s - 1 do
            let a = String.get s (i+0) in
            let b = String.get s (i-1) in
            let c = String.get s (i-2) in
            let d = String.get s (i-3) in
            let set = Char.Set.of_list [a;b;c;d] in
            if Char.Set.length set = 4 then
                raise (Found i)
        done;
        0
    with Found i -> i+1
;;

let _ =
    "./06-1/input.txt"
    |> In_channel.read_lines
    |> List.hd_exn
    |> find_unique
    |> Printf.printf "%d\n%!"
