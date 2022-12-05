open! Core

let split =
    let rec loop acc = function
        | [] -> List.rev acc, []
        | hd :: rest when String.is_empty hd -> 
                acc
                |> List.tl_exn
                |> List.rev,
                rest
        | hd :: rest -> loop (hd :: acc) rest
    in
    loop []

let _print_stacks stacks =
    Array.iteri ~f:(fun i c ->
        Printf.printf "Stack %d -- " i;
        List.iter ~f:(fun c -> Printf.printf "%c " c) c;
        print_endline "";
    ) stacks;
;;

let parse_crates crates =
    let count = String.length (List.hd_exn crates) / 4 + 1 in
    let stacks = Array.create ~len:count [] in

    List.iter crates ~f:(fun c ->
        print_endline c;
        for i=0 to count-1 do
            let ch = String.get c (i*4+1) in
            if Char.(ch <> ' ') then (
                stacks.(i) <- ch :: stacks.(i)
            )
        done
    );

    Array.iteri ~f:(fun i s -> stacks.(i) <- List.rev s) stacks;

    stacks
;;

let do_move stacks (count, from, to_) =
    for _=0 to count-1 do
        match stacks.(from), stacks.(to_) with
        | hf::tf, ts ->
            stacks.(from) <- tf;
            stacks.(to_) <- hf :: ts
        | _ -> failwith "bad stack"
    done;
;;

let parse_moves =
    List.map ~f:(fun s ->
        let m = String.split ~on:' ' s in
        let count = Int.of_string (List.nth_exn m 1) in
        let from = Int.of_string (List.nth_exn m 3) - 1 in
        let to_ = Int.of_string (List.nth_exn m 5) - 1 in
        count, from, to_
    )

let _ =
    let crates, moves =
        "./05-1/input.txt"
        |> In_channel.read_lines
        |> split
    in
    let stacks = parse_crates crates in
    let moves = parse_moves moves in
    List.iter ~f:(do_move stacks) moves;
    Array.iter ~f:(fun l -> 
        Printf.printf "%c" (List.hd_exn l)
    ) stacks;
    print_endline "";


