open! Core

let count_dir arr seen sx sy dx dy =
    let x = ref sx in
    let y = ref sy in
    let len_x = Array.length arr.(0) in
    let len_y = Array.length arr in
    let count = ref 0 in
    let tallest = ref ~-1 in
    while !x >= 0 && !y >= 0 && !x < len_x && !y < len_y do
        let this = arr.(!x).(!y) in
        if this > !tallest then (
            if not seen.(!x).(!y) then (
                incr count;
                seen.(!x).(!y) <- true;
            );
            tallest := this;
        );
        x := !x + dx;
        y := !y + dy;
    done;
    !count
;;

let _count_visible arr =
    let len_x = Array.length arr.(0) in
    let len_y = Array.length arr in
    let total = ref 0 in

    let seen = Array.make_matrix ~dimx:len_x ~dimy:len_y false in

    (* top *) 
    (* bot *)
    for x=0 to len_x-1 do
        total := !total + count_dir arr seen x 0 0 1;
        total := !total + count_dir arr seen x (len_y-1) 0 ~-1
    done;

    (* left *)
    for y=0 to len_y-1 do
        total := !total + count_dir arr seen 0 y 1 0;
        total := !total + count_dir arr seen (len_x-1) y ~-1 0
    done;

    !total
;;

let count_scenic arr =
    let len_x = Array.length arr.(0) in
    let len_y = Array.length arr in
    let total = ref 0 in

    for x=1 to len_x-2 do
        for y=1 to len_y-2 do
            incr total
        done
    done;

    !total
;;

let _ =
    "./08-2/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:(fun s ->
        s
        |> String.to_list
        |> List.map ~f:(fun c -> Char.to_int c - Char.to_int '0')
        |> Array.of_list
    )
    |> Array.of_list
    |> count_scenic
    |> Printf.printf "%d\n%! "

