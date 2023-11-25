open! Core

type t = File of {
    name : string;
    size : int;
}
   | Dir of {
       name : string ;
       mutable files : t String.Map.t;
   }

let to_map (t : t list) = 
    List.fold ~init:String.Map.empty ~f:(fun m t ->
        let name = 
            match t with
            | File f -> f.name
            | Dir d -> d.name
        in
        String.Map.set m ~key:name ~data:t
    ) t

let dir name files = Dir {name; files=to_map files}
let file name size = File {name; size}

type path = t list * t

let start : path = [], dir "." [dir "/" []]

type cmd = Ls of t list
         | Cd of string

let up (path, d) : path =
    match path with
    | [] -> [], d
    | File _ :: _ -> failwith "impossible"
    | (Dir _ as d) :: tl -> (tl, d)
;;

let rec get_root ((path, d) as p) =
    match path with
    | [] -> d
    | _ -> get_root (up p)

let cd (path, dir) file =
    if String.(file = "..") then (
        up (path, dir)
    ) else (
        match dir with
        | Dir d as p -> 
            let new_dir = String.Map.find_exn d.files file in
            p :: path, new_dir
        | File _ -> failwith "expected dir"
    )
;;

let ls (path, dir) files =
    match dir with
    | Dir d as p ->
        let files = to_map files in
        let files = String.Map.merge files d.files 
            ~f:(fun ~key:_ res ->
                match res with
                | `Both _ -> failwith "uh oh"
                | `Left t -> Some t
                | `Right t -> Some t
            )
        in
        d.files <- files;
        path, p
    | File _ -> failwith "can't ls a file"
;;

let apply_command cmd fs =
    match cmd with
    | Cd d -> cd fs d
    | Ls files -> ls fs files
;;

let parse_command cmd =
    if String.is_prefix cmd ~prefix:" cd" then (
        Cd (String.split cmd ~on:' ' |> List.last_exn |> String.strip)
    ) else (
        let files = String.split ~on:'\n' cmd in
        let files =
            files
            |> List.tl_exn
            |> List.map ~f:String.strip
            |> List.filter ~f:(fun s -> not (String.is_empty s))
            |> List.map ~f:(String.split ~on:' ')
            |> List.map ~f:(function
                | ["dir"; name] -> dir (String.strip name) []
                | [size; name] -> file (String.strip name) (Int.of_string size)
                | l -> 
                        List.iter ~f:print_endline l;
                        failwith "bad input"
            )
        in
        Ls files
    )
;;

let pad m =
    for _=0 to m-1 do
        print_string " "
    done

let print d =
    let rec loop i = function
        | Dir d ->
            pad i; print_endline d.name;
            String.Map.iter ~f:(loop (i+2)) d.files
        | File f ->
            pad i; Printf.printf "%s %d\n" f.name f.size
    in
    loop 0 d
;;

let totals = ref 0

let rec calc_size min fs =
    match fs with
    | Dir d ->
        let my_size =
            String.Map.fold ~init:0 ~f:(fun ~key:_ ~data total ->
                match data with
                | Dir _ as c -> total + calc_size min c
                | File f -> total + f.size
            ) d.files
        in
        if my_size <= min && String.(d.name <> "/") then (
            Printf.printf "--> %s\n" d.name;
            totals := !totals + my_size
        );
        my_size
    | File f -> f.size

let _ =
    "./07-1/input.txt"
    |> In_channel.read_all
    |> String.split ~on:'$'
    |> List.fold ~init:start ~f:(fun fs cmd ->
        let cmd = parse_command cmd in
        apply_command cmd fs
    )
    |> get_root
    |> (function
        | Dir d -> String.Map.find_exn d.files "/"
        | _ -> failwith "blah"
    )
    |> (fun r -> 
        print r;
        Printf.printf "%d\n" (calc_size 100000 r);
        Printf.printf "TOTAL: %d\n %!" !totals
    )
     

