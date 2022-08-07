type coord = {x: int; y: int};;
type value = int;;
type grid = value list list;;

(* Create a new grid of size n * n *)

let new_grid n =
    let rec aux1 acc = function
        | 0,a -> acc
        | n,a ->
                let rec aux2 acc = function
                    | 0 -> acc
                    | x -> aux2 (0::acc) (x-1)
                in aux1 ((aux2 [] a)::acc) (n-1,a)
    in aux1 [] (n,n);;

(* Handling out of grid errors *)

exception Out_of_bounds;;

(* Read value in grid *)

let get_value c g =
    let rec aux1 = function
        | (r::rg, x) when x < c.x -> aux1 (rg, x+1)
        | (r::rg, x) when x = c.x ->
                let rec aux2 = function
                    | (v::rr, y) when y < c.y -> aux2 (r, y+1)
                    | (v::rr, y) when y = c.y -> v
                    | _ -> raise Out_of_bounds
                in aux2 (r, 0)
        | _ -> raise Out_of_bounds
    in aux1 (g, 0);;

(* Set value in grid *)

let set_value g c v =
    let rec aux1 = function
        | (r::rg, x) when x < c.x -> r::(aux1 (rg, x+1))
        | (r::rg, x) when x = c.x ->
                let rec aux2 = function
                    | (cell::rr, y) when y < c.y -> cell::(aux2 (rr, y+1))
                    | (cell::rr, y) when y = c.y -> v::rr
                    | _ -> raise Out_of_bounds
                in (aux2 (r, 0))::rg
        | _ -> raise Out_of_bounds
    in aux1 (g, 0);;

(* Get empty grid cells *)

let empty_values g =
    let rec aux1 acc = function
        | ([], _) -> acc
        | (r::rg, x) ->
                let rec aux2 acc2 = function
                    | ([], _) -> acc2
                    | (cell::rr, y) when cell = 0 -> aux2 ({x=x;y=y}::acc2) (rr, y+1)
                    | (_::rr, y) -> aux2 acc2 (rr, y+1)
                in aux1 (aux2 acc (r, 0)) (rg, x+1)
    in aux1 [] (g, 0);;

(* Aux for possible values *)

let build_vlist g =
    let s = List.length g
    in let rec aux acc = function
        | 1 -> 1::acc
        | n -> aux (n::acc) (n-1)
    in aux [] s;;

let sq g c =
    let rt = int_of_float (sqrt (float (List.length g)))
    in {x = c.x / rt; y = c.y / rt};;

let sq_vals g c =
    let rec aux1 acc = function
        | ([], _) -> acc
        | (r::rg, x) ->
                let rec aux2 acc2 = function
                    | ([], _) -> acc2
                    | (v::rr, y) when (sq g c) = (sq g {x = x; y = y}) -> aux2 (v::acc2) (rr, y+1)
                    | (_::rr, y) -> aux2 acc2 (rr, y+1)
                in aux1 (aux2 acc (r,0)) (rg, x+1)
        in aux1 [] (g, 0);;

let h_vals g c =
    let rec aux = function
        | ([], _) -> raise Out_of_bounds
        | (r::rg, x) when x = c.x -> r
        | (_::rg, x) -> aux (rg, x+1)
    in aux (g, 0);;

let v_vals g c =
    let rec aux1 acc = function
        | [] -> acc
        | r::rg ->
                let rec aux2 acc2 = function
                    | ([], _) -> acc2
                    | (v::rr, y) when y = c.y -> aux2 (v::acc2) (rr, y+1)
                    | (_::rr, y) -> aux2 acc2 (rr, y+1)
                in aux1 (aux2 acc (r,0)) rg
        in aux1 [] g;;

let blocked g c =
    List.sort_uniq compare (List.concat [(v_vals g c); (List.concat [(h_vals g c); (sq_vals g c)])]);;

(* End of aux functions *)

(* Search possible values at given coordinates *)

let possible_values g c =
    let gv = build_vlist g
    and bv = blocked g c
    in List.filter (fun v -> not (List.mem v bv)) gv;;

(* Create a list of the valid grids for next step *)

let fill_at c g =
    let pv = possible_values g c
    in let rec aux acc = function
        | [] -> acc
        | (v::r) -> aux ((set_value g c v)::acc) r
    in aux [] pv;;

(* Search all possible solution for the given grid *)

let solve g =
    let rec aux acc = function
        | [] -> List.concat acc
        | c::r -> aux (List.concat (List.rev_map (fun g -> fill_at c g) acc)) r
    in aux [g] (empty_values g);;

(* Testing number of solutions *)

let nb_res r = List.length(r);;

(* Input / Output *)
(*
let load_grid () =
    let s = open_in "g1.grid" in
        try
            let grid = input_value s
            in close_in s; grid
        with | e -> close_in_noerr s; raise e ;;

let save_random_couple_char ()=
    let c = open_out "test.couple" in
    try (output_value c (random_chars' ()) ; close_out c) with
        | e -> close_out_noerr c; raise e ;;


let load_grid fname =
    let ic = open_in fname in
    try (let
*)

(* Sauvegarde du resultat dans u fichier fname *)

let save_grid fname g =
    let oc = open_out fname in
    try (let rec aux1 acc1 = function
        | [] -> output_char oc '\n'
        | rg when acc1 = int_of_float ((sqrt (float(List.length g)))) -> aux1 0 rg
        | r::rg ->
                let rec aux2 acc2 = function
                    | [] -> output_char oc '\n'; aux1 (acc1+1) rg
                    | rr when acc2 = int_of_float ((sqrt (float(List.length g)))) -> aux2 0 rr
                    | v::rr -> Printf.fprintf oc "%d " v; aux2 (acc2+1) rr
                in aux2 0 r
    in aux1 0 g; close_out oc) with
        | e -> close_out_noerr oc; raise e;;

(* Affichage d'une grille dans le top level *)

let print_grid g =
    let rec aux1 acc1 = function
        | [] -> print_char '\n'
        | rg when acc1 = int_of_float ((sqrt (float(List.length g)))) -> print_char '\n'; aux1 0 rg
        | r::rg ->
                let rec aux2 acc2 = function
                    | [] -> print_char '\n'; aux1 (acc1+1) rg
                    | rr when acc2 = int_of_float ((sqrt (float(List.length g)))) -> print_string "  "; aux2 0 rr
                    | v::rr -> print_int v; print_char ' '; aux2 (acc2+1) rr
                in aux2 0 r
    in aux1 0 g;;

(* User interaction *)

(* exception Wrong_input;; *)
(*
let start_file =
    print "Enter file name or absolute path:\n";
    let fname = input_line stdin
    in
*)

(* let start_new = *)
(*     print_string "Enter size of grid"; *)
(*     let n = input_value stdin *)
(*     in let s = solve (new_grid n) *)
(*     in print_grid s;; *)
(**)
(* let start = *)
(*     let c1 = input_value stdin *)
(*     in match c1 with *)
(*         (* | 0 -> print "not available"; start (*start_file*) *) *)
(*         | 1 -> start_new *)
(*         | _ -> raise Wrong_input;; *)

(* Tests *)

let def_4grid =
    [
        [0;0; 0;0];
        [0;0; 0;0];

        [0;0; 0;0];
        [0;0; 0;0];
    ];;

let def_9grid =
    [
        [0;0;0; 0;0;0; 0;0;0];
        [0;0;0; 0;0;0; 0;0;0];
        [0;0;0; 0;0;0; 0;0;0];

        [0;0;0; 0;0;0; 0;0;0];
        [0;0;0; 0;0;0; 0;0;0];
        [0;0;0; 0;0;0; 0;0;0];

        [0;0;0; 0;0;0; 0;0;0];
        [0;0;0; 0;0;0; 0;0;0];
        [0;0;0; 0;0;0; 0;0;0];
    ];;

let grid1 =
    [
        [3;0;6; 5;0;8; 4;0;0];
        [5;2;0; 0;0;0; 0;0;0];
        [0;8;7; 0;0;0; 0;3;1];

        [0;0;3; 0;1;0; 0;8;0];
        [9;0;0; 8;6;3; 0;0;5];
        [0;5;0; 0;9;0; 6;0;0];

        [1;3;0; 0;0;0; 2;5;0];
        [0;0;0; 0;0;0; 0;7;4];
        [0;0;5; 2;0;6; 3;0;0];
    ];;

let grid2 =
    [
        [0;3; 2;0];
        [4;0; 0;3];

        [3;0; 0;2];
        [2;0; 3;0];
    ];;


let s1 = solve grid1;;
(* let s2 = solve grid2;; *)

s1;;

save_grid "r1" s1;;

