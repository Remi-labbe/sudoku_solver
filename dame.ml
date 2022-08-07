type coord = {x: int; y: int};;
type value = E | X | O;;
type grid = value list list;;

(* Create a new grid of size n * n *)

let new_grid n =
    let rec aux1 acc = function
        | 0,a -> acc
        | n,a ->
                let rec aux2 acc = function
                    | 0 -> acc
                    | x -> aux2 (E::acc) (x-1)
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
                    | (cell::rr, y) when cell = E -> aux2 ({x=x;y=y}::acc2) (rr, y+1)
                    | (_::rr, y) -> aux2 acc2 (rr, y+1)
                in aux1 (aux2 acc (r, 0)) (rg, x+1)
    in aux1 [] (g, 0);;

(* Search possible values at given coordinates *)

let possible_values g c =
    let check x y =
        c.x = x
        || c.y = y
        || abs(c.x - x) = abs(c.y - y)
    in let rec aux1 = function
        | ([], _) -> [X; O]
        | (r::rg, x) ->
                let rec aux2 = function
                    | ([], _) -> aux1 (rg, x+1)
                    | (cell::rr, y) when (cell = X) && (check x y) -> [O]
                    | (_::rr, y) -> aux2 (rr, y+1)
                in aux2 (r, 0)
    in aux1 (g, 0);;

(* Create a list of the valid grids for next step *)

let fill_at c g =
    let pv = possible_values g c
    in let rec aux acc = function
        | [] -> acc
        | (v::r) -> aux ((set_value g c v)::acc) r
    in aux [] pv;;

(* Checks valid lists after the algorithm finished *)

let sx gl =
    let s = List.length (List.hd (List.hd gl))
    in let gfilter g =
        let rec aux1 acc = function
            | [] -> acc
            | r::rg ->
                    let rec aux2 = function
                        | [] -> aux1 acc rg
                        | v::rr when v = X -> aux1 (acc+1) rg
                        | _::rr -> aux2 rr
                    in aux2 r
        in aux1 0 g
    in List.filter (fun g -> (gfilter g) = s) gl;;

(* Search all possible solution for the given grid *)

let solve g =
    let rec aux acc = function
        | [] -> sx acc
        | c::r -> aux (List.concat (List.rev_map (fun g -> fill_at c g) acc)) r
    in aux [g] (empty_values g);;

let nb_res r = List.length(r);;
solve (new_grid 8);;
nb_res (solve (new_grid 8));;
