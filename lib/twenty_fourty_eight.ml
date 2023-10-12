type direction = Up | Down | Left | Right
type axis = Horizontal | Vertical
type location = { x: int; y: int }
type cell = { value: int; location: location }
type game = { size: int; cells: cell list }

let dir_to_axis dir =
    match dir with
    | Left | Right -> Horizontal
    | Up   | Down  -> Vertical

let inverse axis =
    match axis with
    | Horizontal -> Vertical 
    | Vertical   -> Horizontal

let get_idx size loc = loc.x + size * loc.y

let loc_to_str size loc = Printf.sprintf "%d" @@ get_idx size loc
let cell_to_str size cell = Printf.sprintf "%d,%s" cell.value @@ loc_to_str size cell.location
let game_to_str game = 
    let rec join sep = function 
        | [] -> ""
        | [str] -> str
        | ""::strs -> join sep strs 
        | str::strs -> str ^ sep ^ join sep strs in
    Printf.sprintf "%d|%s" game.size 
    @@ List.fold_left 
        (fun acc cell -> join ";" [acc ;cell_to_str game.size cell]) 
        "" 
        game.cells

let idx_to_loc size idx =
    { x = idx mod size 
    ; y = idx / size }

let parse msg =
    let strs = List.tl @@ String.split_on_char '|' msg in
    let size = strs |> List.hd |> int_of_string in 
    let cells = List.nth strs 1 in
    let cells = String.split_on_char ';' cells in
    let parse_cell cell =
        let cell' = String.split_on_char ',' cell in 
        let value = List.hd cell' in
        let idx = List.nth cell' 1 in
        { value = int_of_string value 
        ; location = idx |> int_of_string |> idx_to_loc size } in
    let cells = List.map parse_cell cells in
    { size = size 
    ; cells = cells }
     

let get_i_by_axis axis loc =
    match axis with 
    | Horizontal -> loc.x
    | Vertical   -> loc.y

let get_empty_cells game = 
    let open List in 
    let get_idx cell = get_idx game.size cell.location in
    let cells = map get_idx game.cells in 
    let cell_list = init (game.size * game.size) (fun i -> i) in 
    let filter_cells i =
        match find_opt (fun idx -> idx == i) cells with
        | Some _ -> false 
        | None   -> true in
    let empty_cells = filter filter_cells cell_list in
    Array.of_list empty_cells

let generate_cell game =
    let open Array in
    let open Random in 
    let empty_cells = get_empty_cells game in
    let idx () = 
        empty_cells 
        |> length 
        |> int 
        |> (get empty_cells) in
    let cell () =
        { value = if int 16 == 0 then 2 else 1
        ; location = idx_to_loc game.size @@ idx () } in
    let cells = 
        if length empty_cells == 0
        then game.cells
        else 
            let get_idx = get_idx game.size in
            let sort_by_idx a b = get_idx a.location - get_idx b.location in 
            game.cells @ [cell()]
        |> List.sort sort_by_idx in
    { size = game.size 
    ; cells = cells }

let new_game size =
    let empty_game =
        { size = size 
        ; cells = [] } in
    generate_cell empty_game

let move dir game =
    let open Array in
    let empty_lines = make game.size [] in
    let axis = dir_to_axis dir in
    let update_line line cell =
        let i = get_i_by_axis axis cell.location in
        let () = line.(i) <- line.(i) @ [cell] in 
        line in
    let get_j cell = get_i_by_axis (inverse axis) cell.location in
    let sort_by_j a b = get_j a - get_j b in
    let fold_cells acc cell = acc @ [cell] in
    let fold_lines acc line = acc @ List.fold_left fold_cells [] line in
    let cells' = 
        List.fold_left update_line empty_lines game.cells 
        |> map (List.sort sort_by_j) 
        |> fold_left fold_lines [] in
    let game' =
        { size = game.size 
        ; cells = cells' } in
    generate_cell game'

let handle_dir dir =
    match dir with
    | 'h' -> Left
    | 'j' -> Down 
    | 'k' -> Up 
    | 'l' -> Right 
    | _   -> Up

let handle_msg msg = 
    match msg with 
    | "init" -> game_to_str @@ new_game 4
    | msg -> msg 
        |> parse 
        |> move (handle_dir @@ String.get msg 0)
        |> game_to_str 
