(* ==================== Чтение лабиринта ==================== *)

let read_maze filename =
  let chan = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line chan in
      read_lines (line :: acc)
    with End_of_file ->
      close_in chan;
      List.rev acc
  in
  read_lines []


(* ==================== Преобразование в массив ==================== *)

let maze_to_array lines =
  Array.of_list (List.map (fun s -> Array.of_seq (String.to_seq s)) lines)


(* ==================== Поиск символа S или E ==================== *)

let find_char maze ch =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  let rec search i j =
    if i = h then None
    else if j = w then search (i+1) 0
    else if maze.(i).(j) = ch then Some (i, j)
    else search i (j+1)
  in
  search 0 0


(* ==================== DFS: сохраняем только координаты ==================== *)

let solve_maze maze =
  let h = Array.length maze in
  let w = Array.length maze.(0) in

  let visited = Array.make_matrix h w false in
  let path = ref [] in

  let dirs = [ (1,0); (-1,0); (0,1); (0,-1) ] in

  let rec dfs (x,y) =
    if maze.(x).(y) = 'E' then (
      path := (x,y) :: !path;
      true
    ) else (
      visited.(x).(y) <- true;

      let rec try_dirs = function
        | [] -> false
        | (dx,dy) :: rest ->
            let nx = x + dx in
            let ny = y + dy in
            if nx >= 0 && nx < h && ny >= 0 && ny < w &&
               not visited.(nx).(ny) &&
               maze.(nx).(ny) <> '#'
            then
              if dfs (nx,ny) then (
                path := (x,y) :: !path;
                true
              ) else
                try_dirs rest
            else try_dirs rest
      in
      try_dirs dirs
    )
  in

  match find_char maze 'S' with
  | None -> []
  | Some start ->
      if dfs start then List.rev !path else []


(* ==================== Помечаем путь ==================== *)

let mark_path maze path =
  let maze_copy = Array.map Array.copy maze in
  List.iter (fun (x,y) ->
    if maze_copy.(x).(y) = '.' then
      maze_copy.(x).(y) <- '*'
  ) path;
  maze_copy


let print_maze maze =
  Array.iter (fun row ->
    Array.iter print_char row;
    print_newline ()
  ) maze


(* ========================== MAIN ========================== *)

let () =
  let lines = read_maze "/uploads/input.txt" in
  let maze = maze_to_array lines in

  let path = solve_maze maze in

  if path = [] then
    print_endline "No path found"
  else (
    print_endline "Path found:\n";

    let marked = mark_path maze path in
    print_maze marked
  )
