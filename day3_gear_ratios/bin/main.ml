let create_matrix row cols = Array.make_matrix row cols ""

let print_matrix matrix =
  for i = 0 to Array.length matrix - 1 do
    for j = 0 to Array.length matrix.(i) - 1 do
      print_string matrix.(i).(j);
      print_string "\t"
    done;
    print_endline "\n"
  done

let add_matrix matrix x y value = matrix.(x).(y) <- value
let array_exists array value = Array.exists (fun e -> e = value) array

let has_adjacent_symbol condition symbols matrix x y =
  if condition == true then
    if array_exists symbols matrix.(x).(y) then true else false
  else false

let convert_to_array str =
  Array.init (String.length str) (fun i -> String.make 1 str.[i])

let is_digit char = match char with '0' .. '9' -> true | _ -> false
let sub x y = x - y
let add x y = x + y

let check_for_symbols matrix y x =
  let possible_symbols = convert_to_array "!\"#$%&'()*+,-/:;<=>?@[\\]^_`{|}~" in
  let has_left_symbol =
    has_adjacent_symbol (x != 0) possible_symbols matrix y (x - 1)
  in
  let has_right_symbol =
    has_adjacent_symbol
      (x != Array.length matrix.(0) - 1)
      possible_symbols matrix y (x + 1)
  in
  let has_down_symbol =
    has_adjacent_symbol
      (y != Array.length matrix - 1)
      possible_symbols matrix (y + 1) x
  in
  let has_up_symbol =
    has_adjacent_symbol (y != 0) possible_symbols matrix (y - 1) x
  in
  let has_diag_down_right =
    has_adjacent_symbol
      (y + 1 <= Array.length matrix - 1 && x + 1 <= Array.length matrix.(0) - 1)
      possible_symbols matrix (y + 1) (x + 1)
  in
  let has_diag_up_right =
    has_adjacent_symbol
      (y - 1 >= 0 && x + 1 <= Array.length matrix.(0) - 1)
      possible_symbols matrix (y - 1) (x + 1)
  in
  let has_diag_down_left =
    has_adjacent_symbol
      (y + 1 <= Array.length matrix - 1 && x - 1 >= 0)
      possible_symbols matrix (y + 1) (x - 1)
  in
  let has_diag_up_left =
    has_adjacent_symbol
      (y - 1 >= 0 && x - 1 >= 0)
      possible_symbols matrix (y - 1) (x - 1)
  in
  has_left_symbol || has_right_symbol || has_up_symbol || has_down_symbol
  || has_diag_up_left || has_diag_down_left || has_diag_up_right
  || has_diag_down_right

let rec sum_list lst =
  match lst with [] -> 0 | head :: tail -> head + sum_list tail

let get_full_part_number has_adjacent_symbol part_number matrix y x previous_x
    (prefix : bool) =
  let previous_val =
    if previous_x >= 0 && previous_x <= Array.length !matrix.(y) - 1 then
      !matrix.(y).(previous_x)
    else "-1"
  in
  if
    has_adjacent_symbol && part_number != "-1" && x >= 0
    && x <= Array.length !matrix.(y) - 1
  then
    if is_digit !matrix.(y).(x).[0] && is_digit previous_val.[0] then
      let matrix_value = !matrix.(y).(x) in
      let part_number =
        if prefix == false then part_number ^ matrix_value
        else matrix_value ^ part_number
      in
      (part_number, true, x)
    else (part_number, false, 0)
  else (part_number, false, 0)

let read_schematics matrix =
  print_string "\nloaded matrix: \n";
  print_matrix !matrix;
  let part_numbers = ref [] in

  for i = 0 to Array.length !matrix - 1 do
    let partial_list = ref [] in
    let skip_indices = ref [] in
    for j = 0 to Array.length !matrix.(i) - 1 do
      let part_number =
        if is_digit !matrix.(i).(j).[0] then !matrix.(i).(j) else "-1"
      in
      let has_adjacent_symbol =
        if part_number != "-1" then check_for_symbols !matrix i j else false
      in

      let part_number, skip_next_index, index_to_skip =
        get_full_part_number has_adjacent_symbol part_number matrix i (add j 1)
          j false
      in
      skip_indices :=
        if skip_next_index then index_to_skip :: !skip_indices
        else !skip_indices;

      let part_number, skip_next_index, index_to_skip =
        get_full_part_number has_adjacent_symbol part_number matrix i (add j 2)
          (add j 1) false
      in
      skip_indices :=
        if skip_next_index then index_to_skip :: !skip_indices
        else !skip_indices;

      let part_number, skip_next_index, index_to_skip =
        get_full_part_number has_adjacent_symbol part_number matrix i (sub j 1)
          j true
      in
      skip_indices :=
        if skip_next_index then index_to_skip :: !skip_indices
        else !skip_indices;

      let part_number, skip_next_index, index_to_skip =
        get_full_part_number has_adjacent_symbol part_number matrix i (sub j 2)
          (sub j 1) true
      in
      skip_indices :=
        if skip_next_index then index_to_skip :: !skip_indices
        else !skip_indices;

      partial_list :=
        if
          List.mem j !skip_indices == false
          && part_number != "-1" && has_adjacent_symbol
        then int_of_string part_number :: !partial_list
        else !partial_list;

      part_numbers :=
        if j == Array.length !matrix.(i) - 1 then !partial_list @ !part_numbers
        else !part_numbers
    done;
    skip_indices := [];
    partial_list := []
  done;
  print_string "Part Numbers: \n";
  List.iter (fun a -> Printf.printf "%n \n" a) !part_numbers;
  let sum = sum_list !part_numbers in
  sum

let load_schematics file matrix =
  try
    let channel = open_in file in
    let line_number = ref 0 in
    try
      while true do
        try
          let line = input_line channel in
          let array_of_strings = convert_to_array line in
          for
            index = 0
            to min
                 (Array.length array_of_strings - 1)
                 (Array.length matrix.(!line_number) - 1)
          do
            add_matrix matrix !line_number index array_of_strings.(index)
          done;
          line_number := !line_number + 1
        with End_of_file -> raise Exit
      done;
      close_in channel;
      matrix
    with Exit ->
      close_in channel;
      matrix
  with Sys_error msg ->
    Printf.printf "Error: %s \n" msg;
    matrix

let () =
  let matrix = create_matrix 140 140 in
  let sum_of_engine_parts =
    load_schematics
      "/Users/nelminjayanoc/Documents/Development/Laboratory/advent_of_code_2023/day3_gear_ratios/bin/puzzle_input.txt"
      matrix
  in
  let sum_of_engine_parts = ref sum_of_engine_parts in
  let sum_of_parts = read_schematics sum_of_engine_parts in
  Printf.printf "Sum of engine parts: %d \n" sum_of_parts
