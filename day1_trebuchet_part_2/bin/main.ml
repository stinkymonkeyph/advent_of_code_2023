let is_digit char = match char with '0' .. '9' -> true | _ -> false

let number_table =
  let table = Hashtbl.create 2 in

  Hashtbl.add table "one" 1;
  Hashtbl.add table "two" 2;
  Hashtbl.add table "three" 3;
  Hashtbl.add table "four" 4;
  Hashtbl.add table "five" 5;
  Hashtbl.add table "six" 6;
  Hashtbl.add table "seven" 7;
  Hashtbl.add table "eight" 8;
  Hashtbl.add table "nine" 9;

  table

let replace key value line =
  let result =
    Str.global_replace (Str.regexp_string key)
      (key ^ string_of_int value ^ key)
      line
  in
  result

let translate_table line table =
  let modified_line = ref line in
  Hashtbl.iter (fun k v -> modified_line := replace k v !modified_line) table;
  !modified_line

let parse_extract line =
  try
    let parsed_string = translate_table line number_table in
    let parsed_string =
      String.map (fun c -> if is_digit c then c else '0') parsed_string
    in
    let parsed_string =
      String.fold_left
        (fun acc c -> if c = '0' then acc else acc ^ String.make 1 c)
        "" parsed_string
    in
    let parsed_string =
      if String.length parsed_string = 1 then
        let result = parsed_string ^ parsed_string in
        result
      else parsed_string
    in
    let len = String.length parsed_string in
    let first_char = String.get parsed_string 0 in
    let second_char = String.get parsed_string (len - 1) in
    let calibration_value =
      String.make 1 first_char ^ String.make 1 second_char
    in
    let calibration_value = int_of_string calibration_value in
    calibration_value
  with Failure msg ->
    Printf.printf "Error: %s \n" msg;
    0

let read_puzzle_calibration filename : int =
  try
    let channel = open_in filename in
    let sum = ref 0 in
    try
      while true do
        try
          let line = input_line channel in
          let value = parse_extract line in
          sum := !sum + value
        with End_of_file -> raise Exit
      done;
      !sum
    with Exit ->
      close_in channel;
      !sum
  with Sys_error msg ->
    Printf.printf "Error: %s \n" msg;
    0

let () =
  let calibration =
    read_puzzle_calibration
      "/Users/nelminjayanoc/Documents/Development/Laboratory/advent_of_code_2023/day1_trebuchet_part_2/bin/puzzle_input.txt"
  in
  Printf.printf "\nCalibration value: %d \n" calibration
