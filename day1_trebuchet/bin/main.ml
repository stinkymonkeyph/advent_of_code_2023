let is_digit char = match char with '0' .. '9' -> true | _ -> false

let parse_extract line =
  try
    let parsed_string =
      String.map (fun c -> if is_digit c then c else '0') line
    in
    let parsed_string =
      String.fold_left
        (fun acc c -> if c = '0' then acc else acc ^ String.make 1 c)
        "" parsed_string
    in
    let parsed_string = int_of_string parsed_string in
    let parsed_string =
      if String.length (string_of_int parsed_string) = 1 then
        let result =
          int_of_string
            (string_of_int parsed_string ^ string_of_int parsed_string)
        in
        result
      else parsed_string
    in
    let len = String.length @@ string_of_int parsed_string in
    let parsed_string = string_of_int parsed_string in
    let first_char = String.get parsed_string 0 in
    let second_char = String.get parsed_string (len - 1) in
    let calibration_value =
      String.make 1 first_char ^ String.make 1 second_char
    in
    int_of_string calibration_value
  with Failure _ -> 0

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
      "/Users/nelminjayanoc/Documents/Development/Laboratory/Ocaml/AdventOfCode2023/day1_trebuchet/bin/puzzle_input.txt"
  in
  Printf.printf "\nCalibration is: %i" calibration
