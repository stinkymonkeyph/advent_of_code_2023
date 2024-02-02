let get_bucket_value color line =
  let parsed_list =
    String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
  in
  let value =
    match parsed_list with
    | h :: s :: _ -> if s = color then int_of_string h else 0
    | _ -> 0
  in
  value

let break_line_to_sets line =
  let parsed_string =
    String.split_on_char ';' line |> List.filter (fun s -> s <> "")
  in
  parsed_string

let sum acc x = acc + x

let process_line_set line red green blue =
  let parsed_string =
    String.split_on_char ',' line |> List.filter (fun s -> s <> "")
  in
  let red_value = List.map (fun a -> get_bucket_value "red" a) parsed_string in
  let green_value =
    List.map (fun a -> get_bucket_value "green" a) parsed_string
  in
  let blue_value =
    List.map (fun a -> get_bucket_value "blue" a) parsed_string
  in
  let red_value = List.fold_left (fun acc x -> sum acc x) 0 red_value in
  let green_value = List.fold_left (fun acc x -> sum acc x) 0 green_value in
  let blue_value = List.fold_left (fun acc x -> sum acc x) 0 blue_value in
  if red >= red_value && green >= green_value && blue >= blue_value then true
  else false

let check_posibility line red green blue =
  let parsed_string = String.split_on_char ':' line in
  let parsed_string = List.nth parsed_string 1 in
  let parsed_string = break_line_to_sets parsed_string in
  let parsed_string =
    List.map (fun a -> process_line_set a red green blue) parsed_string
  in
  List.for_all (fun a -> a = true) parsed_string

let process_line line red green blue : int =
  let parsed_string = String.split_on_char ':' line in
  let game_id = List.hd parsed_string in
  let game_id = String.split_on_char ' ' game_id in
  let game_id = int_of_string @@ List.nth game_id 1 in
  let is_possible = check_posibility line red green blue in
  if is_possible then game_id else 0

let sum_of_possible_ids ~filename ~red ~green ~blue : int =
  try
    let channel = open_in filename in
    let sum_of_ids = ref 0 in
    try
      while true do
        try
          let line = input_line channel in
          sum_of_ids := !sum_of_ids + process_line line red green blue
        with End_of_file -> raise Exit
      done;
      !sum_of_ids
    with Exit ->
      close_in channel;
      !sum_of_ids
  with Sys_error msg ->
    Printf.printf "Error: %s \n" msg;
    0

let () =
  let sum_of_ids =
    sum_of_possible_ids
      ~filename:
        "/Users/nelminjayanoc/Documents/Development/Laboratory/Ocaml/AdventOfCode2023/day2_code_conundrum/bin/puzzle_input.txt"
      ~red:12 ~green:13 ~blue:14
  in
  Printf.printf "Sum possible game ids: %i \n" sum_of_ids
