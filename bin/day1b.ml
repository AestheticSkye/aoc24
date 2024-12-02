(** Function to split a string by both spaces and newlines *)
let split s =
  let regex = Str.regexp "[ \n]+" in
  Str.split regex s

(** Function to read in the text from the file and split it *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  List.map String.trim (split contents)

let lines = read_lines "data/day1.txt"

(** Get only elements from the left list, conver to Int and sort *)
let left_list =
  List.filteri (fun index _ -> Int.rem index 2 == 1) lines
  |> List.map int_of_string |> List.sort compare

(** Same but for the right list *)
let right_list =
  List.filteri (fun index _ -> Int.rem index 2 == 0) lines
  |> List.map int_of_string |> List.sort compare

(** Function to calculate how many instances of value exist in right_list, and times value by the result *)
let calculate_similarity value =
  List.filter (fun v -> Int.equal value v) right_list
  |> List.length |> Int.mul value

(** Calculate the similarity for the values in left_list and add them *)
let similarity =
  List.map calculate_similarity left_list |> List.fold_left Int.add 0

let () = print_int similarity
