let file = "day_1.txt"

let rec check list =
  let current = List.hd list in
  let new_list = List.tl list in

  match List.find_opt (fun item -> (item + current) = 2020) new_list with
  | Some item -> Some (item, current)
  | None -> check new_list

let () =
  let ic = open_in file in
  let lines = ref [] in

  try
    while true do
      let line = input_line ic in
      lines := !lines @ [int_of_string line];
    done;
  with _e -> ();

  close_in_noerr ic;

  match check !lines with
  | None -> print_endline "No"
  | Some (a, b) -> Printf.printf "%d * %d = %d\n" a b (a * b)
