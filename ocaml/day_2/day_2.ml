let file = "day_2.txt"

let char_in_string_times c str =
  let seq = String.to_seq str in
  let matched_seq = Seq.filter (fun item -> item = c) seq in
  Seq.fold_left (fun a _ -> 1 + a) 0 matched_seq;;

let check_password (policy, password) =
  match String.split_on_char ' ' policy with
  | [count; char_to_check] ->
    (match String.split_on_char '-' count with
    | [min; max] ->
      let actual_times = char_in_string_times (String.get char_to_check 0) password in
      (int_of_string min <= actual_times) && (actual_times <= int_of_string max)
      ;
    | _ -> raise (Failure "something went wrong"))
  | _ -> raise (Failure "something went wrong");;


let check_password_sled_rental (policy, password) =
  (* let pw_length = String.length password in *)

  match String.split_on_char ' ' policy with
  | [count; char_to_check] ->
    (match String.split_on_char '-' count with
      | [should_be_at_idx; should_be_at_idx_2] ->
        let really_a_char = String.get char_to_check 0 in
        let at_first_idx = (String.get password ((int_of_string should_be_at_idx) - 1) = really_a_char) in
        let at_second_idx = (String.get password ((int_of_string should_be_at_idx_2) - 1) = really_a_char) in
        ((at_first_idx || at_second_idx) && (at_first_idx != at_second_idx))
        ;
      | _ -> raise (Failure "something went wrong"))
  | _ -> raise (Failure "something went wrong");;


let () =
  let ic = open_in file in
  let lines = ref [] in

  try
    while true do
      let line = input_line ic in
      let args = String.split_on_char ':' line in
      let policy = List.nth args 0 in
      let password = String.trim (List.nth args 1) in

      lines := !lines @ [(policy, password)];
    done;
  with _e -> ();

  close_in_noerr ic;

  let valid_items = List.filter check_password !lines in
  Printf.printf "Valid items: %d\n" (List.length valid_items);

  let valid_items_sled_rental = List.filter check_password_sled_rental !lines in
  Printf.printf "Valid items: %d\n" (List.length valid_items_sled_rental);

