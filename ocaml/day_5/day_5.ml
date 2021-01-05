open Core

let file = "input.txt"

let char_to_bit char =
  match char with
    | 'F' | 'L' -> 0
    | 'B' | 'R' -> 1
    | _ -> raise (Failure (Printf.sprintf "Invalid character: %c" char))

let fold_array_to_id idx acc item =
  acc + (Int.shift_left item idx)
;;

let parse_line line =
  let a = List.rev (List.map (String.to_list line) ~f:char_to_bit) in
  List.foldi a ~f:fold_array_to_id ~init:0
;;

let rec print_list_string myList = match myList with
  | [] -> ()
  | head::body ->
    print_endline (Printf.sprintf "%d" head);
    print_list_string body
;;

let find_empty_seats list =
  let range = List.range 21 996 in
  List.filter range ~f:(fun a -> not (List.mem list a ~equal:(fun a b -> phys_equal a b)))
;;

let () =
  let ic = In_channel.create file in
  let lines = In_channel.input_lines ic in
  In_channel.close ic;

  let parsed_lines = List.map ~f:parse_line lines in
  let sorted_list = List.sort parsed_lines ~compare:(fun a b -> (b - a)) in

  Printf.printf "Highest ID: %d\n" (List.nth_exn sorted_list 0);

  print_endline "\nEmpty seats:";
  print_list_string (find_empty_seats sorted_list)
;;
