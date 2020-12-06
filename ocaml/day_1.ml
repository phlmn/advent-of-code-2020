let file = "day_1.txt"

let prod l r =
  let g acc a =
    let f acc x = (a, x) :: acc in
    List.fold_left f acc r in
  List.fold_left g [] l |> List.rev

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

  let prod1 = prod !lines !lines in

  let find_sum2 p =
    match List.find_opt (fun (a, b) -> (a + b) = 2020) p with
    | Some (a, b) -> Printf.printf "%d * %d = %d\n" a b (a * b)
    | None -> () in

  find_sum2 prod1;

  let prod2 = prod prod1 !lines in

  let find_sum3 p =
    match List.find_opt (fun ((a, b), c) -> (a + b + c) = 2020) p with
    | Some ((a, b), c) -> Printf.printf "%d * %d * %d = %d\n" a b c (a * b * c)
    | None -> () in

  find_sum3 prod2;
