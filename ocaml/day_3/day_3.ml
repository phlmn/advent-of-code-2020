let file = "map.txt"

let rec tail2 n list =
  if n = 0 then
    list
  else
    let new_list = List.tl list in
    tail2 (n-1) new_list
  ;;

let count_trees_in_map step_right step_down lines =
  let rec count_fn = fun (pos, count) lines ->
    let current_line = List.hd lines in
    let new_pos = pos + step_right in
    let field = String.get current_line (new_pos mod String.length current_line) in
    let new_count = count + (if field = '#' then 1 else 0) in

    if List.length lines > 1 then
      count_fn (new_pos, new_count) (tail2 step_down lines)
    else
      new_count
  in

  count_fn (0, 0) (tail2 step_down lines)
  ;;

let () =
  let ic = open_in file in
  let lines = ref [] in

  try
    while true do
      let line = input_line ic in
      lines := !lines @ [line];
    done;
  with _e -> ();

  close_in_noerr ic;

  let trees_r1_d1 = count_trees_in_map 1 1 !lines in
  let trees_r3_d1 = count_trees_in_map 3 1 !lines in
  let trees_r5_d1 = count_trees_in_map 5 1 !lines in
  let trees_r7_d1 = count_trees_in_map 7 1 !lines in
  let trees_r1_d2 = count_trees_in_map 1 2 !lines in

  Printf.printf "Trees r1 d1: %d\n" trees_r1_d1;
  Printf.printf "Trees r2 d1: %d\n" trees_r3_d1;
  Printf.printf "Trees r5 d1: %d\n" trees_r5_d1;
  Printf.printf "Trees r7 d1: %d\n" trees_r7_d1;
  Printf.printf "Trees r1 d2: %d\n" trees_r1_d2;

  Printf.printf "\nMultiplied: %d\n" (trees_r1_d1 * trees_r3_d1 * trees_r5_d1 * trees_r7_d1 * trees_r1_d2);
