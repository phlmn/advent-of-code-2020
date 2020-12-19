open Core

let file = "input.txt"

type fieldType =
  | BYR
  | IYR
  | EYR
  | HGT
  | HCL
  | ECL
  | PID
  | CID
  | Invalid

let parse_field field =
  let parts = String.split ~on:':' field in

  match parts with
    | ["byr"; year_str] ->
      let year = int_of_string year_str in
      if year >= 1920 && year <= 2002
      then
        Some(BYR)
      else
        Some(Invalid)
      ;
    | ["iyr"; year_str] ->
      let year = int_of_string year_str in
      if year >= 2010 && year <= 2020
      then
        Some(IYR)
      else
        Some(Invalid)
      ;
    | ["eyr"; year_str] ->
      let year = int_of_string year_str in
      if year >= 2020 && year <= 2030
      then
        Some(EYR)
      else
        Some(Invalid)
      ;
    | ["hgt"; value] ->
      let suffix = String.slice value (-2) (0) in
      let num = String.slice value 0 (-2) in

      (
        match [int_of_string_opt(num), suffix] with
        | [Some(a), "cm"] when a >= 150 && a <= 193 -> Some(HGT)
        | [Some(a), "in"] when a >= 59 && a <= 76 -> Some(HGT)
        | _ -> Some(Invalid)
      )
    | ["hcl"; value] ->
      let re = Str.regexp "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$" in
      if Str.string_match re value 0 then
        Some(HCL)
      else
        Some(Invalid)
    | ["ecl"; value] ->
      (
        match value with
        | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> Some(ECL)
        | _ -> Some(Invalid)
      )
    | ["pid"; value] ->
      if String.length value = 9 && Option.is_some (int_of_string_opt value) then
        Some(PID)
      else
        Some(Invalid)
    | ["cid"; _] -> Some(CID);
    | _ -> None;
;;

let parse_line line =
  let parts = String.split line ~on:' ' in
  List.filter_map ~f:parse_field parts
;;

let fold_line acc line =
  let latest_line = List.hd_exn acc in
  if List.length line > 0 then ([line @ latest_line] @ (List.tl_exn acc)) else ([[]] @ acc)
;;

let contains line e =
  not (phys_equal (List.find line ~f:(fun a -> phys_equal a e)) None)

let is_valid line =
  (contains line BYR) &&
  (contains line IYR) &&
  (contains line EYR) &&
  (contains line HGT) &&
  (contains line HCL) &&
  (contains line ECL) &&
  (contains line PID)
  (* (contains line CID) *)
;;


let () =
  let ic = In_channel.create file in
  let lines = In_channel.input_lines ic in
  In_channel.close ic;

  let parsed_lines = List.map ~f:parse_line lines in

  let asdas = List.rev (List.fold_left ~init:[[]] ~f:fold_line parsed_lines) in
  Printf.printf "Test: %d" (List.length (List.filter asdas ~f:is_valid))

;;