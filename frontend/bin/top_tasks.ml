open! Base
open! Magizhchi

let read_file file_name = Stdlib.In_channel.(with_open_text file_name input_lines)

let rec create_parent_dir fn =
  let open Stdlib in
  let parent_dir = Filename.dirname fn in
  if not (Sys.file_exists parent_dir)
  then (
    create_parent_dir parent_dir;
    Sys.mkdir parent_dir 0o755)
;;

let write_file ~file_name data =
  create_parent_dir file_name;
  Stdlib.Out_channel.(with_open_text file_name (Fn.flip output_string data))
;;

let read_chores () =
  let split_line line =
    let open Option.Let_syntax in
    let%map c = String.split ~on:',' line |> List.hd in
    c, 0
  in
  read_file Constants.misc_chores_csv
  |> Fn.flip List.drop 1
  |> List.filter_map ~f:split_line
  |> Map.of_alist_exn (module String)
;;

let read_guests () =
  let split_line line = String.split ~on:',' line |> List.hd in
  read_file Constants.guests_csv
  |> Fn.flip List.drop 1
  |> List.filter_map ~f:split_line
  |> Hash_set.of_list ~growth_allowed:false (module String)
;;

let process_prefs chores guests =
  let chores = ref chores in
  let split_name name =
    let open Option.Let_syntax in
    let%bind g = String.split ~on:'.' name |> List.hd in
    Option.some_if (Hash_set.mem guests g) g
  in
  let guests =
    Stdlib.Sys.readdir Constants.preferences_dir |> Array.filter_map ~f:split_name
  in
  let split_line line =
    match String.split ~on:',' line with
    | [ t; p ] -> Option.map (Int.of_string_opt p) ~f:(fun p -> t, p)
    | _ -> None
  in
  let handle_guest g =
    let guest_prefs =
      read_file (Constants.guest_pref_dir g)
      |> Fn.flip List.drop 1
      |> List.filter_map ~f:split_line
      |> Map.of_alist_multi (module String)
      |> Map.map ~f:List.sum (module Int)
    in
    let process_pref (t, p) =
      let on_found = function
        | Some old_p -> p + old_p
        | None -> p
      in
      chores := Map.update !chores t ~f:on_found
    in
    Map.iter guest_prefs ~f:process_pref
  in
  Array.iter guests ~f:handle_guest;
  !chores
;;

let generate_top_tasks () =
  let chores = read_chores () in
  let guests = read_guests () in
  let compare (_, p1) (_, p2) = Int.compare p2 p1 in
  let score_row (k, v) = Stdlib.Printf.sprintf "%s,%d" k v in
  process_prefs chores guests
  |> Map.to_alist
  |> List.sort ~compare
  |> List.map ~f:score_row
  |> List.cons (String.concat ~sep:"," Constants.top_tasks_hdr)
  |> String.concat ~sep:"\n"
  |> write_file ~file_name:Constants.top_tasks_csv
;;

let () = generate_top_tasks ()
