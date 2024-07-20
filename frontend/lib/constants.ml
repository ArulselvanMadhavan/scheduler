open! Base

let guests_dir = "guests"
let preferences_dir = "preferences"
let chores_dir = "chores"
let guests_with_preferences = "guests_with_preferences"
let delete_guests = "delete_guests"
let guests_csv = Printf.sprintf "%s/guests.csv" guests_dir
let misc_chores_csv = Printf.sprintf "%s/misc_chores.csv" chores_dir
let guest_pref_dir name = Printf.sprintf "%s/%s.csv" preferences_dir name
let preferences_header = [ "task"; "pref" ]
let chores_header = [ "chore_name"; "hours"; "priority" ]
let guests_header = [ "guest"; "hours" ]
let days = [| "sun"; "mon"; "tue"; "wed"; "thu"; "fri"; "sat" |]
let day_chore_names = [| "AM_clean"; "cook_1"; "PM_clean_1"; "PM_clean_2" |]
let day_chore_hours = [| 0.5; 2.; 1.; 1. |]
let day_chore_prio = [| 2; 2; 2; 2 |]

let weekly_chores =
  let clen = Array.length day_chore_names in
  let len = Array.length days * clen in
  let arr = Array.create ~len ("", 0., 0) in
  let update_mat d_i d =
    let day_chore c_i n =
      let i = (d_i * clen) + c_i in
      arr.(i) <- Printf.sprintf "%s_%s" d n, day_chore_hours.(c_i), day_chore_prio.(c_i)
    in
    Array.iteri day_chore_names ~f:day_chore
  in
  Array.iteri days ~f:update_mat;
  arr
;;

let days_dict =
  Array.mapi days ~f:(fun i a -> a, i)
  |> Array.to_list
  |> Map.of_alist_exn (module String)
;;
