let preferences_dir = "preferences"
let chores_dir = "chores"
let guests_csv = Printf.sprintf "%s/guests.csv" preferences_dir
let misc_chores_csv = Printf.sprintf "%s/misc_chores.csv" chores_dir
let guest_pref_dir name = Printf.sprintf "%s/%s.csv" preferences_dir name
let preferences_header = [ "task"; "pref" ]
let days = [| "sun"; "mon"; "tue"; "wed"; "thu"; "fri"; "sat" |]

let days_dict =
  Base.Array.mapi days ~f:(fun i a -> a, i)
  |> Array.to_list
  |> Base.Map.of_alist_exn (module Base.String)
;;
