let preferences_dir = "preferences"
let guests_csv = Printf.sprintf "%s/guests.csv" preferences_dir
let guest_pref_dir name = Printf.sprintf "%s/%s.csv" preferences_dir name
let preferences_header = [ "task"; "pref" ]
