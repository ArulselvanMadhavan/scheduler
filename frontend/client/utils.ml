open! Base

let log_response response =
  Core.Or_error.iter_error response ~f:(fun e ->
    Brr.Console.(log [ str (Error.to_string_hum e) ]))
;;
