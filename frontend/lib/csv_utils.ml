open! Base

let read_csv ?(skip_header = true) fpath =
  let lines = In_channel.with_open_text fpath In_channel.input_lines in
  if skip_header then List.tl_exn lines else lines
;;
