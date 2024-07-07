open! Base

let read_csv ?(skip_header = true) fpath =
  let lines = In_channel.with_open_text fpath In_channel.input_lines in
  if skip_header then List.tl_exn lines else lines
;;

let write_csv ~fpath content =
  Out_channel.with_open_text fpath (fun oc -> Out_channel.output_string oc content);
  Out_channel.flush_all ()
;;
