(** [file_of_dir dir_path] returns an [out_channel] for a new file created in the directory specified by [dir_path]. If the directory does not exist, it is created. The file name is the current UTC date and time. *)
let file_of_dir (dir : string) =
  let path = if String.ends_with ~suffix:"/" dir then dir else dir ^ "/" in
  (try (* Read and write for all users.*)
       Unix.mkdir path 0o777
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let open Core.Time_float in
  open_out (path ^ to_filename_string ~zone:Zone.utc (now ()) ^ ".txt")

(** A [ref] to an [out_channel option]. Once initialised, maintaining an open [out_channel] to a log file for the duration of the program. *)
let oc = ref None

(** [init_oc dir_path] initialises [oc] with an [out_channel] to a new log file created in the directory at [dir_path]. A new directory is created if it does not exist. *)
let init_oc dir_path = oc := Some (file_of_dir dir_path)

let rec write_msg msg =
  match Cli.log_path () with
  | None -> ()
  | Some path -> (
      match !oc with
      | None ->
          init_oc path;
          write_msg msg
      | Some chan ->
          let open Core.Time_ns in
          Printf.fprintf chan "[%d] %s%!" (to_int_ns_since_epoch (now ())) msg)
