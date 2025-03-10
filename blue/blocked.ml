module BlockedSet = Set.Make (String)

let sys_cmd cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let blocked_of_string msg =
  let open Re in
  let regex =
    seq
      [ rep digit; str "."; rep digit; str "."; rep digit; str "."; rep digit ]
    |> compile
  in
  matches regex msg |> BlockedSet.of_list

let query_blocked () =
  sys_cmd "nft list set inet filter blocked" |> blocked_of_string

let toggle_block ip =
  let exit_code =
    if BlockedSet.mem ip @@ query_blocked () then
      Sys.command
      @@ Printf.sprintf "nft 'delete element inet filter blocked { %s }'" ip
    else
      Sys.command
      @@ Printf.sprintf "nft 'add element inet filter blocked { %s }'" ip
  in
  if exit_code = 0 then Ok ()
  else Error (Printf.sprintf "Failed to toggle the block on IP address: %s" ip)
