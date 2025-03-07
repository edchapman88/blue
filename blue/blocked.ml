module BlockedSet = Set.Make (String)

let blocked_of_string msg =
  let open Re in
  let regex =
    seq
      [ rep digit; str "."; rep digit; str "."; rep digit; str "."; rep digit ]
    |> compile
  in
  matches regex msg |> BlockedSet.of_list
