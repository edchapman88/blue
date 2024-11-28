module type Mdp = sig
  type obs
  type observer = unit -> obs
  type 'a action = unit -> 'a
  type 'a reward

  type 'a inference = {
    action : 'a action;
    observer : observer;
  }

  val infer : 'a -> obs -> 'a inference
  val act : 'a action -> observer -> 'a
end

module Simple : Mdp = struct
  type obs =
    | Heads
    | Tails

  type observer = unit -> obs
  type 'a action = unit -> 'a
  type 'a reward

  type 'a inference = {
    action : 'a action;
    observer : observer;
  }

  let make_obs () = Heads

  let regular_observer : observer =
   fun () ->
    Unix.sleep 3;
    make_obs ()

  let action_succeeded (_effect : unit) = true

  let infer effect obs =
    if Bool.not (action_succeeded effect) then
      {
        action = (fun () -> print_endline "try again?");
        observer = regular_observer;
      }
    else
      match obs with
      | Heads ->
          {
            action = (fun () -> print_endline "cause side effect");
            observer = regular_observer;
          }
      | _ -> failwith "policy doesn't cover this!"

  let rec act action observer =
    let effect = action () in
    let obs = observer () in
    let { action = action'; observer = observer' } = infer effect obs in
    act action' observer'
end
