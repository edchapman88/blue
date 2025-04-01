(* Blue Executable: Defending a web-server.

   Commence a [Markov.Agent.S.act] loop to make observations of, and take actions in, the cyber system.

   A module of type [Markov.Agent.S] is created using the functor [Markov.Agent.Make]. The functor has three arguments (each a module) which must be implementations of [Markov.Agent.MarkovCompressorType], [Markov.Agent.RewardType] and [Markov.Agent.RLPolicyType].

   The [blue] library has an implementation for each of these module types, namely [Blue.MarkovCompressor], [Blue.Reward] and [Blue.CountBasedPolicy], respectively.
*)

open Blue

module type P =
  Markov.Agent.RLPolicyType
    with type state = MarkovCompressor.state
    with type reward = Reward.t

let policy_module is_server =
  if is_server then (module ServerPolicy : P) else (module CountBasedPolicy)

let () =
  Cli.arg_parse ();
  let is_server = Cli.server_policy () |> Option.is_some in
  let module Policy = (val policy_module is_server) in
  let module Agent = Markov.Agent.Make (MarkovCompressor) (Reward) (Policy) in
  Agent.act (Agent.init_policy ())
