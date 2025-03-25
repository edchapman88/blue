(* Blue Executable: Defending a web-server.

   Commence a [Markov.Agent.S.act] loop to make observations of, and take actions in, the cyber system.

   A module of type [Markov.Agent.S] is created using the functor [Markov.Agent.Make]. The functor has three arguments (each a module) which must be implementations of [Markov.Agent.MarkovCompressorType], [Markov.Agent.RewardType] and [Markov.Agent.RLPolicyType].

   The [blue] library has an implementation for each of these module types, namely [Blue.MarkovCompressor], [Blue.Reward] and [Blue.CountBasedPolicy], respectively.
*)

open Blue
module Agent = Markov.Agent.Make (MarkovCompressor) (Reward) (CountBasedPolicy)

let () =
  Cli.arg_parse ();
  Agent.act (Agent.init_policy ())
