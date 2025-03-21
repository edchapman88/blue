open Blue

let () = Cli.arg_parse ()

module Policy = CountBasedPolicy
module Agent = Markov.Agent.Make (MarkovCompressor) (Reward) (Policy)

let () =
  Random.init 0;
  Agent.act (Policy.init ())
