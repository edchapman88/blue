open Blue

(* A mock implementation of [Markov.Agent.RLPolicyType], always choosing a fixed action and throwing an `End_Simulation` exception after 3 steps to terminate the test.  *)
module MockTerminatingPolicy = struct
  (* An exception to raise after a fixed number of steps. *)
  exception End_Simulation

  (* Keep track of the number of steps seen by the policy. *)
  type t = int
  type state = MarkovCompressor.state
  type reward = Reward.t
  type observer = unit -> state
  type action = unit -> unit

  type inference = {
    action : action;
    observer : observer;
    policy : t;
  }

  let init () = 0
  let init_observer = MarkovCompressor.observe

  let infer policy (state, reward) =
    if policy = 3 then raise End_Simulation;

    Log.write_msg @@ Reward.string_of_state_reward (state, reward);

    (* Always pick and action that executes a filewall command. *)
    let action () = System.(exec_eff ToggleGreen) in
    {
      action;
      observer =
        (fun () ->
          Unix.sleepf @@ Cli.obs_time_delay ();
          MarkovCompressor.observe ());
      policy = policy + 1;
    }
end

(* Create an agent with implementations from the [blue] library for [MarkovCompressor] and [Reward]. Use a mocked version of the [Policy] module, defined above. *)
module Agent =
  Markov.Agent.Make (MarkovCompressor) (Reward) (MockTerminatingPolicy)

let%expect_test "blue integration test with mocked nft" =
  Random.init 0;
  try Agent.act (MockTerminatingPolicy.init ())
  with MockTerminatingPolicy.End_Simulation ->
    [%expect
      {|
                                                 {"ok_rate":"0.00","green_blocked":false,"red_blocked":false,"reward":0}
                                                 [mock] nft command called
                                                 {"ok_rate":"0.00","green_blocked":false,"red_blocked":false,"reward":0}
                                                 [mock] nft command called
                                                 {"ok_rate":"0.00","green_blocked":false,"red_blocked":false,"reward":0}
                                                 [mock] nft command called
                                                 |}]
