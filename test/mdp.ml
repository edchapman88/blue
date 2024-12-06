open! Lib

type coin =
  | Heads
  | Tails

type effect =
  | Wait
  | Flip

let random_eff () =
  match Random.bool () with
  | true -> Wait
  | false -> Flip

type reward = int

(** Initially set [Heads] as the goal state. *)
let game_goal = ref Heads

type sys_state = {
  s : coin;
  r : reward;
}

module System = struct
  let state = ref Heads
  let steps = ref 0

  let reset () =
    state := Heads;
    steps := 0

  exception End_Simulation

  let string_of_state = function
    | Heads -> Printf.sprintf "%i Heads" !steps
    | Tails -> Printf.sprintf "%i Tails" !steps

  let exec_effect effect =
    match effect with
    | Wait -> print_endline " -> Wait"
    | Flip -> (
        print_endline " -> Flip";
        match Random.bool () with
        | true -> state := Heads
        | false -> state := Tails)

  let observe () =
    steps := !steps + 1;
    if !steps > 30 then raise End_Simulation;
    !state |> string_of_state |> print_string;
    let reward = if !state = !game_goal then 1 else 0 in
    { s = !state; r = reward }
end

module CountBased : Mdp.PolicyType = struct
  type t = (coin * reward) list

  let init () = []
  let init_observer = System.observe

  type state = sys_state
  type observer = unit -> sys_state
  type action = unit -> unit

  type inference = {
    action : action;
    observer : observer;
    policy : t;
  }

  let most_valuable policy =
    let h_sum, t_sum =
      List.fold_left
        (fun (h_acc, t_acc) (s, r) ->
          match s with
          | Heads -> (h_acc + r, t_acc)
          | Tails -> (h_acc, t_acc + r))
        (0, 0) policy
    in
    if h_sum > t_sum then Heads else Tails

  let infer policy state =
    let policy' = (state.s, state.r) :: policy in
    let goal = most_valuable policy in
    let chosen_eff =
      (* Exploration. *)
      if List.length policy < 20 then random_eff ()
      else if (* Exploitation. *)
              state.s = goal then Wait
      else Flip
    in
    if List.length policy == 20 then print_endline "\n   Start exploiting";
    let action () = System.exec_effect chosen_eff in
    {
      action;
      observer =
        (fun () ->
          Unix.sleepf 0.1;
          System.observe ());
      policy = policy';
    }
end

module Actor = Mdp.Make ((CountBased : Mdp.PolicyType))

let%expect_test "Run MDP actor with a count-based policy" =
  Random.init 0;
  try
    let _loop = Actor.act (CountBased.init ()) in
    ()
  with System.End_Simulation -> (
    [%expect
      {|
      1 Heads -> Flip
      2 Heads -> Flip
      3 Heads -> Flip
      4 Heads -> Wait
      5 Heads -> Wait
      6 Heads -> Flip
      7 Tails -> Flip
      8 Heads -> Wait
      9 Heads -> Wait
      10 Heads -> Wait
      11 Heads -> Flip
      12 Heads -> Wait
      13 Heads -> Flip
      14 Tails -> Wait
      15 Tails -> Flip
      16 Heads -> Flip
      17 Tails -> Flip
      18 Heads -> Flip
      19 Tails -> Wait
      20 Tails -> Flip
      21 Tails
         Start exploiting
       -> Flip
      22 Heads -> Wait
      23 Heads -> Wait
      24 Heads -> Wait
      25 Heads -> Wait
      26 Heads -> Wait
      27 Heads -> Wait
      28 Heads -> Wait
      29 Heads -> Wait
      30 Heads -> Wait
      |}];

    (* Re-run with the same seed, but with the opposite state rewarded as the goal. *)
    Random.init 0;
    game_goal := Tails;
    System.reset ();
    try
      let _loop = Actor.act (CountBased.init ()) in
      ()
    with System.End_Simulation ->
      [%expect
        {|
        1 Heads -> Flip
        2 Heads -> Flip
        3 Heads -> Flip
        4 Heads -> Wait
        5 Heads -> Wait
        6 Heads -> Flip
        7 Tails -> Flip
        8 Heads -> Wait
        9 Heads -> Wait
        10 Heads -> Wait
        11 Heads -> Flip
        12 Heads -> Wait
        13 Heads -> Flip
        14 Tails -> Wait
        15 Tails -> Flip
        16 Heads -> Flip
        17 Tails -> Flip
        18 Heads -> Flip
        19 Tails -> Wait
        20 Tails -> Flip
        21 Tails
           Start exploiting
         -> Wait
        22 Tails -> Wait
        23 Tails -> Wait
        24 Tails -> Wait
        25 Tails -> Wait
        26 Tails -> Wait
        27 Tails -> Wait
        28 Tails -> Wait
        29 Tails -> Wait
        30 Tails -> Wait
        |}])
