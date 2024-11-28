type obs
type 'a action = unit -> 'a
type reward

val act : 'a action -> (unit -> obs) -> reward * obs
