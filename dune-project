(lang dune 3.7)

(name markov)

(generate_opam_files true)

(source
 (github edchapman88/blue))

(authors "Ed Chapman")

(maintainers "Ed Chapman")

(license MIT)

(package
 (name markov)
 (synopsis "Interface between RL agents and continuous-time, concurrrently-running system")
 (description "Reinforcement learning agents assume they are interacting with a Markov Decision Process (MDP). Real-world systems are continuous-time and run concurrently with RL agents processes. This project provides a set of abstract interfaces that attempt to formalise the software components that must be implemented in order to develop an MDP-assuming RL agent capably of interacting with a continuous-time, concurrently-running system.")
 (depends (ocaml (>= 5.0)) dune (odoc :with-doc)))

(package
 (name blue)
 (synopsis "")
 (description "")
 (depends
   (ocaml (>= 5.0))
   dune
   (odoc :with-doc)
   domainslib
   lwt 
   markov
   core
   re
   ))
