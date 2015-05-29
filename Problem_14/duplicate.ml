let duplicate : 'a list -> 'a list = function
  | [] -> []
  | h::t -> h::h::(duplicate t)
