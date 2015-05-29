let rec insert_at (x : 'a) (k : int) : 'a list -> 'a list = function
  | [] -> if k = 0 then [x] else []
  | h::t as l -> if k = 0 then x::l else h::(insert_at x (k-1) t)
