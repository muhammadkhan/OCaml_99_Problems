type 'a rle = One of 'a | Many of int*'a

let encode (lst : 'a list) : 'a rle list =
  let get_rle k x =
    if k = 1 then One x
    else Many (k,x)
  in
  let extract = function
    | Some v -> v
    | None -> failwith "problem"
  in
  let rec traverse (cnt, what_to_look_for) = function
    | [] ->
       if what_to_look_for = None then []
       else [get_rle cnt (extract what_to_look_for)]
    | h::t ->
       match what_to_look_for with
       | None -> traverse (1, Some h) t
       | Some w ->
	  if h = w then traverse (cnt + 1, what_to_look_for) t
	  else
	    let prefix =
	      if cnt = 1 then One w else Many (cnt,w)
	    in
	    prefix::(traverse (1, Some h) t)
  in
  traverse (0, None) lst
