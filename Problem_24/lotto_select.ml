let rec lotto_select (num : int) (bound : int) : int list =
  if num = 0 then []
  else (Random.int bound)::(lotto_select (num - 1) bound)
