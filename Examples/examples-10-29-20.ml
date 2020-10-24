let rec for_to_do j k f = 
  if j > k then () else (f j; for_to_do (j+1) k f)

let fact n =
	let soln = ref 1 in 
  (for_to_do 1 n (fun i -> soln := (!soln * i)); 
   !soln)
