

/*Tail recursive split:*/


let split l = let rec split l f =  match l with
|[] -> f ([],[])
|hd :: [] -> f ([hd], [])
|hd1 :: hd2 :: tl -> 
	split tl ( fun (a,b) ->
		 f (hd1::a,hd2::b)) in
			split l (fun x -> x);;

/*Tail recursive merge :*/

		
let rec merge acc = function
|[], [] -> List.rev (acc)
|x, [] ->(List.rev acc) @ x
|[], x ->(List.rev acc) @ x
|x :: xs, y :: ys -> if x < y then merge (x::acc) (xs, (y::ys)) else merge (y::acc) ((x::xs), ys)
/*
Tail recursive mergesort algorithm:
*/
let mergesort list = 

let rec mergesort list f = match list with  
|[] -> f []
|[x] -> f [x]
|xl -> let a , b = split xl in 
mergesort a ( fun ra -> 
mergesort b ( fun rb -> 
f (merge [] (ra,rb) )))
in mergesort list (fun x -> x);;

