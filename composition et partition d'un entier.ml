(* 1 *)
(* a) flemme  *)
(* b)  cela revient à choisir où placer n 1  dans k emplacements soit k^n *)
(* c) en faisant varier k entre 1 et n on fait juste une somme *)



(*2*)
(* a) xp(k-1)-1, p(k)+1 si p(k-1)>1, sinon on applique ça à ceux d'avant*)

let rec scinde l = 
  match l with
  |[]->(0,[])
  |x::r -> if x=1 then ( 1+ fst (scinde r),snd ( scinde r)) else ( 0,x::r);;

let rec descinde (a,b) = if a=0 then b else descinde (a-1,1::b) ;;

let rec composition_suivante l = 
  let (a,b) = scinde l in
  match b with
  |[]->[]
  |x::r -> descinde (a+1,(x-1)::r) ;;


let rec proc l n = 
  match l with
  |[]-> []
  |[x] -> if fst (scinde x ) = n-3 then [descinde (n,[])] else [x]@(proc ([composition_suivante x]) n );;

let composition n = proc [[n]] n ;;