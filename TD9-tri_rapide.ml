let rec partitionne l x = match l with
  |[]->([],[],[])
  |u::r-> match partitionne r x with
    |(a,b,c)-> if u<x then (u::a,b,c) else if u = x then (a,u::b,c) else (a,b,u::c) ;;
  

let rec qsort l = match l with
  |[]->[]
  |[x]->[x]
  |x::r -> let a,b,c=partitionne l x in (qsort a) @ b @(qsort c) ;;

















let swap t i j = let a = t.(i) in t.(i)<-t.(j) ; t.(j)<-a ;;



let parti t deb fin piv =
  let a = t.(piv) in 
  swap t piv (fin-1) ;
  let i = ref deb in
  for j = deb to fin-2 do
    if t.(j)<=a then begin swap t (!i) j ; i:= !i+1 end
  done ; swap t (!i) (fin-1) ; !i ;;



let tri_rapide t = 
  let rec aux t deb fin = 
    if deb<fin then 
    let k = parti t deb fin deb in  
    (aux t deb k ; aux t (k+1) fin )
  in aux t 0 (Array.length t) ;;





















let tableau_pif n p = let t = Array.make n 0 in 
  for c = 0 to n-1 do 
      t.(c)<- Random.int p
  done ; t ;;


let rec list_pif n p = 
  let rec aux n p acc = 
    if n=0 then acc
    else aux (n-1) p ((Random.int p )::(acc))
  in aux n p [] ;;







let rec addition_recursive_terminale a b = if a = 0 then b else addition_recursive_terminale (a-1) (b+1) ;;


let miroir_terminale u = 
  let rec aux u v = match u with
    |[]->v
    |x::r -> aux r (x::v)
  in aux u [] ;;







let rec max_term u = match u with
  |[]-> min_int
  |[x]->x
  |x::y::r-> if x<y then max_term (y::r) else max_term (x::r) ;;

let min_max_term u = 
  let rec aux u v = match u,v with
  |[],[]->(max_int,min_int)
  |[x],[y]->(x,y)
  |x::y::r,a::b::rr->if x<y then if a<b then aux (x::r) (b::rr) else aux (x::r) (a::rr) else if a<b then aux (y::r) (b::rr)   else aux (y::r) (a::rr)
  |_,_->failwith "longueurs différentes"
  in aux u u ;;




let (<|>) a b = 
  let rec aux a b u =
    if a-1=b then u
    else aux a (b-1) (b::u)
  in aux a (b-1) [] ;;



let rec (@|) u v = let a = miroir_terminale u in 
  let rec aux u v = match u with
  |[]->v
  |x::r-> aux r (x::v)
  in aux a v ;;




let map_term f u =
  let rec aux f u v = match u with
  |[]->v
  |x::r-> aux f r ((f x)::v)
  in miroir_terminale (aux f u []) ;;





let rec fold_right f a u = match u with
  |[]->failwith "vide"
  |[x]-> f x a
  |x::r -> f x (fold_right f a r) ;; 
  
  
  
  
(* elle est recursive terminale*)

let rec fold_left f a u =
  let rec aux f u x = match u with
  |[]-> failwith "vide"
  |[m]-> f x m
  |m::r->aux f r (f x m)
  in aux f u a ;;


let fold_left_terminale f a u =
  let q = miroir_terminale u in
  let rec aux f u x = match u with
    |[]-> failwith "vide"
    |[m]-> f m x
    |m::r->aux f r (f m x)
  in aux f q a ;;




(* pour le tri fusion, on ne parcourt pas la liste en longueur mais par division successives de sa longueur, d'où la difficulté*)



let separe_terminale u = 
  let rec aux u (a,b) = match u with 
    |[]->(a,b)
    |[x]->(x::a,b)
    |x::y::r-> aux r (x::a,y::b)
  in aux u ([],[]) ;;

let fusion_terminale p q = let a,b=miroir_terminale p, miroir_terminale q in
  let rec aux a b u = match a,b with
  |_,[] -> (miroir_terminale a) @| u
  |[],_ -> (miroir_terminale b) @| u
  |x::xs,y::ys -> if x<y then aux a ys (y::u) else aux xs b (x::u) (* il faut inverser l'ordre de x et y *)
  in aux a b [] ;;


let rec tri_fusion u = match u with
  |[]-> []
  |[x]->[x]
  |_ -> let a,b = separe_terminale u in fusion_terminale (tri_fusion a) (tri_fusion b) ;;