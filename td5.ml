(*


let rec pour_tout pred u = 
  match u with
  |[]-> true
  |[x]-> pred x
  |x::r-> pred x && (pour_tout pred r) ;; (* ou alors if pred x then prout_tout pred r else false*)

let rec existe pred u = 
  match u with
  |[]-> false
  |[x]-> pred x
  |x::r-> pred x || (existe pred r) ;;

let rec filtre pred u = match u with
|[]-> []
|x::r -> if pred x then x::(filtre pred r ) else filtre pred r ;;



let appartient u x = existe (fun i->i=x) u ;;

let rec inclus u v =  match u with
  |[]-> false
  |[x]-> appartient v x
  |x::r-> (appartient v x) && inclus r v ;;
  (* dans le pire des cas on fait u*v opérations*)

let egal u v = inclus u v && inclus v u ;;


let rec inter u v = match u with
  |[]->[]
  |x::r -> if appartient v x then x::(inter r v ) else inter r v ;;
  (* inter est en u*v opérations*)

let rec prive_de u v = match u with (* on enlève tous ceux dans v u privé de v donc on fait récursivement sur v*)
  |[]->[]
  |x::r -> if appartient v x then prive_de r v else x::( prive_de r v) ;;

let union u v = u @ (prive_de v u ) ;;(* u + u*v*)




(*
let rec union_eff u v = (*u+v*)
  match u,v with
  |[],_ ->v
  |_,[]->u
  |x::xs, y::ys -> if x<y then x::union_eff xs v else if y<x then y::union_eff u ys else x::(union_eff xs ys) ;;

let rec inter_eff u v = (*u+v*)
  match u,v with
  |[],_ ->[]
  |_,[]->[]
  |x::xs, y::ys -> if x<y then inter_eff xs v else if y<x then inter_eff u ys else x::(inter_eff xs ys) ;;






let rec prive_de_eff u v =
  match u,v with
  |[],_ ->[]
  |_,[]->u
  |x::xs, y::ys -> if x<y then prive_de_eff xs v
  else if y<x then prive_de_eff u ys
  else (prive_de_eff xs ys) ;;






let rec inclus_eff u v =
  match u,v with
  |[],_->true
  |x::xs, y::ys when x=y -> inclus_eff xs ys
  |x::xs, y::ys when x>y-> inclus_eff u ys
  |_-> false


let rec egal_eff u v = u=v ;;

*)










let rec union_eff u v = match u,v with
  |[],_ -> v
  |_,[]->u
  |x::xs,y::ys when x<y -> x::(union_eff xs v)
  |x::xs,y::ys when y<x -> y::(union_eff u ys)
  |x::xs,y::ys -> x::(union_eff xs ys) ;;

let rec inter_eff u v = match u,v with
|[],_ -> []
|_,[]->[]
|x::xs,y::ys when x<y -> (inter_eff xs v)
|x::xs,y::ys when y<x -> (inter_eff u ys)
|x::xs,y::ys -> x::(inter_eff xs ys) ;;



let rec prive_de_eff u v = match u,v with
|[],_-> []
|_,[]->u
|x::xs,y::ys when x<y -> x::(prive_de_eff xs v)
|x::xs,y::ys when y<x -> (prive_de_eff u ys) (* on prend pas y puisqu'il n'est donc pas dans la liste u*)
|x::xs,y::ys -> (prive_de_eff xs ys) ;;


let rec inclus_eff u v = match u,v with
|[],_ -> true
|_,[]->false
|x::xs,y::ys when x<y -> false
|x::xs,y::ys when y<x -> (inclus_eff u ys)
|x::xs,y::ys -> (inclus_eff xs ys) ;;









let rec split u = match u with
  |[]->([],[])
  |[x]->([x],[])
  |x::y::r -> let a,b=split r in (x::a,y::b) ;;


let rec fusionne_unique (u,v) = match u,v with
  |[],_ ->v
  |_,[]->u
  |x::xs,y::ys -> if x<y then x::(fusionne_unique (xs,y::ys)) else if y<x then y::(fusionne_unique (x::xs,ys)) else x::(fusionne_unique (xs,ys)) ;;


let rec tri_unique_fusion u = match u with
  |[]->[]
  |[x]->[x]
  |v-> let a,b= split v in fusionne_unique ((tri_unique_fusion a),(tri_unique_fusion b)) ;;

































let map_prefixe x = List.map (fun i-> x::i) ;; (*GOOD*)


let rec partie u = match u with (*GOOD*)
  |[]->[[]]
  |x::r-> map_prefixe x ( partie r) @ partie r ;;






let rec combinaison u n = if n =0 then [[]] else match u,n with
|[],_->[]
|v,1-> List.map (fun i-> [i]) v
|x::r,_-> map_prefixe x ( combinaison r (n-1) ) @ combinaison r (n);;

let parties_bis u = (* euh pas très effficace*)
  let l=ref [] in
  for c= 0 to List.length u do
    l:=(combinaison u c)@(!l)
  done ; !l ;;





let rec insertion u x = 
  let rec ajout l x n = if n = 0 then x::l else
  match l with
  |[]->[]
  |a::b-> a::(ajout b x (n-1))
  in
  let rec aux l x n = if n = 0 then [x::l] else match l with
    |[]->[]
    |r-> (ajout l x n)::(aux l x (n-1))
  in aux u x (List.length u) ;;



let rec flatten u = (* inutile*)
  match u with
  |[]->[]
  |x::r-> x@ (flatten r) ;;


let rec ajout_x_liste l x = match l with
  |[]->[]
  |u::r-> insertion u x @ ajout_x_liste r x ;;

let rec permutation u =
  match u with
  |[]->[[]]
  |x::r-> ajout_x_liste (permutation r) x ;;



  

let rec p_listes u p = if p = 0 then [[]] else
  let rec ajout_x_devant l x = match l with
  |[]->[]
  |y::r-> (x::y)::(ajout_x_devant r x)
  in 
  let rec ajout_tout_u_devant u l =
  match u with
  |[]->[]
  |x::r ->(ajout_x_devant l x)@ajout_tout_u_devant r l
  in ajout_tout_u_devant u (p_listes u (p-1)) ;;


let ppm_array t =
  let a = Array.make (Array.length t) false in
  for c=0 to Array.length t - 1 do 
    if t.(c)<Array.length t then a.(t.(c)) <- true
  done ;
  let trouve = ref false in
  let i = ref 0 in
  let c = ref 0 in
  while not(!trouve) && !i<Array.length a do
    if not(a.(!i)) then begin trouve:=true ; c:=!i end ; i:=!i+1
  done ; if !trouve then !c else Array.length a ;;




*)




(* EXERCICE 1*)
let rec pour_tout p u = match u with
  |[]->true
  |x::r-> p x && pour_tout p r ;;

let rec existe p u = match u with
  |[]->true
  |x::r-> p x || pour_tout p r ;;


let rec filtre p u =  match u with
  |[]->[]
  |x::r when p x -> x::( filtre p r)
  |x::r-> filtre p r ;;










let appartient x = existe (fun a->a=x)

let inclus u v = pour_tout (fun x-> appartient x v) u (* u*v au max *)

let egal u v = inclus u v && inclus v u ;;


let inter u = filtre (fun x-> appartient x u) (* on fait de la curryfication *)

let prive_de u v = filtre (fun x->not(appartient x v)) u ;;

let union u v = v @ prive_de u v ;; (* complexité v + u*v*)



















let rec union_ u v = match u,v with
  |[],x|x,[]->x 
  |x::a,y::b when x<y -> x::(union_ a v)
  |x::a,y::b when x>y -> y::(union_ u b )
  |x::a,y::b-> x::( union_ a b) ;;




let rec inter_ u v = match u,v with
|[],x|x,[]->[]
|x::a,y::b when x<y -> inter_ a v
|x::a,y::b when x>y -> inter_ u b 
|x::a,y::b-> x::( inter_ a b) ;;




let rec prive_de_ u v = match u,v with
|[],_->[]
|_,[]->u
|x::a,y::b when x<y -> x::(prive_de_ a v)
|x::a,y::b when x>y -> prive_de_ u b 
|x::a,y::b-> prive_de_ a b ;;




let rec inclus_ u v = match u,v with
|[],_->true
|_,[]->false
|x::a,y::b when x=y-> inclus_ a b 
|x::a,y::b when x>y -> inclus_ u b 
|x::a,y::b -> false


let egal u v = u = v (* deux parties croissantes égales sont égales en représentations *)



(* complexité max en u + v*)

let rec tri_unique u = if u=[] then [] else
  let rec split u = match u with
    |[]->([],[])
    |[x]->([x],[])
    |x::y::r-> match split r with
      |(a,b) -> (x::a,y::b)
  in let rec fus (a,b) = match a,b with
    |[],[]->[]
    |x,[]|[],x -> x
    |x::xs,y::ys -> if x<y then x::(fus (xs,b)) else if y<x then y::fus( (a,ys) ) else x::(fus (xs,ys))
  in match u with 
  |[]->[]
  |[x]->[x]
  |_->let a,b = split u in fus ((tri_unique a),(tri_unique b)) ;;




  (* combi *)



  (* *)

let ppm_array t = (* mémoisation *)
  let n = Array.length t in
  let x = Array.make n false in 
  for c = 0 to n-1 do
    if t.(c)<n then  x.(t.(c))<- true 
  done ; (* 1ère étape du listing des éléments qui sont inférieurs à n*)
  let c = ref 0 in
  while !c<n && x.(!c)do 
    c:= !c+1
  done ; !c ;;





let rec partition u b = match u with
    |[]->([],[],0)
    |x::r -> match partition r b with
      |(v,w,k) -> if x<b then (x::v,w,k+1) else (v,x::w,k+1) ;;







let ppm_list u =






























