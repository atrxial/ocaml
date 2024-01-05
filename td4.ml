let indice_mini t =
  let m = ref t.(0) in
  let i = ref 0 in
  for c=0 to Array.length t -1 do
	  let a = !m in if t.(c)<a then m:=a ; i:=c
	done ; !i ;;


let mini t =
  let m = ref t.(0) in
  for c = 0 to Array.length t -1 do
	  let a = !m in if t.(c)<a then m:=t.(c)
	done ; !m ;;












let somme_cumulees t =
  let s = ref 0 in
  let a=Array.make (Array.length t) 0 in
  for c=0 to Array.length t -1 do 
    s:=!s+ t.(c); a.(c) <- !s 
  done ; a ;;







let map f t = let a = Array.make (Array.length t) (f t.(0)) in
  for c= 0 to Array.length a -1 do 
    a.(c)<-f t.(c) 
  done ; a ;;


let init n f =
  let a=Array.make n ( f 0) in
  for c=0 to n -1 do 
    a.(c)<- f c
  done ; a ;;


let to_list t = let l = ref [] in
  for c = Array.length t -1 downto 0 do 
    l:= (t.(c))::(!l) 
  done ; !l ;;


let to_list_bis t =
  let rec aux t k = 
    if k = Array.length t then 
      [] 
    else 
      t.(k) ::(aux t (k+1)) 
  in
  aux t 0 ;;









let of_list l =
  let u = ref l in
  let t = Array.make (List.length l) (match l with |[]-> failwith "vide" |x::r -> x) in
  for c = 0 to Array.length t -1 do 
    match !u with 
    |[]->()
    |x::r-> t.(c)<- x ; u := r 
  done ; t ;;


let of_list_bis l =
  let t = Array.make (List.length l) (match l with |[]-> failwith "vide" |x::r -> x) in 
  let rec aux t u k = match u with 
    |[]-> ()
    |x::r -> t.(k)<-x ; aux t r (k+1) in
  aux t l 0 ; t ;; (* en gros on parcourt la liste récursivement en ajoutant dans t au fur et à mesure*)



















let indice x t =
  let i = ref None in
  let c = ref 0 in 
  while !c<Array.length t -1 && !i=None do
    if t.(!c)=x then i:=Some !c else c:=!c+1
  done ; !i ;;

let indice_bis x t = 
  let rec aux x t k = 
  if k=Array.length t
    then None
  else
    if t.(k)=x then Some k else aux x t (k+1)
  in
  aux x t 0 ;;









  

  
let cherche_dicho x t = let i = ref 0 in let j = ref ((Array.length t )-1) in 
  while (!j-(!i) )>1 do 
  let m = (!j+(!i))/2 in if t.(m)<=x then i:= m else j:=m
  done ; if t.(!i) = x then Some !i else None ;;


  let cherche_dicho_bis x t = (* indice i inclu, j exclu*)
  let rec aux i j =
    if i>=j then None
    else
      let m = (j+i)/2 in
      if t.(m)= x then Some m else
      if t.(m)<x then aux (m+1) j
      else aux i m
  in
  aux 0 (Array.length t) ;;












let range n = Array.init n (fun i-> i) ;;

  
  
  
  
  
  
let estPermutation t =
  let c = ref 0 in let n = Array.length t in
  let test = ref true in
  let u =  Array.make n false in  (* on utilise la méthode de mémoïsation*)
  while !c<n && !test do
    if t.(!c)>=n || t.(!c) <0 || u.(t.(!c)) then 
      test:=false
    else 
      u.(t.(!c))<-true ; 
    c:=!c+1
  done ; !test ;;





let composer u v =
  let t = Array.make (Array.length u) 0 in
  for c=0 to Array.length u -1 do 
    t.(c)<-u.(v.(c)) 
  done ; t ;;


let inverser t =
  let u = Array.make (Array.length t) 0 in
  for c = 0 to Array.length t -1 do
    u.(t.(c))<- c 
  done ; u ;;
  

  

  

let puissance t k =
  let a = ref t in 
  for c = 0 to k-1 do
    a:= composer t (!a) 
  done ; !a ;;



let ordre t =
  let n = Array.length t in
  let a = ref t in
  let k = ref 0 in
  while !a <> range n do
    a:= composer t (!a) ;
    k:=!k+1 
  done ; !k ;;

let periode t i =
  let va= ref t.(t.(i)) in
  let c = ref 1 in
  while !va<>t.(i) do
    c:=!c+1 ;
    va:=t.(!va)
  done ; !c ;;


let est_dans_orbite t i j = if t.(i)=j then true else
  let va = ref t.(t.(i)) in
  let test = ref (!va = j) in
  while !test=false && !va<>t.(i) do
    va:=t.(!va) ;
    test:=!va = j
  done ; !test ;;



let est_transposition t = (* transpo ssi t est sa propre bijection reciproque*)
  let c = ref 0 in
  while !c < Array.length t && t.(t.(!c))=(!c) do
    c:=!c+1
  done ; !c=Array.length t ;;





let est_cycle t = (* cycle ssi le nombre d'éléments où t o t(c) != c vaut 1*)
  let nb = ref 0 in
  for c = 0 to Array.length t -1 do
    if t.(t.(c))<>c then nb:=!nb+1
  done ; !nb=1 ;;


let periodes t = let n = Array.length t in
  let a = Array.make (n) 0 in
  for c=0 to n-1 do
    if a.(c)=0 then
      let p = periode t c in
      let i = ref c in
      for c = 0 to p-1 do
        a.(!i)<-p ;
        i:=t.(!i)
      done
  done ; a ;;





let rec pgcd a b = if b=0 then a else pgcd b (a mod b) ;;

let ppcm a b = a*b/(pgcd a b) ;;

let rec ppcm_liste t = List.fold_right ppcm t 1 ;;



let ordre_efficace t =
  let x= ref Array.make (n) 0 in
  for c=0 to Array.length t -1 do


