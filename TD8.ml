(* type 'a file_fonct = 'a list * 'a list ;;

let file_vide = ([],[]) ;;

let miroir u =
  let rec proc l v = match l with
  |[]->v
  |x::xs-> proc xs (x::v)
  in proc u [] ;;



let ajoute x f = let a,b = f in (x::a,b) ;;

let rec enleve f = let e,s = f 
  in match s with
  |[]-> if e =[] then None else enleve ([],miroir e)
  |x::xs->Some (e,xs) ;;


let rec somme f =
  match f with
  |([],[])->0
  |(x::xs,[])-> x+ somme (xs,[])
  |(a,x::xs)-> x+ somme (a,xs) ;;

let file_fonct_of_list l = function 'a list ->
  match l with
  |[]->file_vide
  |x::xs-> (xs,[x])











*)



type 'a file_fonct = 'a list * 'a list ;;

let file_vide = ([],[]) ;;

let miroir u = 
  let rec aux u v = match u with
    |[]-> v
    |x::r -> aux r (x::v)
  in aux u [] ;;

let ajoute x file =  let e,s = file in (x::e,s)

let rec enleve file = match file with
  |[],[]-> None
  |v,[] -> enleve ([], miroir v)
  |e,x::r -> Some (x,(e,r)) ;;







  let rec somme f = match enleve f with
    |None -> 0
    |Some (x,ff) -> x + somme ff ;;



let  file_fonct_of_list u = 
  let rec aux u =  match u with
  |[]-> file_vide
  |x::r -> ajoute x (file_fonct_of_list r)
  in aux (miroir u) ;;


let rec file_fonct_of_list_ u = match u with
  |[]-> ([],[])
  |x::r-> match file_fonct_of_list r with
    |a,b -> (x::a,b) ;;

  

let rec itere_file f file = match enleve file with
  |None -> ()
  |Some (x,a) -> f x ; itere_file f a;;


let afficher = itere_file (fun i -> print_newline ( print_int i)) ;;

















type 'a pile =  {donnees : 'a option array ; mutable courant : int} ;;

let capacite p = Array.length p.donnees ;;



let nouvelle_pile n = { donnees = Array.make n None ; courant = -1} ;;

let pop pile = if pile.courant= -1 then None else
  let a = pile.donnees.(pile.courant) in 
  pile.donnees.(pile.courant)<- None ; pile.courant<- pile.courant -1 ; a ;;

let push x pile = if pile.courant < capacite pile then (pile.courant<-pile.courant +1 ; pile.donnees.(pile.courant)<- Some x )
  else failwith "PLEIN";;








type 'a file_i = {donnees : 'a option array ; mutable entree : int ; mutable sortie : int ; mutable cardinal : int} ;;




let file_vide_i n = { donnees = Array.make n None ; entree = 0 ; sortie = 0 ; cardinal = 0} ;;

let capacite_i file = Array.length file.donnees ;;


let ajoute_i x file = 
  if file.cardinal<capacite_i file then (file.cardinal<-file.cardinal+1 ; file.donnees.(file.entree)<-Some x ; file.entree<-(file.entree +1 ) mod capacite_i file)
 else failwith "PLEINE" ;;



let enleve_i file = 
  if file.cardinal > 0 then (let a = file.donnees.(file.sortie) in file.donnees.(file.sortie) <- None ; file.sortie<-(file.sortie +1 ) mod capacite_i file ; a )
  else None ;;


let de_liste_i u n = 
  let t = Array.make n None in
  let n = List.length u in
  let r = ref u in 
  for c = 0 to n-1 do
    match !r with
      |[]-> ()
      |x::v -> t.(c)<- x
  done ; { donnees = t ; entree = n ; sortie = 0 ; cardinal = n} ;;











let peek_1 pile = let a = pop pile in begin match a with 
  |None -> ()
  |Some x -> push x pile end ; a ;;


let est_vide_1 pile = peek_1 pile = None ;;


let iter_destructif f s =
  while not (est_vide_1 s) do
    match pop s with 
    |None -> ()
    |Some x -> f x
  done ;;



let iter_destructif_ f s = let a = nouvelle_pile (capacite s) in 
  while not ( est_vide_1 s) do
    match pop s with 
    |Some x -> (push x a ; f x)
  done ;  
  while not (est_vide_1 a) do
    match pop a with
    |Some x -> push x s
  done ;;

let copie s = let s_copie=nouvelle_pile (capacite s) in
  let miroir = nouvelle_pile (capacite s) in 
  iter_destructif_ (fun x -> push x miroir) s ; iter_destructif_ (fun x -> push x s_copie) miroir
  ; s_copie ;;





let egal u v = let a,b = copie u, copie v in
  let rec aux a b = match pop a, pop b with
  |None,None -> true
  |Some x, Some y -> x=y && aux a b
  |_-> false 
  in
  aux a b ;;




























let est_vide pile = pile.courant = -1 ;;

let flush pile =
  for c = 0 to capacite pile -1 do 
    pile.donnees.(c) <- None
  done ; pile.courant <- -1;;





let egal p q = p.courant = q.courant &&
  (let c = ref 0 in
  while !c<=p.courant && p.donnees.(!c)=q.donnees.(!c) do 
    c:= !c+1 
  done ; !c=p.courant +1 );;



let itere f pile = 
  for c = 0 to pile.courant +1 do
    match pile.donnees.(c) with
    |None-> ()
    |Some x -> f x
  done ;;














type 'a cell =
    | V
    | C of {elt : 'a; mutable prev : 'a cell; mutable next : 'a cell}
             
type 'a deque = {mutable left : 'a cell; mutable right : 'a cell}
    
let make_deque () = {left = V; right = V} ;;

let push_left x l = match l with
  |V
  |C {elt = a ; next = c} -> 


















































































type 'a pile_pyth = { mutable don : 'a option array ; mutable cour : int } ;;



let nouvelle_pile = { don = Array.make 1 None ; cour = -1} ;;


let push x a =
  let n = Array.length a.don in
  if a.cour = n-1 then ( let t = Array.make (2*n) None in 
  for c = 0 to n-1 do
    t.(c)<-a.don.(c)
  done ;t.(n)<- Some x ; a.cour<- a.cour +1 ; a.don <-t )
  else ( a.don.(a.cour +1)<- Some x ; a.cour<- a.cour +1 );;



let pop a = if a.cour = -1 then None
  else let x = a.don.(a.cour) in a.don.(a.cour)<- None ; a.cour<-a.cour -1 ; x ;;







