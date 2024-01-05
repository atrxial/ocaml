type ('a, 'b) arbre =
  | Interne of 'a * ('a, 'b) arbre * ('a, 'b) arbre
  | Feuille of 'b ;;



(* faire des terminalisations aussi*)


let rec hauteur a = match a with
  |Feuille f -> 0
  |Interne (_,g,d) -> 1+max (hauteur g) (hauteur d) ;;


let rec taille a = match a with
  |Feuille  f -> 1
  |Interne (_,g,d) -> 1+ taille g + taille d ;;


let rec dernier a = match a with
  | Feuille f -> f
  |Interne (_,g,d) -> dernier d ;;









let rec affiche_prefixe a = match a with
  |Feuille f -> print_newline (print_int f)
  |Interne (x,g,d) -> print_newline (print_int x) ; affiche_prefixe g ; affiche_prefixe d ;;








let rec affiche_infixe a = match a with
  |Feuille f -> print_newline (print_int f)
  |Interne (x,g,d) -> affiche_infixe g ; print_newline (print_int x) ;  affiche_infixe d ;;






let rec affiche_postfixe a = match a with
  |Feuille f -> print_newline (print_int f)
  |Interne (x,g,d) ->  affiche_postfixe g ; affiche_postfixe d ; print_newline (print_int x) ;;














type ('a, 'b) token = N of 'a | F of 'b ;;




let rec postfixe_naif a = match a with
  |Feuille f -> [f]
  |Interne (x,g,d) -> postfixe_naif g @ postfixe_naif d @ [x] ;;



let rec post_fixe a =
  let rec aux a l = match a with (* distinguer un cas ou on est qu'avec des feuilles en bas *)
  | Feuille f -> (F f )::l
  |Interne (x,g,d) -> (N x)::(aux d (aux g l))
  in List.rev (aux a []) ;;


let rec prefixe a =
  let rec aux a l = match a with
  | Feuille f -> (F f )::l
  |Interne (x,g,d) -> aux d (aux g ((N x)::l))
  in List.rev (aux a []) ;;


let rec infixe a =
  let rec aux a l = match a with
  |Feuille f -> (F f)::l
  |Interne (x,g,d) -> aux d (( N x)::(aux g l))
  in List.rev (aux a []) ;;


let rec post_fixe_term a = 
  let rec aux f e = match f with
  |[]->e
  |a::r -> match a with
  |Feuille m -> aux r (F m::e)
  |Interne (x,g,d) -> aux (d::g::r) (( N x)::e) (* l'ordre est choisi pour respecter l'ordre postfixe inverse, au final on construit bien la liste à l'envers*)
  in (aux [a] []) ;;

(* utiliser une file*)

let rec etiquettes_largeur a =
  let rec aux file u =
    match Queue.pop file with
    |Feuille f when Queue.is_empty file -> (F f)::u
    |Feuille f -> aux file ((F f)::u)
    |Interne (x,g,d) -> Queue.push g file ; Queue.push d file ; aux file ((N x)::u)
  in let m = Queue.create () in Queue.push a m ; List.rev_append (aux m []) [] ;;



let rec lire_etiquette u a =
  match u,a with
  |[],Feuille f -> f
  |[],Interne (x,_,_) -> x
  |x::r,Interne (_,g,d) when x -> lire_etiquette r d
  |x::r,Interne (_,g,d) -> lire_etiquette r g
  |_,Feuille f -> failwith "error" ;;


let rec incremente a u =
  match a,u with
  |Feuille f,[] -> Feuille (f+1)
  |Interne (x,g,d),[]-> Interne (x+1,g,d)
  |Interne (m,g,d),x::r when x -> Interne (m,g, incremente d r)
  |Interne (m,g,d),x::r -> Interne (m,incremente g r,d)
  |Feuille f,_ -> failwith "error" ;;


let affiche_avec_adresse x adresse =
    List.iter (fun b -> print_int (if b then 1 else 0)) adresse;
    Printf.printf " : %i\n" x ;;


let tableau_adresses a =
  let rec aux a cour = match a with
    |Feuille f -> affiche_avec_adresse f cour
    |Interne (x,g,d) ->  affiche_avec_adresse x cour  ; aux g (false::cour) ; aux d (true::cour)
  in aux a [];;






































































  (*refaire le type pile *)

type 'a pile = 'a list ref ;;

let pile_vide = ref [] ;;

let pop pile = match !pile with
  |[] -> failwith "impossible"
  |x::r ->  pile:= r ; x ;;

let push x pile = pile := (x::(!pile)) ;;

let lire_postfixe u  = 
  let rec aux pile l = match l with
  |[] -> pop pile 
  |x::r -> match x with
    |F f -> push (Feuille f) pile ; aux pile r
    |N m -> (let a = pop pile in let b = pop pile in push (Interne (m,b,a)) pile ; aux pile r)
  in aux pile_vide u ;;



(*utiliser une pile comme liste*) (* fonction à priori inutile, c'était juste pour tester*)

let lire_prefixe_izi u =
  let rec reverse a = match a with
    | Feuille f -> Feuille f
    |Interne (x,g,d) -> Interne (x, reverse d, reverse g)
    in reverse (lire_postfixe (List.rev u)) ;;




let lire_largeur u = (* mettre sous le module Queue*)
  let rec aux file u = match u with
  |[] -> pop file
  |x::r -> match x with
    |F f -> push (Feuille f ) file ; aux file r
    |N x -> let a = pop file in let b = pop file in push  (Interne (x,b,a))file ; aux file r
  in let m = file_vide in aux m u ;;







type cote = G | D ;;
  (* arbre binaire entier non étiqueté *)
type arbre = Feuille | Noeud of arbre * arbre ;;
  (* on annote chaque noeud interne avec le cardinal du sous-arbre
  correspondant *)
type arbre_annote =
  | N of int * arbre_annote * arbre_annote
  | F;;

let card = function
  | F -> 1
  | N(taille, _, _) -> taille ;;

let rec annote a = match a with
  |Feuille  -> F
  |Noeud (g,d) -> let a,b = annote g,annote d in match a,b with
    |F,F -> N (2,a,b)
    |F,N (x,_,_) |N (x,_,_),F -> N (x+1,a,b)
    |N (x,_,_),N (y,_,_) -> N (x+y+1,a,b) ;;


let insere (b,x,t) = (*faire un parcours préfixe après*) (*ne pas oublier d'augmenter le cardinal de chaque père après *)(*x est le noeud à avoir dans l'ordre préfixe*)
  let rec aux b x t =  match b,t with
  |F,D -> (N (3,F,F),3)
  |F,G  -> (N (3,F,F),2)
  |N (taille,g,d),G when x=1 -> (N (taille+1,F,b),taille +1) (* A FINIR*)
  |N (taille,g,d),D when x=1 -> (N (taille+1,b,F),taille +2)
  |N (taille,g,d),_ ->let t1= card g in let a,f =(if t1<=x then aux d (x-t1) t
    else aux g (x-1) t) in if t1<=x then (N (taille + 2,g,a),f)
    else (N (taille+2,a,d),f)
  in aux b x t ;;


let efface (a,f) = 
  let rec aux a f = match a with
  |F -> (F,1,D)
  |N (x,F,F) -> if f=1 then (F,1,G) else if f=2 then (F,1,D) else (F,1,D)
  |N (x,F,m) when f=1-> (m,x,G)
  |N (x,m,F) when f=1 ->(m,x,D)
  |N (taille,g,d) -> let t1=card g in let b,x,e = (if t1<=f then aux d (f-t1+1) else aux g (f-1)) in if t1<= f then (N (taille -1,g,b),x,e ) else (N (taille-1,b,d),x,e)
  in aux a f ;;


let test a = 
  let rec aux n a = if n=1 then true else insere (efface (a,n)) = (a,n) && aux (n-1) a 
  in aux (card a) a ;;