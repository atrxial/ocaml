type ’a cell = { valeur : ’a ; mutable next : ’a cell} ;;


type ’a liste = 
  | Nil 
  | Cellule of ’a cell ;;

type ’a file = { mutable queue : ’a liste} ;;


let newfile = { queue = Nil} ;;



let peek file = match file.queue with
  |Nil -> failwith "vide"
  |Cellule c -> c.valeur ;; 

let take file = match file.queue with
|Nil -> failwith "vide"
|Cellule c -> let a = c.valeur in file.queue <- Cellule c.next ; a ;;


let add x file = match file.queue with
|Nil -> let rec c=  { valeur = x ; next = c } in file.queue <- Cellule c (* tactique du let rec c ...*)
|Cellule c -> file.queue <- Cellule { valeur = x ; next = c} ;;





type 'a file = { pile_a : 'a Stack.t ; pile_b : 'a Stack.t} ;;





let newfile = { pile_a = Stack.new  ; pile_b = Stack.new} ;;




































