(* let rec (<|>) a b = if a=b then [] else a::((a+1)<|> b) ;;






let rec records cmp u =
  match u with
  |[]-> []
  |x::r->
  let rec aux cmp u courant = 
    match u with
    |[]->[]
    |x::r when cmp x courant >0 -> x::( aux cmp r x)
    |x::r->aux cmp r courant
  in aux cmp u x;;



let rec records2 cmp u = match u with
  | [] -> []
  | [a] -> [a]
  | a :: b :: q ->
      if cmp b a > 0 then a :: records2 cmp (b :: q) 
      else records2 cmp (a :: q)



let rec pgcd a b =
  let rec aux a b x =
    if b=0 then a,x else 
    aux b (a mod b) (x+1)
  in aux a b 0;;




let phi n =
  let rec aux k n = if k=0 then 0 
    else let a,b= aux (k-1) n,(snd (pgcd n k)) in if a>b then a else b
  in aux n n ;;





let records_euclide borne =
  let a = List.map (fun i -> (i,phi i)) ( 2<|>borne) in
  records (fun x y -> (snd x - snd y)) a;;















let rec f n = if n = 1 then 0 else if n mod 2 = 0 then 1+ f(n/2) else 1+f(n+1) ;;


let records_bis borne =
   let a = List.map (fun i -> f i) ( 2<|>borne) in
  records (fun x y -> ( x - y)) a;;












































(*6.9*)



let rec f m n =
  if n=0 then 1
  else if m = 1 then 2*n
  else f (m-1) (f m (n-1)) ;;


*)









let rec (<|>) a b = if a=b then [] else a::( (a+1)<|> b) ;;




let records cmp u = 
  let rec aux u x= match u with
    |[]->[]
    |a::r-> if cmp a x >0 then a::( aux r a) else aux r x
  in match u with
  |[]->[]
  |x::r->x::(aux r x) ;;



let rec f a b = if b =0 then 0 else 1+f b (a mod b) ;;





let phi n = let rec aux n k = if n = 0 then 0 else
  if k=0 then f n 0 else let a,b= f n k , aux n (k-1) in
  if a>b then a else b
  in aux n (n-1) ;;


let records_euclide n = 
  let a = 0<|>n in
  let rec aux a = match a with
  |[]->[]
  |x::r-> (x,phi x)::(aux r) in
  records (fun (x,y) (m,p) ->y-p) (aux a) ;;

  (* fibonacci*)







let rec ff n = 
  if n = 1 then 0 else 
  if n mod 2 = 0 then 1+ ff ( n/2) 
  else 1+ ff (n+1) ;;



let records_ff n = 
  let a = 1<|>n in
  let rec aux a = match a with
  |[]->[]
  |x::r-> (x,ff x)::(aux r) in
  records (fun (x,y) (m,p) ->y-p) (aux a) ;;
  








  
  

let rec fack m n = 
  if n = 0 then 1 else
  if m = 1 then 2*n else
  fack (m-1) (fack m (n-1)) ;;















(*


let rec power_mod a b n = if b = 0 then 1 else a*(product_mod a (b-1) n) mod n

let ack4 n  modu = 
  let rec aux u = if u = 0 then 13 else
  in power_mod 
*)