type ('a, 'b) table = {data : ('a * 'b) list array; hash : 'a -> int}

let rec set_list (k, v) u = 
  match u with
 |[]-> [(k,v)]
 |(p,m)::r when p=k -> set_list (k,v) r
 |x::r-> x::(set_list (k,v) r) ;;


 let rec get_list k u = match u with
 |[] -> None
 |(p,m)::r when p = k -> Some m
 |x::r -> get_list k r ;;

 let rec rem_list k u =  match u with
 |[] -> []
 |(p,m)::r when p = k -> r
 |x::r -> x::(rem_list k r) ;;

 let test_exo1 () =
  let u =
    [(12, 35); (1, 43); (10, 30)]
    |> set_list (17, 24)
    |> set_list (1, 42)
    |> rem_list 10 in
  assert (get_list 10 u = None
          && get_list 12 u = Some 35
          && get_list 1 u = Some 42
          && get_list 17 u = Some 24) ;;

let empty_table n f = { data = Array.make n [] ; hash = f }


let get k {data = t; hash = f} = get_list k t.(f k mod (Array.length t)) ;;

let set (k, v) {data = t; hash = f} = t.(f k mod (Array.length t)) <- set_list (k,v) t.(f k mod (Array.length t)) ;;


let rem k {data = t; hash = f} = t.(f k) <- rem_list k t.(f k mod (Array.length t))


let from_list u n f = let t = Array.make n [] in List.iter ( fun (k,v) -> t.(f k) <- (k,v)::t.(f k)) u ;
    {data = t ; hash = f} ;;


let items { data = t ; hash = f} =
    let a =  ref [] in 
    for c = 0 to Array.length t -1 do 
        a := t.(c) @ (!a) 
    done ; !a ;;



let keys { data = t ; hash = f} =
    let a = ref []
    in for c = 0 to Array.length t -1 do
        match t.(c) with 
        |[]->()
        |(k,v)::r -> a:= k::(!a)
    done ; !a ;;


let items { data = t ; hash = f} = (* par construction il n'y a pas de doublons de v*)
    let a = ref []
    in let c = ref 0
    in while !c < Array.length t -1 do
        match t.(!c) with 
        |[]-> c:= !c+1
        |(k,v)::r -> a:= (k,v) :: !a
    done ; !a ;;






























































