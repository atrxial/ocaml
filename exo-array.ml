let appartient x t =
  let test = ref false in
  let c = ref 0 in
  while !c< Array.length t && not(!test) do
    test:= t.(!c)=x ; c:=!c+1
  done ; !test ;;

let croissant t =
  let c = ref 0 in
  while !c< Array.length t -1 && t.(!c)<=t.(!c+1) do
    c:=!c+1
  done ; !c=Array.length t -1 ;;

let est_palindrome t =
  let c = ref 0 in
  let n = Array.length t in let m = n/2 in
  while !c<m && t.(!c)=t.(n-1-(!c)) do
    c:=!c+1
  done ; !c=m ;;

let swap t i j = let a = t.(i) in t.(i)<-t.(j) ; t.(j)<- a ;;

let renverse t =
  let n = Array.length t -1 in
  for c=0 to n/2 do
    swap t c (n-c)
  done ;;

let occurences x t = 
  let l = ref [] in 
  for c= 0 to Array.length t -1 do 
    if t.(c)= x then l:= c::(!l)
  done ; !l ;;

let indices_mini t =
  let l= ref [] in
  let m = ref t.(0) in
  for c =1 to Array.length t -1 do
    if t.(c)<(!m) then (m:=t.(c) ; l:=[c]) else if t.(c)=(!m) then l:=c::(!l) 
  done ; !l ;;

let filtre pred t = 
  let l=ref [] in 
  for c=0 to Array.length t -1 do
    if pred t.(c) then l:=t.(c)::(!l)
  done ; !l ;;



let cherche_somme t s =
  let test = ref None in 
  let n = Array.length t in
  let i=ref 0 in
  while !i<n && !test = None do
    ( let j = ref !i in
    while !j<n && !test = None do
      if t.(!j)+t.(!i)=s then test:= Some (!i,!j) ; j:=!j+1 done) ;
    i:=!i+1
  done ; !test ;;

let cherche_somme_tri t s =
  let test = ref None in 
  let n = Array.length t in
  let i=ref 0 in
  while !i<n && t.(!i)<=s && !test = None do
    (let j = ref (!i) in
    while !j<n && t.(!j)<=s-t.(!i) && !test = None do
      if t.(!j)+t.(!i)=s then test:= Some (!i,!j) ; j:=!j+1 done) ;
    i:=!i+1
  done ; !test ;;













let est_equilibre t a b =
  let v = ref 0 in let f = ref 0 in
  for c = a to b-1 do
    if t.(c) then v:=!v+1 else f:=!f+1
  done ; !v=(!f) ;;

let max_equilibre t =
  let l = ref 0 in
  for a= 0 to Array.length t -1 do
    for b = 0 to Array.length t do
      if est_equilibre t a b then if b-a>(!l) then l:=b-a 
    done
  done ; !l ;;

let max_depuis t a =
  let l = ref 0 in let v = ref 0 in let f = ref 0 in
  for b=a to Array.length t-1 do
    if t.(b) then v:=!v+1 else f:=!f+1 ; if !v-(!f)>(!l) then l:= b-a 
  done ; !l ;;



let max_equilibre_eff t = 
  let l = ref 0 in
  for a = 0 to Array.length t-1 do
    let mdp=max_depuis t a in if mdp>(!l) then l:=mdp
  done ; !l ;;





let max_equilibre_ultraeff t = (* on trouve l tel qu'on maximise et tel qu'on minimise et bim on fait la différence en longueur *)
  let lmax = ref 0 in
  let lmin =  ref 0 in
  let v= ref 0 in
  let f = ref 0 in
  for x = 0 to Array.length t -1 do
    if t.(x) then v:=!v+1 else f:=!f+1 ; let a = !v-(!f) in if a<(!lmin) then lmin:=a else if a>(!lmax) then  lmax:=a
  done ; (!lmax -(!lmin)) ;;