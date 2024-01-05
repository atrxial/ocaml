let partitionne l x = match l with
  |[]->([],[],[])
  |u::r-> match partitionne r x with
    |(a,b,c)-> if u<x then (u::a,b,c) else if u = x then (a,bu::,c) else(a,b,u::c) ;;