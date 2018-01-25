eff ST =
  get : Unit => Int;
  put : Int  => Unit;

let facRec =
  letrec facRec n ->
    if isZero n then 1
    else mul n (facRec (sub n 1)) end
  in facRec

run print (facRec 5)

let handleState = fn c ->
  handle ST in c () with
    get u, r -> fn s -> (r s) s;
    put x, r -> fn s -> (r ()) x;
    return x -> fn s -> s;

let facIter = fn n ->
  letrec aux k ->
    if isZero k
      then get ()
      else 
        v <- get (),
        u <- put (mul v k),
        aux (sub k 1)
      end
  in handleState (fn u -> aux n) 1

run print (facIter 5)
