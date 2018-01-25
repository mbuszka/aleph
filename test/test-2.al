eff ST =
  get : Unit => Int;
  put : Int  => Unit;

let handleState = fn c ->
  handle ST in c () with
    get u, r -> fn s -> (r s) s;
    put x, r -> fn s -> (r ()) x;
    return x -> fn s -> s;

let c = fn u ->
  u <- put 5,
  x <- get (),
  u <- print x,
  put (add x 2)

run print (handleState c 1)