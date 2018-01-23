
eff RD =
  ask : Unit => Int;

eff WR =
  tell : Int => Unit;

run print 1

run print (add 1 1)

run let x = 2 in print (add x 1)

run let f = fn x -> sub 5 x in print (f 1)

run let f = fn g -> g 5 in f print 

let runState = fn s -> fn c ->
  handle RD in
    handle WR in
      c ()
    with
      tell x, r -> 
          (handle RD in 
            r ()
          with
            ask u, r -> r x;
            return y -> y;);
      return y -> y;
  with
    ask u, r -> r s;
    return y -> y;

run runState 7 (fn u ->
  a <- ask (),
  u <- print a,
  u <- tell (add 1 a),
  b <- ask (),
  u <- print b,
  b)

      

