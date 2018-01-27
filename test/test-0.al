
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
          lift RD in (handle RD in 
            r ()
          with
            ask u, r -> r x;
            return y -> y;);
      return y -> y;
  with
    ask u, r -> r s;
    return y -> y;

run print (runState 6 (fn u ->
  a <- ask (),
  u <- print a,
  u <- tell (add 1 a),
  b <- ask (),
  u <- print b,
  (add b 1)))

run 
  let c = fn u ->
    a <- ask (),
    u <- print a,
    b <- lift RD in (ask ()),
    u <- print b,
    u <- tell (add a 2),
    u <- print (ask ()),
    1
  in handle RD in runState 9 c with
    ask u, r -> r 10;
    return x -> x;
      

