eff RD =
  ask : Unit -> Int;

eff EX =
  throw : Int -> Unit;

let f = fn x -> x

run print (f 2)

let h = fn comp -> fn r -> 
  (handle RD in comp () with
    ask u, k -> k r;
    return x -> x;)

let x = h (fn u ->
  a <- ask (),
  a) 7

run print x

run handle EX in 
  u <- print 5,
  u <- throw 9,
  print 42 
    with
      throw n, k -> print n;
      return x -> ();

run handle IO in print 17 with
  print x, k -> k (print 8);
  return x   -> x;

let y = h (fn u ->
  a <- h (fn u -> lift RD in (ask ())) 5,
  b <- ask (),
  a) 6

run print y

let z = fn u -> lift RD in (ask ())

let v = h (fn u -> lift RD in (ask ()))

let z = h (fn u -> v 3) 4

