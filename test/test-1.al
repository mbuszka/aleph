eff RD =
  ask : Unit -> Int;

let f = fn x -> x

run print (f 7)

let h = fn comp -> fn r -> 
  (handle RD in comp () with
    ask u, k -> k r;
    return x -> x;)

let x = h (fn u ->
  a <- ask (),
  a) 7

let y = h (fn u ->
  a <- h (fn u -> lift RD in (ask ())) 5,
  b <- ask (),
  a) 6

let z = fn u -> lift RD in (ask ())

let v = h (fn u -> lift RD in (ask ()))