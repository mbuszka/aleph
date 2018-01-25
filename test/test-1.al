eff RD =
  ask : Unit => Int;

eff EX =
  throw : Int => Unit;

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

run if false then print 17 else print 14 end

run print (add 3 4)

run if isZero (sub 3 4) then print 0 else print 1 end

let test = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))

run u <- print 3,
  letrec fun x ->
    u <- print x,
    if isZero x
      then ()
      else fun (sub x 1) end
  in fun 5


let map = fn f ->
  letrec aux l ->
    if null l 
      then nil
      else
        h <- head l,
        t <- tail l,
        fh <- f h,
        ft <- aux t,
        cons fh ft
      end
  in aux


let printInt = fn x ->
  u <- print x,
  1

let someFun = fn x -> add x 2

run 
    l <- map someFun test,
    map printInt l

let f = fn x ->
    r <- ask (),
    mul x r

let c = fn u -> map f test

run 
  let l =
    handle RD in c () with
      ask u, r -> r 5;
      return x -> x;
  in map printInt l

{-
run
  l <- h c 2,
  map printInt l
-}