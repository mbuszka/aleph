def id: forall 'a. () -> 'a () = fun 'a. fun x: (). x

def f: forall 'a. Int -> 'a Int = fun 'a. fun x: Int. x

run let u1 = id in 42