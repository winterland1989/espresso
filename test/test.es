let foo = 3
let foo2 = f 3

let bar = \ -> 4

let bar2 = go (\ -> 4)

let bar3 = (\ -> 4) 2

let fooBar = \ -> foo + bar

let fooBarBar = \ -> 
    foo + bar


let fooBarBar = \ -> 
    let x = 2
    let y = 3

let [a,b,c] = [1,2,3]


let test = \ a, {b, c = 3}, [d, {i, e: {f: g}}] -> a + b + c + 
                                                    d

let test2 = \ {
        a
    ,   b
    ,   c
    } ->
        a + b + c


{   k1: 2
,   k2: 3
,   k3:
    {   k4: 4
    ,   k5: {}
    }   
}

let test2 = \ {
        a
    ,   b
    ,   c
    } ->
        a + b + c 3

let test2 = \ {
        a
    ,   b
    ,   c
    } ->
        a + b + c

            {   k1: 2
            ,   k2: 3
            ,   k3:
                {   k4: 4
                ,   k5: {}
                }   
            }

let test3 = \ -> \ -> \ -> f 1

let test3 = \ -> go <| \ -> \ -> 1
