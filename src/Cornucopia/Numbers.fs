namespace Archipendulum.Cornucopia

/// n-sided dice, ex: d6
type D =
    | D of int
    with
    static member (*) (x: int, d: D) = Dice (x, d)
    static member (+) (x: int, d: D) = Add [ Int x; Dice (1, d) ]
    static member (+) (d: D, x: int) = Add [ Dice (1, d); Int x ]
    static member (+) (d1: D, d2: D) = Add [ Dice (1, d1); Dice (1, d2) ]
    static member (-) (d: D, x: int) = Add [ Dice (1, d); Int (- x) ]
    static member (-) (x: int, d: D) = Add [ Int x; Dice (- 1, d); ]
    static member (-) (d1: D, d2: D) = Add [ Dice (1, d1); Dice (- 1, d2) ]
/// Dice rolls, ex: 4d6+2
and Roll =
    | Int of int
    | Dice of int * D
    | Add of list<Roll>
    // | Best of (int * Roll)
    // | Worst of (int * Roll)
    with
    static member private neg (roll: Roll) =
        match roll with
        | Int x -> Int (- x)
        | Dice (x, d) -> Dice (- x, d)
        | Add rolls -> Add (rolls |> List.map Roll.neg)
    static member (+) (r1: Roll, r2: Roll) =
        match r1, r2 with
        | Add x1, Add x2 -> Add (x1 @ x2)
        | Add x1, x2 -> Add (x1 @ [ x2 ])
        | x1, Add x2 -> Add (x1 :: x2)
        | _ -> Add [ r1; r2 ]
    static member (+) (x: int, r: Roll) =
        match r with
        | Add xs -> Add (Int x :: xs)
        | _ -> Add [ Int x; r ]
    static member (+) (r: Roll, x: int) =
        match r with
        | Add xs -> Add (xs @ [ Int x ])
        | _ -> Add [ r; Int x ]
    static member (+) (r: Roll, d: D) =
        match r with
        | Add x -> Add (x @ [ Dice (1, d) ])
        | _ -> Add [ r; Dice (1, d) ]
    static member (+) (d: D, r: Roll) = Add [ Dice (1, d); r ]
    static member (-) (r: Roll, x: int) =
        r + Roll.neg (Int x)
    static member (-) (r: Roll, d: D) =
        r + Roll.neg (Dice (1, d))
    static member (-) (r1: Roll, r2: Roll) =
        r1 + Roll.neg r2