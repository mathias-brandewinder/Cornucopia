namespace Archipendulum.Cornucopia

type Weight =
    private | W of int
    with
    static member create w =
        if w <= 0
        then failwith $"Invalid weight {w}: pick weight must be positive."
        else W w
    member this.Value =
        this
        |> function | W w -> w

type Distribution<'T> =
    | Uniform of list<'T>
    | Weighted of list<Weight * 'T>
    with
    member this.Length =
        match this with
        | Uniform xs -> xs.Length
        | Weighted xs -> xs.Length

module Pick =

    open System

    let weight (i: int) = Weight.create i

    let uniform (rng: Random) (uniform: List<_>) =
        let i = rng.Next(0, uniform.Length)
        uniform.[i]

    let weighted (rng: Random) (weighted: list<Weight * _>) =
        let total =
            weighted
            |> List.sumBy (fun (w, _) -> w.Value)
        let roll = rng.Next (0, total) + 1
        let rec search acc (entries: list<Weight * _>) =
            match entries with
            | [] -> failwith "no entry to pick from table"
            | (weight, value) :: tl ->
                let acc = acc + weight.Value
                if acc >= roll
                then value
                else search acc tl
        search 0 weighted

    let from (rng: Random) (distribution: Distribution<_>) =
        match distribution with
        | Uniform distribution -> uniform rng distribution
        | Weighted distribution -> weighted rng distribution
