namespace Archipendulum.Cornucopia

open System

/// Some list of items, with or without weights
type Distribution<'T> =
    | Flat of 'T []
    | Weighted of (float * 'T) []

type Name = | Name of string

type Expr =
    | Source of Name
    | Or of list<float * Expr>
    | Maybe of (float * Expr)

type Context = {
    Sources: Map<Name, Distribution<string>>
    }

type Picker (rng: Random) =

    member this.Roll () =
        rng.NextDouble ()

    member this.Uniform<'T> (distribution: 'T []) =
        if distribution |> Array.isEmpty
        then
            // degenerate data: we have nothing to pick from
            None
        else
            let rollIndex = rng.Next(0, distribution.Length)
            distribution.[rollIndex]
            |> Some

    member this.Weighted<'T> (distribution: (float * 'T)[]) =
        if distribution |> Array.isEmpty
        then
            // degenerate data: we have nothing to pick from
            None
        else
            let total =
                distribution
                |> Seq.sumBy fst
            let roll = total * rng.NextDouble ()
            let rec search (index, acc) =
                let weight = distribution.[index] |> fst
                let acc = acc + weight
                if acc >= roll
                then distribution.[index] |> snd
                else search (index + 1, acc)
            search (0, 0.0)
            |> Some

    member this.Weighted<'T> (distribution: list<float * 'T>) =
        match distribution with
        | [] ->
            // degenerate data: we have nothing to pick from
            None
        | distribution ->
            let total =
                distribution
                |> Seq.sumBy fst
            let roll = total * rng.NextDouble ()
            let rec search acc rest =
                match rest with
                | [] -> None
                | (weight, item) :: rest ->
                    let acc = acc + weight
                    if acc >= roll
                    then Some item
                    else search acc rest
            search 0.0 distribution

module Pick =

    let picker = Random() |> Picker

    /// Pick a random element directly from a source distribution.
    let fromSource (source: Distribution<'T>) =
        match source with
        | Flat source -> picker.Uniform source
        | Weighted source -> picker.Weighted source

    let rec from (context: Context) (exp: Expr) =

        match exp with
        | Source name ->
            context.Sources
            |> Map.tryFind name
            |> Option.bind fromSource
        | Or choices ->
            choices
            |> picker.Weighted
            |> Option.bind (from context)
        | Maybe (proba, expr) ->
            if picker.Roll () <= proba
            then None
            else from context expr