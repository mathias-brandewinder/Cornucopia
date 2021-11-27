namespace Archipendulum.Cornucopia

open System

/// Set of items, with or without weights
type Distribution<'T> =
    | Flat of 'T []
    | Weighted of (float * 'T) []

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

    /// Pick a random element directly from a source distribution.
    member this.FromSource (source: Distribution<'T>) =
        match source with
        | Flat source -> this.Uniform source
        | Weighted source -> this.Weighted source

type Name = | Name of string

type Context = {
    Sources: Map<Name, Distribution<string>>
    }

type Attribute = {
    Tag: string
    Value: string
    }

type Definition<'T> =
    | Source of Name
    | Def of Name * Definition<'T>
    | Or of list<float * Definition<'T>>
    | Maybe of float * Definition<'T>

type Select (context: Context, rng: Random) =

    let picker = Picker rng

    member this.OneOf (def: Definition<'T>) =
        match def with
        | Source name ->
            context.Sources
            |> Map.tryFind name
            |> Option.bind (picker.FromSource)
        | Def (name, definition) ->
            this.OneOf definition
        | Or choices ->
            picker.Weighted choices
            |> Option.bind this.OneOf
        | Maybe (_, choice) ->
            this.OneOf choice

    member this.MaybeOne (def: Definition<'T>) =
        match def with
        | Source name ->
            context.Sources
            |> Map.tryFind name
            |> Option.bind (picker.FromSource)
        | Def (_, definition) ->
            this.MaybeOne definition
        | Or choices ->
            picker.Weighted choices
            |> Option.bind this.MaybeOne
        | Maybe (proba, choice) ->
            if picker.Roll () <= proba
            then None
            else
                this.OneOf choice