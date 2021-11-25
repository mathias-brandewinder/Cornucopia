#load "Library.fs"
open Archipendulum.Cornucopia
open System

let source1 =
    [
        "Alpha"
        "Bravo"
        "Charlie"
        "Delta"
    ]
    |> Array.ofList

let source2 =
    [
        "A"
        "B"
        "C"
        "D"
    ]
    |> Array.ofList

let exp =
    Or [
        0.4, Source (Name "source 1")
        0.6, Maybe (0.5, Source (Name "source 2"))
        ]

let rng = Random ()

type Context = {
    Sources: Map<Name, Distribution<string>>
    }

let rec pick (context: Context) (exp: Expr) =
    let picker = Random () |> Picker
    match exp with
    | Source name ->
        context.Sources
        |> Map.tryFind name
        |> Option.bind (Pick.from rng)
    | Or choices ->
        choices
        |> picker.Weighted
        |> Option.bind (pick context)
    | Maybe (proba, expr) ->
        if rng.NextDouble () <= proba
        then None
        else pick context expr

let context = {
    Sources =
        [
            Name "source 1", Flat source1
            Name "source 2", Flat source2
        ]
        |> Map.ofList
    }

pick context exp
