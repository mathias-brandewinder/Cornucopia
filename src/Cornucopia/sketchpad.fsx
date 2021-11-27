#load "Core.fs"
open Archipendulum.Cornucopia

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

let context = {
    Sources =
        [
            Name "source 1", Flat source1
            Name "source 2", Flat source2
        ]
        |> Map.ofList
    }

let def =
    Or [
        0.4, Source (Name "source 1")
        0.6, Maybe (0.5, Source (Name "source 2"))
        ]

let picker = Select(context, System.Random())

def
|> picker.OneOf

def
|> picker.MaybeOne