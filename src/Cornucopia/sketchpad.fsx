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

let expr =
    Or [
        0.4, Source (Name "source 1")
        0.6, Maybe (0.5, Source (Name "source 2"))
        ]

expr
|> Pick.from context
