namespace Archipendulum.Cornucopia.Tables

open System
open Archipendulum.Cornucopia

type TableRef = | Ref of string

type Entry<'T> =
    | Item of 'T
    | And of list<Entry<'T>>
    | Or of list<Entry<'T>>
    | Table of TableRef
    | Weighted of list<Weight * Entry<'T>>
    | Repeat of int * Entry<'T>
    | Merge of (Entry<'T> * Entry<'T>)

type NamedTable<'T> = {
    Name: string
    Entries: Distribution<Entry<'T>>
    }

type Context<'T> = {
    RNG: Random
    Tables: Map<TableRef, NamedTable<'T>>
    Merge: 'T * 'T -> 'T
    }

[<RequireQualifiedAccess>]
module Table =

    let eval ctx table =
        let rec eval (ctx: Context<'T>) (entry: Entry<'T>) =
            match entry with
            | Item item -> List.singleton item
            | And entries -> entries |> List.collect (eval ctx)
            | Or entries ->
                match entries with
                | [] -> []
                | _ ->
                    entries |> Pick.uniform ctx.RNG |> eval ctx
            | Weighted entries ->
                match entries with
                | [] -> []
                | _ ->
                    entries |> Pick.weighted ctx.RNG |> eval ctx
            | Table ref ->
                ctx.Tables.[ref].Entries |> Pick.from ctx.RNG |> eval ctx
            | Repeat (n, entry) ->
                List.init n (fun _ -> entry |> eval ctx)
                |> List.collect id
            | Merge (entry1, entry2) ->
                eval ctx entry1
                |> List.collect (fun value1 ->
                    eval ctx entry2
                    |> List.map (fun value2 ->
                        ctx.Merge (value1, value2)
                        )
                    )
            |> List.distinct
        eval ctx (Table (Ref table))