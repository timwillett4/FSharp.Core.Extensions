module FSharp.Core.Extensions.FSharpType

open Microsoft.FSharp.Reflection

let getAllUnionCases<'T>() =
    [for case in FSharpType.GetUnionCases(typeof<'T>) -> FSharpValue.MakeUnion(case, [||]) |> unbox<'T>]

let getAllEnumsValues<'T>() =
    [for value in System.Enum.GetValues(typeof<'T>) -> value |> unbox<'T>]