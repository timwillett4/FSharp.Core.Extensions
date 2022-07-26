module FSharp.Core.Extensions.FSharpType

open Microsoft.FSharp.Reflection

/// <summary>
/// <remarks>This method currently only works for unions where all cases have no data</remarks>
/// </summary>
let getAllUnionCases<'T>() =
    [for case in FSharpType.GetUnionCases(typeof<'T>) -> FSharpValue.MakeUnion(case, [||]) |> unbox<'T>]

let getAllEnumsValues<'T>() =
    [for value in System.Enum.GetValues(typeof<'T>) -> value |> unbox<'T>]