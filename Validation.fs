module FSharp.Core.Extensions.Validation

type ValidationRule<'T> = 'T -> bool * string

let buildValidator (rulesList : ValidationRule<'T> list) =
    rulesList |> List.reduce (fun firstRule secondRule ->
        fun objToValidate -> 
            match (objToValidate |> firstRule) with
            | true,_ ->
                match objToValidate |> secondRule with
                | true,_ -> true, ""
                | false,error -> false, error
            | false, error -> false, error 
        )
