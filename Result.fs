// code taken from: https://github.com/swlaschin/13-ways-of-looking-at-a-turtle/blob/master/Common.fsx
// with only slight modifications

module FSharp.Core.Extensions.Result

open System

let returnR x = 
    Ok x

// infix version of bind
let ( >>= ) xR f = 
    Result.bind f xR

// infix version of map
let ( <!> ) = Result.map 

let apply fR xR = 
    fR >>= (fun f ->
    xR >>= (fun x ->
        returnR (f x) ))

// infix version of apply
let ( <*> ) = apply

// lift a one-parameter function to result world (same as mapR)
let lift f x = f <!> x 

// lift a two-parameter function to result world
let lift2 f x y = f <!> x <*> y

/// Computation Expression
type ResultBuilder() =
    member this.Bind(m:Result<'a,'error>,f:'a -> Result<'b,'error>) = 
        Result.bind f m
    member this.Return(x) :Result<'a,'error> = 
        returnR x
    member this.ReturnFrom(m) :Result<'a,'error> = 
        m
    member this.Zero() :Result<unit,'error> = 
        this.Return ()
    member this.Combine(m1, f) = 
        this.Bind(m1, f)
    member this.Delay(f) = 
        f
    member this.Run(m) = 
        m()
    member this.TryWith(m:Result<'a,'error>, h: exn -> Result<'a,'error>) =
        try this.ReturnFrom(m)
        with e -> h e
    member this.TryFinally(m:Result<'a,'error>, compensation) =
        try this.ReturnFrom(m)
        finally compensation()
    member this.Using(res:#IDisposable, body) : Result<'b,'error> =
        this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
    member this.While(cond, m) =
        if not (cond()) then 
            this.Zero()
        else
            this.Bind(m(), fun _ -> this.While(cond, m))
    member this.For(sequence:seq<_>, body) =
        this.Using(sequence.GetEnumerator(),
            (fun enum -> this.While(enum.MoveNext, fun _ -> body enum.Current)))
        
let result = ResultBuilder()