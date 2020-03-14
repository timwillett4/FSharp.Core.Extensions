// code taken from here: https://theburningmonk.com/2012/10/f-helper-functions-to-convert-between-asyncunit-and-task/

[<AutoOpen>]
module FSharp.Core.Extensions.Async

open System.Threading.Tasks

let StartAsyncTAsTask (async : Async<_>) = Task.Factory.StartNew(fun () -> async |> Async.RunSynchronously)
let StartAsyncAsTask (async : Async<unit>) = Task.Factory.StartNew(fun () -> async |> Async.RunSynchronously)

let AwaitTask (task: Task) = 
    // rethrow exception from preceding task if it faulted
    let continuation (t : Task) =
        match t.IsFaulted with
        | true -> raise t.Exception
        | false -> ()
    task.ContinueWith continuation |> Async.AwaitTask

let AwaitTaskT (task: Task<_>) = 
    // rethrow exception from preceding task if it faulted
    let continuation (t : Task<_>) =
        match t.IsFaulted with
        | true -> raise t.Exception
        | false -> t.Result
    task.ContinueWith continuation |> Async.AwaitTask

