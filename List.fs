module FSharp.Core.Extensions.List

// code for traverse functions taken taken from https://fsharpforfunandprofit.com/posts/elevated-world-4/#traverse 
// with only slight modifications

/// Map a Result producing function over a list to get a new Result 
/// using applicative style
/// ('a -> Result<'b>) -> 'a list -> Result<'b list>
let rec traverseResultA f list =

    // define the applicative functions
    let (<*>) = Result.apply
    let retn = Result.Ok

    // define a "cons" function
    let cons head tail = head :: tail

    // loop through the list
    match list with
    | [] -> 
        // if empty, lift [] to a Result
        retn []
    | head::tail ->
        // otherwise lift the head to a Result using f
        // and cons it with the lifted version of the remaining list
        retn cons <*> (f head) <*> (traverseResultA f tail)


/// Map a Result producing function over a list to get a new Result 
/// using monadic style
/// ('a -> Result<'b>) -> 'a list -> Result<'b list>
let rec traverseResultM f list =

    // define the monadic functions
    let (>>=) x f = Result.bind f x
    let retn = Result.Ok

    // define a "cons" function
    let cons head tail = head :: tail

    // loop through the list
    match list with
    | [] -> 
        // if empty, lift [] to a Result
        retn []
    | head::tail ->
        // otherwise lift the head to a Result using f
        // then lift the tail to a Result using traverse
        // then cons the head and tail and return it
        f head                 >>= (fun h -> 
        traverseResultM f tail >>= (fun t ->
        retn (cons h t) ))

 // see https://stackoverflow.com/questions/9213761/cartesian-product-two-lists
let product xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

/// distinct returns a list containing elements in list1 that are not in list2
let distinct list1 list2 =
    list1 |> List.filter (fun x -> list2 |> List.contains x = false)
        
    