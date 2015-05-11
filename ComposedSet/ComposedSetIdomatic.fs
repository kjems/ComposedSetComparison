namespace ComposedSet.FSharpIdomatic
open Common

type Indices        = int list
type Composed<'a>   = Indices * ResizeArray<'a>
type Decomposer<'a> = ResizeArray<'a> -> 'a     -> Composed<'a>
type Composer<'a>   = Composed<'a>              -> 'a

module ComposedSetDatabase =     
    open System.Collections.Generic   
    let decompose split : Decomposer<'a> =    
        // Internal mutable cache        
        let partToIndex         = Dictionary<'a, int>()    
        let composedToIndices   = Dictionary<'a, Indices>()

        fun parts composed ->
            let ok, cachedIndices = composedToIndices.TryGetValue composed
            match ok with
            | true -> cachedIndices, parts
            | false ->
                let indices = [
                    for part in split composed do 
                        let ok, cachedIndex = partToIndex.TryGetValue part
                        match ok with
                        | true  -> yield cachedIndex
                        | false -> 
                            parts.Add(part)
                            let newIndex = parts.Count - 1                        
                            partToIndex.Add(part, newIndex)                    
                            yield newIndex
                ]
                composedToIndices.Add(composed, indices)
                indices, parts
        
module ComposedSet =
    let calchash      = List.fold (fun h x -> h * 7 + x) 13
    let isempty       = List.isEmpty
    let startswith    = List.startsWith
    let endswith      = List.endsWith
    let equals xs ys  = if calchash xs = calchash ys then List.forall2 (fun x y -> x = y) xs ys else false
    let trimend xs ys = if endswith xs ys then List.sub xs 0 (xs.Length - ys.Length) else xs

module ComposedSetDatabaseOfStrings =    
    open System.Text.RegularExpressions
    let regex                    = @"("")|(\])|(\[)|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)"
    let split composed           = Regex.Split(composed, regex, RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))
    let decompose                = ComposedSetDatabase.decompose split
    let compose (indices, parts : ResizeArray<string>) = indices |> Seq.map (fun i -> parts.[i]) |> String.concat ""

module Test =    
    let testit = 
        let parts = ResizeArray<string>()
        let dumpParts (composed : Composed<string>) = printfn "indices:[%s], parts:[%s] " (fst composed |> Seq.map (fun i -> i.ToString()) |> String.concat "|") (snd composed |> String.concat "|")
        let s1 = "Hello World, this is fun stuff"
        let s2 = "fun stuff with a World"
        let cs1 = ComposedSetDatabaseOfStrings.decompose parts s1        
        dumpParts cs1 |> ignore
        let cs2 = ComposedSetDatabaseOfStrings.decompose parts s2
        dumpParts cs2 |> ignore
        printfn "%s" (ComposedSetDatabaseOfStrings.compose cs1)
        printfn "%s" (ComposedSetDatabaseOfStrings.compose cs2)