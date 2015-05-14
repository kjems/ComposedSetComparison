namespace ComposedSet.FSharpIdiomatic
open Common

type Indices        = int list
type Decomposed<'a> = {indices : Indices; hash : int}
type Decomposer<'a> = 'a             -> Decomposed<'a>
type Composer<'a>   = Decomposed<'a> -> 'a

module ComposedSetDatabase =     
    open System.Collections.Generic   

    // Internal mutable cache
    let partToIndex         = Dictionary<'a, int>()    
    let composedToIndices   = Dictionary<'a, Decomposed<'a>>()
    let parts               = ResizeArray<'a>()

    let assembler = 
        fun decomposed -> decomposed.indices |> Seq.map (fun i -> parts.[i]) 

    let decompose split : Decomposer<'a> =    
        fun composed ->
            let ok, cachedIndices = composedToIndices.TryGetValue composed
            match ok with
            | true -> cachedIndices
            | false ->
                let indices' = [
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
                let cs = {indices = indices'; hash = List.calchash indices'}
                composedToIndices.Add(composed, cs)
                cs
        
module ComposedSet =
    let isempty    cs    = List.isEmpty    cs.indices
    let calchash   cs    = List.calchash   cs.indices    
    let startswith xs ys = List.startsWith xs.indices ys.indices
    let endswith   xs ys = List.endsWith   xs.indices ys.indices
    let equals     xs ys = if xs.hash = ys.hash then List.forall2 (fun x y -> x = y) xs.indices ys.indices else false    
    let concat     xs ys = 
        let indices' = xs.indices @ ys.indices
        {indices = indices'; hash = List.calchash indices'}
    let (++)             = concat
    let trimend    xs ys = 
        if endswith xs ys then 
            let indices' = List.sub xs.indices 0 (xs.indices.Length - ys.indices.Length) 
            {indices = indices'; hash = List.calchash indices'}
        else xs

module ComposedSetOfStrings =    
    open System.Text.RegularExpressions
    let regex                    = @"("")|(\])|(\[)|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)"
    let split composed           = Regex.Split(composed, regex, RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))
    let decompose                = ComposedSetDatabase.decompose split
    let compose indices          = ComposedSetDatabase.assembler indices |> String.concat ""
