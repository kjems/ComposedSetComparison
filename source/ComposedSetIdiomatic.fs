namespace ComposedSet.FSharpIdiomatic
open Common
        
module ComposedSet =
    
    type Indices = Indices of int list
    let inline (!) (Indices i) = i
    type Decomposed<'a> = {indices : Indices; hash : int}
    type Decomposer<'a> = 'a             -> Decomposed<'a>
    type Composer<'a>   = Decomposed<'a> -> 'a
    
    module Database =     
        open System.Collections.Generic   

        // Internal mutable state
        let partToIndex         = Dictionary<'a, int>()        
        let parts               = ResizeArray<'a>()
        // Internal mutable memoization
        let composedToIndices   = Dictionary<'a, Indices>()

        let assembler =  fun decomposed -> !decomposed.indices |> List.map (fun i -> parts.[i])
        let decompose split : Decomposer<'a> =    
            fun composed ->
                let ok, cachedIndices = composedToIndices.TryGetValue composed
                match ok with
                | true -> {indices = cachedIndices; hash = List.calchash !cachedIndices}
                | false ->
                    let indices' = Indices ([
                        for part in split composed do 
                            let ok, cachedIndex = partToIndex.TryGetValue part
                            match ok with
                            | true  -> yield cachedIndex
                            | false -> 
                                parts.Add(part)
                                let newIndex = parts.Count - 1                        
                                partToIndex.Add(part, newIndex)                    
                                yield newIndex
                    ])
                    let decomposed = {indices = indices'; hash = List.calchash !indices'}
                    composedToIndices.Add(composed, indices')
                    decomposed


    let isempty    cs    = List.isEmpty    !cs.indices
    let calchash   cs    = List.calchash   !cs.indices    
    let startswith xs ys = List.startsWith !xs.indices !ys.indices
    let endswith   xs ys = List.endsWith   !xs.indices !ys.indices
    let equals     xs ys = if xs.hash = ys.hash then List.forall2 (fun x y -> x = y) !xs.indices !ys.indices else false    
    let concat     xs ys = 
        let indices' = List.append !xs.indices !ys.indices |> Indices
        {indices = indices'; hash = List.calchash !indices'}
    let (++)             = concat
    let trimend    xs ys = 
        if endswith xs ys then 
            let indices' = List.sub !xs.indices 0 (List.length !xs.indices - List.length !ys.indices) |> Indices 
            {indices = indices'; hash = List.calchash !indices'}
        else xs

module ComposedSetOfStrings =    
    open System.Text.RegularExpressions
    let regex                    = @"("")|(\])|(\[)|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)"
    let split composed           = Regex.Split(composed, regex, RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))
    let decompose                = ComposedSet.Database.decompose split
    let compose indices          = ComposedSet.Database.assembler indices |> String.concat ""
