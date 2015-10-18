module ComposedSet.FSharpIdiomatic.ComposedSetIdiomatic
    open Common
    type Indices = int list
    type Decomposed<'T> = private {indices : Indices; hash : int}
                 
    let composer (parts : ResizeArray<'T>) (stitch : 'T list -> 'T) =  
        fun decomposed -> decomposed.indices |> List.map (fun i -> parts.[i]) |> stitch
                
    let decomposer (parts : ResizeArray<'T>) (split : 'T -> 'T array) =
        let partIndex = Perf.memoize (fun p -> parts.Add(p); parts.Count - 1)
        Perf.memoize (fun composed ->
            let indices' = [for part in split composed do yield partIndex part]
            {indices = indices'; hash = List.calchash indices'})

    let isempty    cs    = List.isEmpty    cs.indices
    let calchash   cs    = List.calchash   cs.indices    
    let startswith xs ys = List.startsWith xs.indices ys.indices
    let endswith   xs ys = List.endsWith   xs.indices ys.indices
    let equals     xs ys = if xs.hash = ys.hash then List.forall2 (fun x y -> x = y) xs.indices ys.indices else false    
    let concat     xs ys = 
        let indices' = List.append xs.indices ys.indices
        {indices = indices'; hash = List.calchash indices'}
    let (++)             = concat
    let trimend    xs ys = 
        if endswith xs ys then 
            let indices' = List.sub xs.indices 0 (List.length xs.indices - List.length ys.indices)
            {indices = indices'; hash = List.calchash indices'}
        else xs