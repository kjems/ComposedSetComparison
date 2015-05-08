namespace ComposedSet.FSharpIdomatic
open Common
type Indices = int list

module ComposedSetDatabase =
    open System.Collections.Generic      
    
    let parts<'a> :                                      ResizeArray<'a>          = new ResizeArray<'a>()
    let partToIndex<'a when 'a : equality  > :           Dictionary<'a, int>      = new Dictionary<'a, int>()    
    let composedToIndices<'a when 'a : equality  > :     Dictionary<'a, Indices>  = new Dictionary<'a, Indices>()

    let decompose split composed =
        let ok, cachedIndices = composedToIndices.TryGetValue composed
        match ok with
        | true -> cachedIndices 
        | false ->
            let indices = [
                for part in split composed do 
                    let ok, cachedIndex = partToIndex.TryGetValue part
                    match ok with
                    | true -> yield cachedIndex
                    | false -> 
                        parts.Add part
                        let newIndex = (parts |> Seq.length) - 1
                        yield newIndex
                        partToIndex.Add(part, newIndex)                    
            ]
            composedToIndices.Add(composed, indices)
            indices
        
module ComposedSet =
    let calchash      = List.fold (fun h x -> h * 7 + x) 13
    let isempty       = List.isEmpty
    let startswith    = List.startsWith
    let endswith      = List.endsWith
    let equals xs ys  = if calchash xs = calchash ys then List.forall2 (fun x y -> x = y) xs ys else false
    let trimend xs ys = if endswith xs ys then List.sub xs 0 (xs.Length - ys.Length) else xs

module ComposedSetDatabaseOfStrings =
    open System.Text.RegularExpressions
    let regex               = @"("")|(\])|(\[)|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)"
    let split composed      = Regex.Split(composed, regex, RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))
    let decompose composed  = ComposedSetDatabase.decompose split composed
    let compose indices     = indices |> Seq.map (fun i -> ComposedSetDatabase.parts.[i]) |> String.concat ""



