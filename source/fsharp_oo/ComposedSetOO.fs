namespace ComposedSet.FSharp
open Common

module ComposedSetOO =
    type Indices = int list
    
    type IComposedSetDatabase<'T> =
        abstract member Compose     : Indices -> 'T
        abstract member Decompose   : 'T       -> Indices

    [<AbstractClass>]
    type ComposedSetDatabase<'T when 'T : comparison>() as this =
        let partToIndex                   = new System.Collections.Generic.Dictionary<'T, int>()
        let composedToIndices             = new System.Collections.Generic.Dictionary<'T, Indices>();
        member val parts : ResizeArray<'T>= new ResizeArray<'T>()

        abstract member Compose           : Indices  -> 'T
        abstract member Split             : 'T       -> 'T array
    
        member this.Decompose composed = 
            let ok, cachedIndices = composedToIndices.TryGetValue composed
            match ok with
            | true -> cachedIndices 
            | false ->
                let indices = [
                    for part in this.Split composed do 
                        let ok, cachedIndex = partToIndex.TryGetValue part
                        match ok with
                        | true  -> yield cachedIndex
                        | false -> 
                            this.parts.Add part
                            let newIndex = this.parts.Count - 1
                            partToIndex.Add(part, newIndex)
                            yield newIndex
                ]
                composedToIndices.Add(composed, indices)
                indices

        interface IComposedSetDatabase<'T> with
            member x.Compose indices   = this.Compose indices
            member x.Decompose composed = this.Decompose composed
            

    type ComposedSet<'T, 'TDB when 'TDB :> ComposedSetDatabase<'T> and 'T : comparison and 'TDB :(new : unit -> 'TDB)>(in_indices : Indices) =
        // Static
        static let database = new 'TDB()
        static let empty    = new ComposedSet<'T, 'TDB>()
    
        let calchash indices = List.fold (fun h x -> h * 7 + x) 13 indices
        let hash = calchash in_indices
     
        // Constructors
        new()               = ComposedSet([] : Indices)
        new(composed : 'T)  = ComposedSet(database.Decompose(composed) : Indices)

        // Instance
        member this.indices : Indices = in_indices
    
        member this.GetIndicesAsString = "[" + (this.indices |> List.map (fun i -> i.ToString()) |> String.concat ", ") + "]"
        member this.GetParts = (database.parts.ToArray() |> Array.map (fun s -> s.ToString()) |> String.concat "|") + "\n[Count:" + database.parts.Count.ToString() + "]"
        member this.Compose = database.Compose(this.indices)
        override this.Equals other =
            match other with
            | :? ComposedSet<'T,'TDB> as other -> 
                match this.GetHashCode() = other.GetHashCode() with
                | true  -> List.forall2 (=) this.indices other.indices
                | false -> false
            | _ -> false // type mis-match

        override this.GetHashCode() = hash

        static member (+) (left : ComposedSet<'T, 'TDB>, right : ComposedSet<'T, 'TDB>) =
            new ComposedSet<'T, 'TDB>( left.indices @ right.indices )

        static member IsNullOrEmpty(cset : ComposedSet<'T,'TDB>) =
            cset.indices.Length = 0

        member this.EndsWith (other : ComposedSet<'T,'TDB>) =
            List.endsWith this.indices other.indices

        member this.StartsWith (other : ComposedSet<'T,'TDB>) =
            List.startsWith this.indices other.indices

        member this.TrimEnd (other : ComposedSet<'T,'TDB>) =
            match this.EndsWith other with
            | true  -> new ComposedSet<'T,'TDB>(List.sub this.indices 0 (this.indices.Length - other.indices.Length))
            | false -> this
        
    open System.Text.RegularExpressions
    type StringComposedSetDatabase() =
        inherit ComposedSetDatabase<string>()

        override this.Compose indices = 
            indices |> List.map (fun i -> this.parts.[i]) |> String.concat ""
            
        override this.Split composed = 
            let regex = @"("")|(\])|(\[)|(\()|(\))|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)"
            Regex.Split(composed, regex, RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))