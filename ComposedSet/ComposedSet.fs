namespace ComposedSet.FSharp

type Indicies = int list

type IComposedSetDatabase<'T> =
    abstract member Compose     : Indicies -> 'T
    abstract member Decompose   : 'T       -> Indicies

[<AbstractClass>]
type ComposedSetDatabase<'T when 'T : comparison>() as this =
    let partToIndex                   = new System.Collections.Generic.Dictionary<'T, int>()
    let composedToIndicies            = new System.Collections.Generic.Dictionary<'T, Indicies>();
    member val parts                  : ResizeArray<'T> = new ResizeArray<'T>()

    abstract member Compose           : Indicies  -> 'T
    abstract member Split             : 'T        -> 'T array  
    
    member this.Decompose composed = 
        let ok, cachedIndicies = composedToIndicies.TryGetValue composed
        match ok with
        | true -> cachedIndicies 
        | false ->
            let indicies = [
                for part in this.Split composed do 
                    let ok, cachedIndex = partToIndex.TryGetValue part
                    match ok with
                    | true -> yield cachedIndex
                    | false -> 
                        this.parts.Add part
                        let newIndex = (this.parts |> Seq.length) - 1
                        yield newIndex
                        partToIndex.Add(part, newIndex)                    
            ]
            composedToIndicies.Add(composed, indicies)
            indicies

    interface IComposedSetDatabase<'T> with
        member x.Compose indicies   = this.Compose indicies
        member x.Decompose composed = this.Decompose composed
            


type ComposedSet<'T, 'TDB when 'TDB :> ComposedSetDatabase<'T> and 'T : comparison and 'TDB :(new : unit -> 'TDB)>(in_indicies : Indicies) =

    let startWith (xs: 't list) (ys: 't list) =
        match (xs.Length, ys.Length) with
            | (_,0) -> false
            | (xl,yl) when xl < yl -> false
            | (_,_) ->
                let rec startWithRec (xs, ys) =
                    match (xs, ys) with
                    | ([],[]) -> true   // equals
                    | (_ ,[]) -> true   // ends with
                    | ([], _) -> false  // never happens
                    | (x::xs, y::ys) ->
                        match x = y with
                            | false -> false
                            | true -> startWithRec (xs, ys)
                startWithRec (xs, ys)

    let subList (xs: 't list) (startIndex: int) (count: int) =
        let rec sub xs c i acc = 
            match (c,i) with
            | (c,_) when c >= count         -> List.rev acc
            | (_,i) when i <  startIndex    -> sub xs c (i+1) acc
            | (_,i) when i >= startIndex    -> 
                match xs with
                | []    -> List.rev acc
                | x::xs -> sub xs (c+1) (i+1) (x::acc)
        sub xs 0 0 []

    // Static
    static let database = new 'TDB()
    static let empty    = new ComposedSet<'T, 'TDB>()
     
    // Constructors
    new()               = ComposedSet([] : Indicies)
    new(composed : 'T)  = ComposedSet(database.Decompose(composed) : Indicies)

    // Instance
    member this.indicies : Indicies = in_indicies
    member this.GetIndiciesAsString = "[" + (this.indicies |> List.map (fun i -> i.ToString()) |> String.concat ", ") + "]"
    member this.Compose = database.Compose(this.indicies)
    override this.Equals other =
        match other with
        | :? ComposedSet<'T,'TDB> as other -> 
            if (this.indicies.Length <> other.indicies.Length) then false 
            else 
                seq { for i in 0 .. this.indicies.Length - 1 do
                        if this.indicies.[i] <> other.indicies.[i] then yield false }
                |> Seq.forall id
        | _ -> false

    override this.GetHashCode() =
        let mutable hash = 13
        for i in 0..this.indicies.Length-1 do
            hash <- (hash * 7) + this.indicies.[i]
        hash

    static member (+) (left : ComposedSet<'T, 'TDB>, right : ComposedSet<'T, 'TDB>) =
        new ComposedSet<'T, 'TDB>( List.append left.indicies right.indicies )

    member this.EndsWith (other : ComposedSet<'T,'TDB>) =
        startWith (List.rev this.indicies) (List.rev other.indicies) 

    member this.StartsWith (other : ComposedSet<'T,'TDB>) =
        startWith this.indicies other.indicies

    member this.TrimEnd (other : ComposedSet<'T,'TDB>) =
        if this.EndsWith other then
            let subList2 lst startIndex count =
                Array.sub (List.toArray lst) startIndex count |> List.ofArray

            new ComposedSet<'T,'TDB>(subList2 this.indicies 0 (this.indicies.Length - other.indicies.Length))
        else
            this