namespace ComposedSet.FSharp

type IComposedSetDatabase<'T> =
    abstract member Compose     : int array -> 'T
    abstract member Decompose   : 'T        -> int array

[<AbstractClass>]
type ComposedSetDatabase<'T when 'T : comparison>() as this =
    let partToIndex                   = new System.Collections.Generic.Dictionary<'T, int>()
    let composedToIndicies            = new System.Collections.Generic.Dictionary<'T, int array>();
    member val parts                  : ResizeArray<'T> = new ResizeArray<'T>()

    abstract member Compose           : int array -> 'T
    abstract member Split             : 'T        -> 'T array  
    
    member this.Decompose composed = 
        let ok, cachedIndicies = composedToIndicies.TryGetValue composed
        match ok with
        | true -> cachedIndicies 
        | false ->
            let indicies = [|
                for part in this.Split composed do 
                    let ok, cachedIndex = partToIndex.TryGetValue part
                    match ok with
                    | true -> yield cachedIndex
                    | false -> 
                        this.parts.Add part
                        let newIndex = (this.parts |> Seq.length) - 1
                        yield newIndex
                        partToIndex.Add(part, newIndex)                    
            |]
            composedToIndicies.Add(composed, indicies)
            indicies

    interface IComposedSetDatabase<'T> with
        member x.Compose indicies   = this.Compose indicies
        member x.Decompose composed = this.Decompose composed
            


type ComposedSet<'T, 'TDB when 'TDB :> ComposedSetDatabase<'T> and 'T : comparison and 'TDB :(new : unit -> 'TDB)>(in_indicies : int array) =

    // Static
    static let database = new 'TDB()
    static let empty    = new ComposedSet<'T, 'TDB>()
     
    // Constructors
    new()               = ComposedSet([||] : int array)
    new(composed : 'T)  = ComposedSet(database.Decompose(composed) : int array)

    // Instance
    member this.indicies : int array = in_indicies
    member this.GetIndiciesAsString = "[" + (this.indicies |> Array.map (fun i -> i.ToString()) |> String.concat ", ") + "]"
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

    member this.EndsWith (other : ComposedSet<'T,'TDB>) =
        let tlen = this.indicies.Length
        let olen = other.indicies.Length
        if (olen = 0 || olen > tlen) then false
        else
            seq { for i in 0 .. other.indicies.Length - 1 do
                    if other.indicies.[olen - i - 1] <> this.indicies.[tlen - i - 1] then yield false }
            |> Seq.forall id

    member this.StartsWith (other : ComposedSet<'T,'TDB>) =
        let tlen = this.indicies.Length
        let olen = other.indicies.Length
        if (olen = 0 || olen > tlen) then false
        else
            seq { for i in 0 .. other.indicies.Length - 1 do
                    if other.indicies.[i] <> this.indicies.[i] then yield false }
            |> Seq.forall id

    member this.TrimEnd (other : ComposedSet<'T,'TDB>) =
        if this.EndsWith other then
            new ComposedSet<'T,'TDB>(Array.sub this.indicies 0 (this.indicies.Length - other.indicies.Length))
        else
            this

 
