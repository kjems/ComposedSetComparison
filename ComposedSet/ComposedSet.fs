namespace ComposedSet.FSharp

type IComposedSetDatabase<'T> =
    abstract member Compose     : int array -> 'T
    abstract member Decompose   : 'T        -> int array

[<AbstractClass>]
type ComposedSetDatabase<'T when 'T : comparison>() as this =
    let partToIndex                   = new System.Collections.Generic.Dictionary<'T, int>()
    let composedToIndicies            = new System.Collections.Generic.Dictionary<'T, int array>();
    member val parts                  : 'T list               = [] with get,set

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
                        this.parts <- this.parts @ [part]
                        let newIndex = (this.parts |> List.length) - 1
                        yield newIndex
                        partToIndex.Add(part, newIndex)                    
            |]
            composedToIndicies.Add(composed, indicies)
            indicies

    interface IComposedSetDatabase<'T> with
        member x.Compose indicies   = this.Compose indicies
        member x.Decompose composed = this.Decompose composed
            


type ComposedSet<'T, 'TDB when 'TDB :> ComposedSetDatabase<'T> and 'T : comparison and 'TDB :(new : unit -> 'TDB)>(indicies : int array) as this =

    // Static
    static let database = new 'TDB()
    static let empty    = new ComposedSet<'T, 'TDB>()
     
    // Constructors
    new()               = ComposedSet([||] : int array)
    new(composed : 'T)  = ComposedSet(database.Decompose(composed) : int array)

    // Instance
    member x.indicies = this.indicies
    member x.GetIndiciesAsString = "[" + (indicies |> Array.map (fun i -> i.ToString()) |> String.concat ", ") + "]"
    member x.Compose = database.Compose(indicies)
 
    
