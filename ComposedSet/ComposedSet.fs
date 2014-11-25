namespace ComposedSet.FSharp

type IComposedSetDatabase<'T> =
    abstract member Compose     : int list  -> 'T
    abstract member Decompose   : 'T        -> int list

[<AbstractClass>]
type ComposedSetDatabase<'T when 'T : comparison>() as this =
    let mutable partToIndex         : Map<'T, int>      = Map.empty
    let mutable composedToIndicies  : Map<'T, int list> = Map.empty
    member val parts                : 'T list           = [] with get,set

    abstract member Compose         : int list  -> 'T
    abstract member Split           : 'T        -> 'T list  

    member this.Decompose composed = 
        let cachedIndicies = composedToIndicies |> Map.tryFind composed
        match cachedIndicies with
        | Some indicies -> indicies 
        | None ->
            let indiciesSeq = seq {
                for part in this.Split composed do 
                    let cachedIndex = partToIndex |> Map.tryFind part
                    match cachedIndex with
                    | Some index -> yield index
                    | None -> 
                        this.parts <- this.parts @ [part]
                        let newIndex = (this.parts |> List.length) - 1
                        yield newIndex
                        partToIndex <- partToIndex.Add(part, newIndex)
                    
            }
            let indicies = Seq.toList indiciesSeq
            composedToIndicies <- composedToIndicies.Add(composed, indicies)
            indicies

    interface IComposedSetDatabase<'T> with
        member x.Compose indicies   = this.Compose indicies
        member x.Decompose composed = this.Decompose composed
            


type ComposedSet<'T, 'TDB when 'TDB :> ComposedSetDatabase<'T> and 'T : comparison and 'TDB :(new : unit -> 'TDB)>(indicies : int list) as this =

    // Static
    static let database = new 'TDB()
    static let empty    = new ComposedSet<'T, 'TDB>()
     
    // Constructors
    new()               = ComposedSet([] : int list)
    new(composed : 'T)  = ComposedSet(database.Decompose(composed) : int list)

    // Instance
    member x.indicies = this.indicies
    member x.GetIndiciesAsString = indicies |> List.map (fun i -> i.ToString()) |> String.concat ""  
    member x.Compose = database.Compose(indicies)
 
    
