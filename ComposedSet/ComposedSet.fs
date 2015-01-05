namespace ComposedSet.FSharp

type IComposedSetDatabase<'T> =
    abstract member Compose     : ResizeArray<int>  -> 'T
    abstract member Decompose   : 'T                -> ResizeArray<int>

[<AbstractClass>]
type ComposedSetDatabase<'T when 'T : comparison>() as this =
    let partToIndex                   = new System.Collections.Generic.Dictionary<'T, int>()
    let composedToIndicies            = new System.Collections.Generic.Dictionary<'T, ResizeArray<int>>()
    member val parts                  : ResizeArray<'T>               = new ResizeArray<'T>()

    abstract member Compose           : ResizeArray<int> -> 'T
    abstract member Split             : 'T               -> 'T array  
    
    member this.Decompose composed = 
        let ok, cachedIndicies = composedToIndicies.TryGetValue composed        
        match ok with
        | true -> cachedIndicies 
        | false ->
            let indicies : ResizeArray<int> = new ResizeArray<int>()            
            for part in this.Split composed do 
                let ok, cachedIndex = partToIndex.TryGetValue part
                match ok with
                | true -> indicies.Add cachedIndex
                | false -> 
                    this.parts.Add part
                    let newIndex = (this.parts |> Seq.length) - 1
                    indicies.Add newIndex
                    partToIndex.Add(part, newIndex)                    
            
            composedToIndicies.Add(composed, indicies)
            indicies

    interface IComposedSetDatabase<'T> with
        member x.Compose indicies   = this.Compose indicies
        member x.Decompose composed = this.Decompose composed
            


type ComposedSet<'T, 'TDB when 'TDB :> ComposedSetDatabase<'T> and 'T : comparison and 'TDB :(new : unit -> 'TDB)>(indicies : ResizeArray<int>) as this =

    // Static
    static let database = new 'TDB()
    static let empty    = new ComposedSet<'T, 'TDB>()
     
    // Constructors
    new()               = ComposedSet(new ResizeArray<int>())
    new(composed : 'T)  = ComposedSet(database.Decompose(composed) : ResizeArray<int>)

    // Instance
    member x.indicies = this.indicies
    member x.GetIndiciesAsString = "[" + (indicies |> Seq.map (fun i -> i.ToString()) |> String.concat ", ") + "]"
    member x.Compose = database.Compose(indicies)
 
    
