namespace ComposedSet.FSharp

module List =
    let startWith (xs: 't list) (ys: 't list) =
        let rec startWithRec xs ys =
            match xs, ys with
            | [],[] -> true   // equals
            | _ ,[] -> true   // start with
            | [], _ -> false  // longer so false
            | x::xs, y::ys ->
                match x = y with
                    | false -> false
                    | true  -> startWithRec xs ys
        startWithRec xs ys                

    let sub (xs: 't list) (startIndex: int) (count: int) =
        let rec sub xs c i acc = 
            match c,i with
            | c,_ when c >= count      -> List.rev acc
            | _,i when i <  startIndex -> sub xs c (i+1) acc
            | _,i when i >= startIndex -> 
                match xs with
                | []    -> List.rev acc
                | x::xs -> sub xs (c+1) (i+1) (x::acc)
            | _,_ -> []  // should not happen
        sub xs 0 0 []

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
    override this.Equals other = // 457
        match other with
        | :? ComposedSet<'T,'TDB> as other -> 
            match this.indicies.Length = other.indicies.Length with
            | true  -> List.forall2 (fun a b -> a = b) this.indicies other.indicies
            | false -> false // length is not equal    
        | _ -> false // type mis-match

    override this.GetHashCode() =
        let rec calchash xs hash =
            match xs with
            | []    -> hash
            | x::xs -> calchash xs (hash * 7 + x)
        calchash this.indicies 13

    static member (+) (left : ComposedSet<'T, 'TDB>, right : ComposedSet<'T, 'TDB>) =
        new ComposedSet<'T, 'TDB>( List.append left.indicies right.indicies )

    static member IsNullOrEmpty(cset : ComposedSet<'T,'TDB>) =
        cset.indicies.Length = 0

    member this.EndsWith (other : ComposedSet<'T,'TDB>) =
        List.startWith (List.rev this.indicies) (List.rev other.indicies) 

    member this.StartsWith (other : ComposedSet<'T,'TDB>) =
        List.startWith this.indicies other.indicies

    member this.TrimEnd (other : ComposedSet<'T,'TDB>) =
        match this.EndsWith other with
        | true  -> new ComposedSet<'T,'TDB>(List.sub this.indicies 0 (this.indicies.Length - other.indicies.Length))
        | false -> this
