namespace ComposedSet.FSharp

open ComposedSet
open System.Text.RegularExpressions

type StringComposedSetDatabase() =
    inherit ComposedSetDatabase<string>()
        do printfn "Creating StringComposedSetDatabase"

    override this.Compose indicies = 
        indicies |> Array.map (fun i -> this.parts.[i]) |> String.concat ""
            
    override this.Split composed = 
         Regex.Split(composed, @"(\.)|(\/)|(\@)|(_)|(\f)", RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))

