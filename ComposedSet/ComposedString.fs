namespace ComposedSet.FSharp

open ComposedSet
open System.Text.RegularExpressions

type StringComposedSetDatabase() =
    inherit ComposedSetDatabase<string>()
        do printfn "Creating StringComposedSetDatabase"

    override this.Compose indicies = 
        indicies |> List.map (fun i -> (this :> ComposedSetDatabase<string>).parts.[i]) |> String.concat ""
            
    override this.Split composed = 
         Regex.Split(composed, @"(\.)|(\/)|(\@)|(_)", RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s)) |> Array.toList

