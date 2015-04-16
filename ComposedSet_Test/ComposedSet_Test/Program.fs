namespace ComposedSet.Test

module PerformanceTest =
    type CComposedSet = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FComposedSet = ComposedSet.FSharp.ComposedSet<System.String, ComposedSet.FSharp.StringComposedSetDatabase>

    let testStringA  = "A.B.C.D"
    let testStringB  = "B/A.C/A.C.D"
    let testStringC  = "C.D"

    let fsharpA = FComposedSet(testStringA)
    let csharpA = CComposedSet(testStringA)
    
    let fsharpB = FComposedSet(testStringB)
    let csharpB = CComposedSet(testStringB)
    
    let fsharpC = FComposedSet(testStringC)
    let csharpC = CComposedSet(testStringC)
    
    let profile name iterations f =
        let w = new System.Diagnostics.Stopwatch()
        w.Start()
        for i in 0..iterations do f()
        w.Stop()
        System.GC.Collect()
        System.Threading.Thread.Sleep(100)
        printfn "%s took %dms" name w.ElapsedMilliseconds

    
    let iterations = 1000000
    printfn "\n--- StartsWith x%i F# vs C# ---" iterations
    profile "F# Startswith" iterations (fun () -> (fsharpB.StartsWith(fsharpA)) |> ignore)
    profile "C# Startswith" iterations (fun () -> (csharpB.StartsWith(csharpA)) |> ignore) 

    printfn "\n--- EndsWith x%i F# vs C# ---" iterations
    profile "F# Endswith" iterations (fun () -> (fsharpB.EndsWith(fsharpA)) |> ignore )
    profile "C# Endswith" iterations (fun () -> (csharpB.EndsWith(csharpA)) |> ignore )
    
    printfn "\n--- Equals x%i F# vs C# ---" iterations
    profile "F# Equals" iterations (fun () -> (fsharpB.Equals(fsharpA)) |> ignore )
    profile "C# Equals" iterations (fun () -> (fsharpB.Equals(fsharpA)) |> ignore )
    
    printfn "\n--- First Decompose F# vs C# ---"
    let shakespeare = System.IO.File.ReadAllText("..\..\shakespeare.txt")    

    let mutable fsharpShakespeare = FComposedSet("")
    let mutable csharpShakespeare = CComposedSet("")
    profile "F# Decompose" 1 (fun () -> (fsharpShakespeare <- FComposedSet(shakespeare)))
    profile "C# Decompose" 1 (fun () -> (csharpShakespeare <- CComposedSet(shakespeare)))
    
    printfn "\n--- Second Decompose F# vs C# ---"
    profile "F# Decompose" 1 (fun () -> (fsharpShakespeare <- FComposedSet(shakespeare)))
    profile "C# Decompose" 1 (fun () -> (csharpShakespeare <- CComposedSet(shakespeare)))
    
    printfn "\n--- Compose F# vs C# ---"
    let mutable fshakespeare = ""
    let mutable cshakespeare = ""
    profile "F# Compose" 4 (fun () -> (fshakespeare <- fsharpShakespeare.Compose  ))
    profile "C# Compose" 4 (fun () -> (cshakespeare <- csharpShakespeare.Compose()))
    