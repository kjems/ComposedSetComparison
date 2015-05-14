namespace ComposedSet.Test

module PerformanceTest =
    type CComposedSet   = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FComposedSetOO = ComposedSet.FSharp.ComposedSetOO.ComposedSet<System.String, ComposedSet.FSharp.ComposedSetOO.StringComposedSetDatabase>
    open ComposedSet.FSharpIdiomatic.ComposedSetOfStrings
    open ComposedSet.FSharpIdiomatic.ComposedSet

    //open ComposedSet.FSharpIdomatic.Test
    //testit
    //exit 0

    let testStringA  = "A.B.C.D"
    let testStringA2 = "A.B.C.D"
    let testStringB  = "B/A.C/A.C.D"
    let testStringC  = "C.D"
    let testStringD  = "A.B"
    let testStringE  = "A.B.C.D.E.F"

    let fsharpA = FComposedSetOO(testStringA)
    let ifsa    = decompose testStringA
    let csharpA = CComposedSet(testStringA)

    let fsharpA2= FComposedSetOO(testStringA2)
    let csharpA2= CComposedSet(testStringA2)
    
    let fsharpB = FComposedSetOO(testStringB)
    let ifsb = decompose testStringB
    let csharpB = CComposedSet(testStringB)
    
    let fsharpC = FComposedSetOO(testStringC)
    let csharpC = CComposedSet(testStringC)

    let fsharpD = FComposedSetOO(testStringD)
    let ifsd    = decompose testStringD
    let csharpD = CComposedSet(testStringD)

    let fsharpE = FComposedSetOO(testStringE)
    let csharpE = CComposedSet(testStringE)
    
    let profile name iterations f =
        let w = new System.Diagnostics.Stopwatch()
        w.Start()
        for i in 0..iterations do f()
        w.Stop()
        System.GC.Collect()
        System.Threading.Thread.Sleep(100)
        printfn "%s took %dms" name w.ElapsedMilliseconds

    let iterations = 500000

    printfn "\n--- StartsWith x%i F# vs C# ---" iterations
    profile "F# Startswith" iterations (fun () -> 
            fsharpB.StartsWith(fsharpA) |> ignore  
            fsharpB.StartsWith(fsharpD) |> ignore)

    profile "F# ido Startswith" iterations (fun () -> 
            startswith ifsb ifsa  |> ignore  
            startswith ifsb ifsd  |> ignore)

    profile "C# Startswith" iterations (fun () -> 
        csharpB.StartsWith(csharpA) |> ignore
        csharpB.StartsWith(csharpD) |> ignore)

    printfn "\n--- EndsWith x%i F# vs C# ---" iterations
    profile "F# Endswith" iterations (fun () -> 
        fsharpB.EndsWith(fsharpA) |> ignore
        fsharpB.EndsWith(fsharpC) |> ignore)

    profile "C# Endswith" iterations (fun () -> 
        csharpB.EndsWith(csharpA) |> ignore
        csharpB.EndsWith(csharpC) |> ignore) 
    
    printfn "\n--- Equals x%i F# vs C# ---" iterations
    profile "F# Equals" iterations (fun () -> 
        fsharpB.Equals(fsharpA)  |> ignore
        fsharpA = fsharpA2       |> ignore
        fsharpA = fsharpE        |> ignore)

    profile "C# Equals" iterations (fun () -> 
        csharpB.Equals(csharpA) |> ignore
        (csharpA = csharpA2)    |> ignore
        csharpA = csharpE       |> ignore)

    printfn "\n--- Concat x%i F# vs C# ---" iterations
    profile "F# Concat" iterations (fun () -> 
        fsharpB + fsharpA   |> ignore
        fsharpA + fsharpA2  |> ignore
        fsharpA + fsharpE   |> ignore)

    profile "C# Concat" iterations (fun () -> 
        csharpB + csharpA   |> ignore
        csharpA + csharpA2  |> ignore
        csharpA + csharpE   |> ignore)

    printfn "\n--- TrimEnd x%i F# vs C# ---" iterations
    profile "F# TrimEnd" iterations (fun () -> (fsharpB.TrimEnd(fsharpC)) |> ignore )
    profile "C# TrimEnd" iterations (fun () -> (csharpB.TrimEnd(csharpC)) |> ignore )
    
    printfn "\n--- First Decompose F# vs C# ---"
    let shakespeare = System.IO.File.ReadAllText("..\..\shakespeare.txt")    

    let mutable fsharpShakespeare = FComposedSetOO("")
    let mutable csharpShakespeare = CComposedSet("")
    profile "F# Decompose" 1 (fun () -> (fsharpShakespeare <- FComposedSetOO(shakespeare)))
    profile "C# Decompose" 1 (fun () -> (csharpShakespeare <- CComposedSet(shakespeare)))
    
    printfn "\n--- Second Decompose F# vs C# ---"
    profile "F# Decompose" 1 (fun () -> (fsharpShakespeare <- FComposedSetOO(shakespeare)))
    profile "C# Decompose" 1 (fun () -> (csharpShakespeare <- CComposedSet(shakespeare)))
    
    printfn "\n--- Compose F# vs C# ---"
    let mutable fshakespeare = ""
    let mutable cshakespeare = ""
    profile "F# Compose" 4 (fun () -> (fshakespeare <- fsharpShakespeare.Compose  ))
    profile "C# Compose" 4 (fun () -> (cshakespeare <- csharpShakespeare.Compose()))

    //printfn "%s" fsharpShakespeare.GetParts

    printfn "F# Decompose => Compose Equal original: %b" (fsharpShakespeare.Compose.Equals(shakespeare))
    printfn "C# Decompose => Compose Equal original: %b" (csharpShakespeare.Compose().Equals(shakespeare))

    
    
    