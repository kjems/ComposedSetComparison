namespace ComposedSet.Test

module PerformanceTest =
    type CComposedSet = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FComposedSet = ComposedSet.FSharp.ComposedSet<System.String, ComposedSet.FSharp.StringComposedSetDatabase>


    let testStringA  = "A.B.C.D"
    let testStringA2 = "A.B.C.D"
    let testStringB  = "B/A.C/A.C.D"
    let testStringC  = "C.D"

    let csharpA = CComposedSet(testStringA)
    let fsharpA = FComposedSet(testStringA)

    let csharpA2 = CComposedSet(testStringA2)
    let fsharpA2 = FComposedSet(testStringA2)

    let csharpB = CComposedSet(testStringB)
    let fsharpB = FComposedSet(testStringB)

    let csharpC = CComposedSet(testStringC)
    let fsharpC = FComposedSet(testStringC)
        
    
    let iterations = 500000
    printfn "\n--- EndsWith x%i F# vs C# ---" iterations
    let mutable res = false
    
    // ENDWITH
    let stopWatch = System.Diagnostics.Stopwatch()
    stopWatch.Start()    
    for i in 0..iterations do
        res <- csharpB.EndsWith(csharpA)
        res <- csharpA.EndsWith(csharpC)
        res <- csharpA.EndsWith(csharpA2)

    stopWatch.Stop()    
    printfn "C# EndsWith took : %fms" stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()
    stopWatch.Start()

    for i in 0..iterations do
        res <- fsharpB.EndsWith(fsharpA)
        res <- fsharpA.EndsWith(fsharpC)
        res <- fsharpA.EndsWith(fsharpA2)
    
    stopWatch.Stop()

    printfn "F# EndsWith took : %fms" stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()

    // EQUALS
    stopWatch.Start()    
    for i in 0..iterations do
        res <- csharpB.Equals(csharpA)
        res <- csharpA.Equals(csharpC)
        res <- csharpA.Equals(csharpA2)

    stopWatch.Stop()    
    printfn "C# Equals   took : %fms" stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()
    stopWatch.Start()

    for i in 0..iterations do
        res <- fsharpB.Equals(fsharpA)
        res <- fsharpA.Equals(fsharpC)
        res <- fsharpA.Equals(fsharpA2)
    
    stopWatch.Stop()

    printfn "F# Equals   took : %fms" stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()

    // COMPOSE / DECOMPOSE
    printfn "\n--- Compose / Decompose F# vs C# ---"
    let shakespeare = System.IO.File.ReadAllText("..\..\shakespeare.txt")    
       
    stopWatch.Reset()
    stopWatch.Start()
    let fsharpShakespeare = FComposedSet(shakespeare)
    stopWatch.Stop()
    printfn "1) F# Decomposing Shakespeare took : %fms" stopWatch.Elapsed.TotalMilliseconds

    stopWatch.Reset()
    stopWatch.Start()
    let fsharpShakespeare2 = FComposedSet(shakespeare)
    stopWatch.Stop()
    printfn "2) F# Decomposing Shakespeare took : %fms" stopWatch.Elapsed.TotalMilliseconds

    stopWatch.Reset()
    stopWatch.Start()
    let fsharpShakespeareComposed = fsharpShakespeare2.Compose
    stopWatch.Stop()
    printfn "3) F# Composing   Shakespeare took : %fms (%b)\n" stopWatch.Elapsed.TotalMilliseconds (shakespeare.CompareTo(fsharpShakespeareComposed) = 0)

    stopWatch.Reset()
    stopWatch.Start()
    let csharpShakespeare = CComposedSet(shakespeare)
    stopWatch.Stop()
    printfn "1) C# Decomposing Shakespeare took : %fms" stopWatch.Elapsed.TotalMilliseconds

    stopWatch.Reset()
    stopWatch.Start()
    let csharpShakespeare2 = CComposedSet(shakespeare)
    stopWatch.Stop()
    printfn "2) C# Decomposing Shakespeare took : %fms" stopWatch.Elapsed.TotalMilliseconds

    stopWatch.Reset()
    stopWatch.Start()
    let csharpShakespeareComposed = csharpShakespeare2.Compose()
    stopWatch.Stop()
    printfn "3) C# Composing   Shakespeare took : %fms (%b)\n" stopWatch.Elapsed.TotalMilliseconds (shakespeare.CompareTo(csharpShakespeareComposed) = 0)