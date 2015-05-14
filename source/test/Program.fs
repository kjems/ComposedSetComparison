namespace ComposedSet.Test

module PerformanceTest =
    type CComposedSet   = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FComposedSetOO = ComposedSet.FSharp.ComposedSetOO.ComposedSet<System.String, ComposedSet.FSharp.ComposedSetOO.StringComposedSetDatabase>
    open ComposedSet.FSharpIdiomatic.ComposedSetOfStrings
    open ComposedSet.FSharpIdiomatic.ComposedSet

    let str_a  = "A.B.C.D"
    let str_a2 = "A.B.C.D"
    let str_b  = "B/A.C/A.C.D"
    let str_c  = "C.D"
    let str_d  = "A.B"
    let str_e  = "A.B.C.D.E.F"

    let fsoo_a = FComposedSetOO(str_a)
    let fs_a   = decompose str_a
    let cs_a   = CComposedSet(str_a)

    let fsoo_a2= FComposedSetOO(str_a2)
    let fs_a2  = decompose str_a2
    let cs_a2  = CComposedSet(str_a2)
    
    let fsoo_b = FComposedSetOO(str_b)
    let fs_b   = decompose str_b
    let cs_b   = CComposedSet(str_b)
    
    let fsoo_c = FComposedSetOO(str_c)
    let fs_c   = decompose str_c
    let cs_c   = CComposedSet(str_c)

    let fsoo_d = FComposedSetOO(str_d)
    let fs_d   = decompose str_d
    let cs_d   = CComposedSet(str_d)

    let fsoo_e = FComposedSetOO(str_e)
    let fs_e   = decompose str_e
    let cs_e   = CComposedSet(str_e)
    
    let profile name iterations f =
        let w = new System.Diagnostics.Stopwatch()
        w.Start()
        for i in 0..iterations do f()
        w.Stop()
        System.GC.Collect()
        System.Threading.Thread.Sleep(100)
        printfn "%s took %dms" name w.ElapsedMilliseconds

    let iterations = 500000

    printfn "\n--- StartsWith x%i ---" iterations
    profile "C#    Startswith" iterations (fun () -> 
        cs_b.StartsWith(cs_a) |> ignore
        cs_b.StartsWith(cs_d) |> ignore)
    profile "F# OO Startswith" iterations (fun () -> 
        fsoo_b.StartsWith(fsoo_a) |> ignore  
        fsoo_b.StartsWith(fsoo_d) |> ignore)
    profile "F#    Startswith" iterations (fun () -> 
        startswith fs_b fs_a  |> ignore  
        startswith fs_b fs_d  |> ignore)

    printfn "\n--- EndsWith x%i ---" iterations
    profile "C#    Endswith" iterations (fun () -> 
        cs_b.EndsWith(cs_a) |> ignore
        cs_b.EndsWith(cs_c) |> ignore) 
    profile "F# OO Endswith" iterations (fun () -> 
        fsoo_b.EndsWith(fsoo_a) |> ignore
        fsoo_b.EndsWith(fsoo_c) |> ignore)
    profile "F#    Endswith" iterations (fun () -> 
        endswith fs_b fs_a  |> ignore  
        endswith fs_b fs_c  |> ignore)
    
    printfn "\n--- Equals x%i ---" iterations
    profile "C#    Equals" iterations (fun () -> 
        cs_b.Equals(cs_a) |> ignore
        (cs_a = cs_a2)    |> ignore
        cs_a = cs_e       |> ignore)
    profile "F# OO Equals" iterations (fun () -> 
        fsoo_b.Equals(fsoo_a)  |> ignore
        fsoo_a = fsoo_a2       |> ignore
        fsoo_a = fsoo_e        |> ignore)
    profile "F#    Equals" iterations (fun () -> 
        equals fs_b fs_a  |> ignore  
        equals fs_a fs_a2 |> ignore
        equals fs_a fs_e  |> ignore)    

    printfn "\n--- Concat x%i ---" iterations
    profile "C#    Concat" iterations (fun () -> 
        cs_b + cs_a   |> ignore
        cs_a + cs_a2  |> ignore
        cs_a + cs_e   |> ignore)
    profile "F# OO Concat" iterations (fun () -> 
        fsoo_b + fsoo_a   |> ignore
        fsoo_a + fsoo_a2  |> ignore
        fsoo_a + fsoo_e   |> ignore)
    profile "F#    Concat" iterations (fun () -> 
        concat fs_b fs_a  |> ignore  
        concat fs_a fs_a2 |> ignore
        concat fs_a fs_e  |> ignore)

    printfn "\n--- TrimEnd x%i ---" iterations
    profile "C#    TrimEnd" iterations (fun () -> (cs_b.TrimEnd(cs_c)) |> ignore )
    profile "F# OO TrimEnd" iterations (fun () -> (fsoo_b.TrimEnd(fsoo_c)) |> ignore)
    profile "F#    TrimEnd" iterations (fun () -> (trimend fs_b fs_c) |> ignore)
    
    printfn "\n--- First Decompose ---"
    let str_bigtext = System.IO.File.ReadAllText("..\..\..\..\data\hack11a.txt")    

    let mutable cs_bigtext   = CComposedSet("")
    let mutable fsoo_bigtext = FComposedSetOO("")    
    let mutable fs_bigtext   = decompose ""
    profile "C#    Decompose" 1 (fun () -> (cs_bigtext   <- CComposedSet(str_bigtext)))
    profile "F# OO Decompose" 1 (fun () -> (fsoo_bigtext <- FComposedSetOO(str_bigtext)))
    profile "F#    Decompose" 1 (fun () -> (fs_bigtext   <- decompose str_bigtext))
    
    
    printfn "\n--- Second Decompose ---"
    profile "C#    Decompose" 1 (fun () -> (cs_bigtext   <- CComposedSet(str_bigtext)))
    profile "F# OO Decompose" 1 (fun () -> (fsoo_bigtext <- FComposedSetOO(str_bigtext)))
    profile "F#    Decompose" 1 (fun () -> (fs_bigtext   <- decompose str_bigtext))
    
    
    printfn "\n--- Compose ---"
    let mutable cs_composed_bigtext   = ""
    let mutable fsoo_composed_bigtext = ""
    let mutable fs_composed_bigtext   = ""
    profile "C#    Compose" 4 (fun () -> (cs_composed_bigtext   <- cs_bigtext.Compose()))
    profile "F# OO Compose" 4 (fun () -> (fsoo_composed_bigtext <- fsoo_bigtext.Compose  ))    
    profile "F#    Compose" 4 (fun () -> (fs_composed_bigtext   <- compose fs_bigtext))

    //printfn "%s" fsharpShakespeare.GetParts

    printfn "C#    Decompose => Compose Equal original: %b" (cs_bigtext.Compose().Equals(str_bigtext))
    printfn "F# OO Decompose => Compose Equal original: %b" (fsoo_bigtext.Compose.Equals(str_bigtext))
    printfn "F#    Decompose => Compose Equal original: %b" ((compose fs_bigtext).Equals(str_bigtext))

    
    
    