// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.



module TestCompsedSet =
    
    let testStringA = "A.B.C.D"
    let testStringB = "B/A.C/A.C.D"
    
    let csharpA = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>(testStringA)
    let fsharpA = ComposedSet.FSharp.ComposedSet<System.String, ComposedSet.FSharp.StringComposedSetDatabase>(testStringA)

    let csharpB = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>(testStringB)
    let fsharpB = ComposedSet.FSharp.ComposedSet<System.String, ComposedSet.FSharp.StringComposedSetDatabase>(testStringB)
        
    printfn "\n-- TEST '%s' --" testStringA
    
    printfn "\n GetIndiciesAsString:"
    printfn "  %s" (csharpA.GetIndiciesAsString())
    printfn "  %s" (fsharpA.GetIndiciesAsString)

    printfn "\n Compose:"
    printfn "  %s" (csharpA.Compose())
    printfn "  %s" (fsharpA.Compose)
       
    printfn "\n-- TEST '%s' --" testStringB
        
    printfn "\n GetIndiciesAsString:"
    printfn "  %s" (csharpB.GetIndiciesAsString())
    printfn "  %s" (fsharpB.GetIndiciesAsString)

    printfn "\n Compose:"
    printfn "  %s" (csharpB.Compose())
    printfn "  %s" (fsharpB.Compose)

