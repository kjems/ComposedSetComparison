namespace ComposedSet.Test

open System
open System.Collections
open NUnit.Framework
open NUnit.Framework.Constraints

module UnitTest =

    type CCSet = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FCSet = ComposedSet.FSharp.ComposedSet<System.String, ComposedSet.FSharp.StringComposedSetDatabase>

    [<Test>]
    let TrimEndFS() =
        let abcd  = FCSet("A.B.C.D")
        let ab    = FCSet("A.B.")
        let cd    = FCSet("C.D")
        let empty = FCSet("")
        Assert.That(abcd .TrimEnd(cd)    .Equals(ab),    Is.True)
        Assert.That(ab   .TrimEnd(ab)    .Equals(empty), Is.True)
        Assert.That(empty.TrimEnd(empty) .Equals(empty), Is.True)
        Assert.That(ab   .TrimEnd(empty) .Equals(ab),    Is.True)
        Assert.That(abcd .TrimEnd(ab)    .Equals(ab),    Is.False)
        Assert.That(abcd .TrimEnd(ab)    .Equals(cd),    Is.False)

    [<Test>]
    let TrimEndCS() =
        let abcd  = CCSet("A.B.C.D")
        let ab    = CCSet("A.B.")
        let cd    = CCSet("C.D")
        let empty = CCSet("")
        Assert.That(abcd .TrimEnd(cd)    .Equals(ab),    Is.True)
        Assert.That(ab   .TrimEnd(ab)    .Equals(empty), Is.True)
        Assert.That(empty.TrimEnd(empty) .Equals(empty), Is.True)
        Assert.That(ab   .TrimEnd(empty) .Equals(ab),    Is.True)
        Assert.That(abcd .TrimEnd(ab)    .Equals(ab),    Is.False)
        Assert.That(abcd .TrimEnd(ab)    .Equals(cd),    Is.False)
        

    [<Test>]
    let OperatorAddFS() =
        let abcd  = FCSet("A.B.C.D")
        let a     = FCSet("A")
        let b     = FCSet("B")
        let c     = FCSet("C")
        let d     = FCSet("D")
        let ab    = FCSet("A.B")
        let cd    = FCSet("C.D")
        let dot   = FCSet(".")
        Assert.That(abcd.Equals(ab + dot + cd), Is.True)
        Assert.That((ab + dot + cd).Equals(ab + dot + cd), Is.True)
        Assert.That(abcd.Equals(a + dot + b + dot + c + dot + d), Is.True)
        Assert.That(abcd.Equals(ab + dot + dot + cd), Is.False)
        
    [<Test>]
    let OperatorAddCS() =
        let abcd  = CCSet("A.B.C.D")
        let a     = CCSet("A")
        let b     = CCSet("B")
        let c     = CCSet("C")
        let d     = CCSet("D")
        let ab    = CCSet("A.B")
        let cd    = CCSet("C.D")
        let dot   = CCSet(".")
        Assert.That(abcd.Equals(ab + dot + cd), Is.True)
        Assert.That((ab + dot + cd).Equals(ab + dot + cd), Is.True)
        Assert.That(abcd.Equals(a + dot + b + dot + c + dot + d), Is.True)
        Assert.That(abcd.Equals(ab + dot + dot + cd), Is.False)
        

    [<Test>]
    let EndsWithFS() =
        Assert.That(FCSet("A.B.C.D")    .EndsWith(FCSet("A.B.C.D")),Is.True)
        Assert.That(FCSet("A.B.C.D")    .EndsWith(FCSet("A.B.C.D")),Is.True)
        Assert.That(FCSet("B/A.C/A.C.D").EndsWith(FCSet("C.D")),    Is.True) 
        Assert.That(FCSet("0.B.C.D")    .EndsWith(FCSet("C.D")),    Is.True) 
        Assert.That(FCSet("a.B.C.D")    .EndsWith(FCSet("C.D")),    Is.True)
        Assert.That(FCSet("A/B/C/D")    .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("A.B.CD")     .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("A.B.C.C")    .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("A.B.C.D.")   .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("0.B.C.D")    .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("a.B.C.D")    .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("B.B.C.D")    .EndsWith(FCSet("A.B.C.D")),Is.False) 
        Assert.That(FCSet("A.B.C.D")    .EndsWith(FCSet("C.D")),    Is.True)  
        Assert.That(FCSet(" ")          .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("")           .EndsWith(FCSet("A.B.C.D")),Is.False)
        Assert.That(FCSet("")           .EndsWith(FCSet("")),       Is.False)


    [<Test>]
    let EndsWithCS() =
        Assert.That(CCSet("A.B.C.D")    .EndsWith(CCSet("A.B.C.D")),Is.True)
        Assert.That(CCSet("A.B.C.D")    .EndsWith(CCSet("A.B.C.D")),Is.True)
        Assert.That(CCSet("B/A.C/A.C.D").EndsWith(CCSet("C.D")),    Is.True) 
        Assert.That(CCSet("0.B.C.D")    .EndsWith(CCSet("C.D")),    Is.True) 
        Assert.That(CCSet("a.B.C.D")    .EndsWith(CCSet("C.D")),    Is.True)
        Assert.That(CCSet("A/B/C/D")    .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("A.B.CD")     .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("A.B.C.C")    .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("A.B.C.D.")   .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("0.B.C.D")    .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("a.B.C.D")    .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("B.B.C.D")    .EndsWith(CCSet("A.B.C.D")),Is.False) 
        Assert.That(CCSet("A.B.C.D")    .EndsWith(CCSet("C.D")),    Is.True)  
        Assert.That(CCSet(" ")          .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("")           .EndsWith(CCSet("A.B.C.D")),Is.False)
        Assert.That(CCSet("")           .EndsWith(CCSet("")),       Is.False)


    [<Test>]
    let EqualsFS() =
        Assert.That(FCSet("A.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.True)
        Assert.That(FCSet(" ")       .Equals(FCSet(" ")),        Is.True)
        Assert.That(FCSet("" )       .Equals(FCSet("")),         Is.True)
        Assert.That(FCSet("A/B/C/D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.CD"  ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.C.C" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.C.D.").Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("0.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("a.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("B.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.False)     
        Assert.That(FCSet(" ")       .Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("" )       .Equals(FCSet("A.B.C.D" )), Is.False)
        

    [<Test>]
    let EqualsCS() =
        Assert.That(CCSet("A.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.True)
        Assert.That(CCSet(" ")       .Equals(CCSet(" ")),        Is.True)
        Assert.That(CCSet("" )       .Equals(CCSet("")),         Is.True)
        Assert.That(CCSet("A/B/C/D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.CD"  ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.C.C" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.C.D.").Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("0.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("a.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("B.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.False)     
        Assert.That(CCSet(" ")       .Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("" )       .Equals(CCSet("A.B.C.D" )), Is.False)
        

    [<Test>]
    let ComposeFS() =
        Assert.That(FCSet("A.B.C.D") .Compose, Is.EqualTo("A.B.C.D"))
        Assert.That(FCSet("A.B.C.D") .Compose, Is.Not.EqualTo("A/B/C/D"))
        Assert.That(FCSet("A/B/C/D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(FCSet("A.B.CD")  .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(FCSet("A.B.C.C") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(FCSet("A.B.C.D.").Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(FCSet("0.B.C.D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(FCSet("a.B.C.D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(FCSet("B.B.C.D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        

    [<Test>]
    let ComposeCS() =
        Assert.That(CCSet("A.B.C.D") .Compose, Is.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.C.D") .Compose, Is.Not.EqualTo("A/B/C/D"))
        Assert.That(CCSet("A/B/C/D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.CD")  .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.C.C") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.C.D.").Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("0.B.C.D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("a.B.C.D") .Compose, Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("B.B.C.D") .Compose, Is.Not.EqualTo("A.B.C.D"))