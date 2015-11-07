namespace ComposedSet.Test

open NUnit.Framework

module UnitTest =

    type CCSet = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FCSet = ComposedSet.FSharp.ComposedSetOO.ComposedSet<System.String, ComposedSet.FSharp.ComposedSetOO.StringComposedSetDatabase>
    open ComposedSet.FSharp.ComposedSetOfStrings
    open ComposedSet.FSharp.ComposedSet

    [<Test>]
    let GetHashCodeFSOO() =
        let abcd  = FCSet("A.B.C.D")
        Assert.That(FCSet("A.B.C.D").GetHashCode()  = FCSet("A.B.C.D")  .GetHashCode(), Is.True)
        Assert.That(abcd            .GetHashCode()  = abcd              .GetHashCode(), Is.True)
        Assert.That(FCSet("")       .GetHashCode()  = FCSet("")         .GetHashCode(), Is.True)
        Assert.That(FCSet("A.B.C.D").GetHashCode()  = FCSet("A.B.C.E")  .GetHashCode(), Is.False)
        Assert.That(FCSet(" ")      .GetHashCode()  = FCSet("")         .GetHashCode(), Is.False)

    [<Test>]
    let GetHashCodeFS() =
        let abcd  = decompose "A.B.C.D"
        let dcca  = calchash << decompose
        Assert.That(dcca "A.B.C.D" = dcca "A.B.C.D" , Is.True)
        Assert.That(calchash abcd  = calchash abcd  , Is.True)
        Assert.That(dcca ""        = dcca ""        , Is.True)
        Assert.That(dcca "A.B.C.D" = dcca "A.B.C.E" , Is.False)
        Assert.That(dcca " "       = dcca ""        , Is.False)

    [<Test>]
    let GetHashCodeCS() =
        let abcd  = CCSet("A.B.C.D")
        Assert.That(CCSet("A.B.C.D").GetHashCode()  = CCSet("A.B.C.D")  .GetHashCode(), Is.True)
        Assert.That(abcd            .GetHashCode()  = abcd              .GetHashCode(), Is.True)
        Assert.That(CCSet("")       .GetHashCode()  = CCSet("")         .GetHashCode(), Is.True)
        Assert.That(CCSet("A.B.C.D").GetHashCode()  = CCSet("A.B.C.E")  .GetHashCode(), Is.False)
        Assert.That(CCSet(" ")      .GetHashCode()  = CCSet("")         .GetHashCode(), Is.False)


    [<Test>]
    let TrimEndFSOO() =
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
    let TrimEndFS() =
        let abcd  = decompose "A.B.C.D"
        let ab    = decompose "A.B."
        let cd    = decompose "C.D"
        let empty = decompose ""
        Assert.That(equals (trimend abcd cd) ab,         Is.True)
        Assert.That(equals (trimend ab ab) empty,        Is.True)
        Assert.That(equals (trimend empty empty) empty,  Is.True)
        Assert.That(equals (trimend ab empty) ab,        Is.True)
        Assert.That(equals (trimend abcd ab) ab,         Is.False)
        Assert.That(equals (trimend abcd ab) cd,         Is.False)

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
    let OperatorAddFSOO() =
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
        Assert.That(abcd.GetHashCode() = (a + dot + b + dot + c + dot + d).GetHashCode(), Is.True)
        Assert.That(abcd.Equals(ab + dot + dot + cd), Is.False)

    [<Test>]
    let OperatorAddFS() =
        let abcd  = decompose "A.B.C.D"
        let a     = decompose "A"
        let b     = decompose "B"
        let c     = decompose "C"
        let d     = decompose "D"
        let ab    = decompose "A.B"
        let cd    = decompose "C.D"
        let dot   = decompose "."
        Assert.That(equals abcd (ab ++ dot ++ cd), Is.True)
        Assert.That(equals (ab ++ dot ++ cd) (ab ++ dot ++ cd), Is.True)
        Assert.That(equals abcd (a ++ dot ++ b ++ dot ++ c ++ dot ++ d), Is.True)
        Assert.That(abcd.GetHashCode() = (a ++ dot ++ b ++ dot ++ c ++ dot ++ d).GetHashCode(), Is.True)
        Assert.That(equals abcd (ab ++ dot ++ dot ++ cd), Is.False)
        
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
        Assert.That(abcd.GetHashCode() = (a + dot + b + dot + c + dot + d).GetHashCode(), Is.True)
        Assert.That(abcd.Equals(ab + dot + dot + cd), Is.False)
        

    [<Test>]
    let EndsWithFSOO() =
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

    [<Test>]
    let EndsWithFS() =
        let dcew a b = endswith (decompose a) (decompose b)
        Assert.That(dcew "A.B.C.D"     "A.B.C.D",Is.True)
        Assert.That(dcew "A.B.C.D"     "A.B.C.D",Is.True)
        Assert.That(dcew "B/A.C/A.C.D" "C.D",    Is.True) 
        Assert.That(dcew "0.B.C.D"     "C.D",    Is.True) 
        Assert.That(dcew "a.B.C.D"     "C.D",    Is.True)
        Assert.That(dcew "A/B/C/D"     "A.B.C.D",Is.False)
        Assert.That(dcew "A.B.CD"      "A.B.C.D",Is.False)
        Assert.That(dcew "A.B.C.C"     "A.B.C.D",Is.False)
        Assert.That(dcew "A.B.C.D."    "A.B.C.D",Is.False)
        Assert.That(dcew "0.B.C.D"     "A.B.C.D",Is.False)
        Assert.That(dcew "a.B.C.D"     "A.B.C.D",Is.False)
        Assert.That(dcew "B.B.C.D"     "A.B.C.D",Is.False) 
        Assert.That(dcew "A.B.C.D"     "C.D",    Is.True)
        Assert.That(dcew " "           "A.B.C.D",Is.False)
        Assert.That(dcew ""            "A.B.C.D",Is.False)

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
        

    [<Test>]
    let EqualsFSOO() =
        Assert.That(FCSet("A.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.True)
        Assert.That(FCSet("A.B.C.D" ) =     (FCSet("A.B.C.D" )), Is.True)
        Assert.That(FCSet(" ")       .Equals(FCSet(" ")),        Is.True)
        Assert.That(FCSet("" )       .Equals(FCSet("")),         Is.True)
        Assert.That(FCSet("A/B/C/D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.CD"  ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.C.C" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.C.C" ) =     (FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("A.B.C.D.").Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("0.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("a.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("B.B.C.D" ).Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet(" ")       .Equals(FCSet("A.B.C.D" )), Is.False)
        Assert.That(FCSet("" )       .Equals(FCSet("A.B.C.D" )), Is.False)

    [<Test>]
    let EqualsFS() =
        let dceq a b = equals (decompose a) (decompose b)
        Assert.That(dceq "A.B.C.D"  "A.B.C.D", Is.True)
        Assert.That(dceq "A.B.C.D"  "A.B.C.D", Is.True)
        Assert.That(dceq " "        " ",       Is.True)
        Assert.That(dceq ""         "",        Is.True)
        Assert.That(dceq "A/B/C/D"  "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.CD"   "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.C.C"  "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.C.C"  "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.C.D." "A.B.C.D", Is.False)
        Assert.That(dceq "0.B.C.D"  "A.B.C.D", Is.False)
        Assert.That(dceq "a.B.C.D"  "A.B.C.D", Is.False)
        Assert.That(dceq "B.B.C.D"  "A.B.C.D", Is.False)
        Assert.That(dceq " "        "A.B.C.D", Is.False)
        Assert.That(dceq ""         "A.B.C.D", Is.False)

    [<Test>]
    let EqualsCS() =
        Assert.That(CCSet("A.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.True)
        Assert.That(CCSet("A.B.C.D" ) =     (CCSet("A.B.C.D" )), Is.True)
        Assert.That(CCSet(" ")       .Equals(CCSet(" ")),        Is.True)
        Assert.That(CCSet("" )       .Equals(CCSet("")),         Is.True)
        Assert.That(CCSet("A/B/C/D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.CD"  ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.C.C" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.C.C" ) =     (CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("A.B.C.D.").Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("0.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("a.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("B.B.C.D" ).Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet(" ")       .Equals(CCSet("A.B.C.D" )), Is.False)
        Assert.That(CCSet("" )       .Equals(CCSet("A.B.C.D" )), Is.False)
        

    [<Test>]
    let ComposeFSOO() =
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
    let ComposeFS() =
        let dcc = compose << decompose
        Assert.That(dcc "A.B.C.D" , Is.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.C.D" , Is.Not.EqualTo("A/B/C/D"))
        Assert.That(dcc "A/B/C/D" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.CD"  , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.C.C" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.C.D.", Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "0.B.C.D" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "a.B.C.D" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "B.B.C.D" , Is.Not.EqualTo("A.B.C.D"))
        

    [<Test>]
    let ComposeCS() =
        Assert.That(CCSet("A.B.C.D") .Compose(), Is.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.C.D") .Compose(), Is.Not.EqualTo("A/B/C/D"))
        Assert.That(CCSet("A/B/C/D") .Compose(), Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.CD")  .Compose(), Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.C.C") .Compose(), Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("A.B.C.D.").Compose(), Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("0.B.C.D") .Compose(), Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("a.B.C.D") .Compose(), Is.Not.EqualTo("A.B.C.D"))
        Assert.That(CCSet("B.B.C.D") .Compose(), Is.Not.EqualTo("A.B.C.D"))