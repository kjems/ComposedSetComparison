namespace ComposedSet.Test

open System
open System.Collections
open NUnit.Framework
open NUnit.Framework.Constraints

module UnitTest =

    type CCSet = ComposedSet.CSharp.ComposedSet<System.String, ComposedSet.CSharp.StringComposedSetDatabase>
    type FCSet = ComposedSet.FSharp.ComposedSet<System.String, ComposedSet.FSharp.StringComposedSetDatabase>

    let abcd1 = "A.B.C.D"
    let abcd2 = "A.B.C.D"
    let abcd3 = "A/B/C/D"
    let abcd4 = "A.B.CD"
    let abcd5 = "A.B.C.C"
    let abcd6 = "A.B.C.D."
    let abcd7 = "0.B.C.D"
    let abcd8 = "a.B.C.D"
    let abcd9 = "B.B.C.D"
    let str1 = "B/A.C/A.C.D"
    let cd = "C.D"
    let space = " "
    let empty = ""

    [<Test>]
    let EndsWithFS() =
        let cs1     = FCSet(abcd1)
        let cs2     = FCSet(abcd2)
        let cs3     = FCSet(abcd3)
        let cs4     = FCSet(abcd4)
        let cs5     = FCSet(abcd5)
        let cs6     = FCSet(abcd6)
        let cs7     = FCSet(abcd7)
        let cs8     = FCSet(abcd8)
        let cs9     = FCSet(abcd9)
        let csstr1  = FCSet(str1)
        let cscd    = FCSet(cd)
        let csspace = FCSet(space)
        let csempty = FCSet(empty)
        Assert.That(cs1.EndsWith(cs2), Is.True)
        Assert.That(cs2.EndsWith(cs1), Is.True)
        Assert.That(cs3.EndsWith(cs1), Is.False)
        Assert.That(cs4.EndsWith(cs1), Is.False)
        Assert.That(cs5.EndsWith(cs1), Is.False)
        Assert.That(cs6.EndsWith(cs1), Is.False)
        Assert.That(cs7.EndsWith(cs1), Is.False)
        Assert.That(cs8.EndsWith(cs1), Is.False)
        Assert.That(cs9.EndsWith(cs1), Is.False) 
        Assert.That(cs1.EndsWith(cscd), Is.True) 
        Assert.That(csstr1.EndsWith(cscd), Is.True) 
        Assert.That(cs7.EndsWith(cscd), Is.True) 
        Assert.That(cs8.EndsWith(cscd), Is.True) 
        Assert.That(csspace.EndsWith(cs1), Is.False)
        Assert.That(csempty.EndsWith(cs1), Is.False)
        Assert.That(csempty.EndsWith(csempty), Is.False)

    [<Test>]
    let EndsWithCS() =
        let cs1     = CCSet(abcd1)
        let cs2     = CCSet(abcd2)
        let cs3     = CCSet(abcd3)
        let cs4     = CCSet(abcd4)
        let cs5     = CCSet(abcd5)
        let cs6     = CCSet(abcd6)
        let cs7     = CCSet(abcd7)
        let cs8     = CCSet(abcd8)
        let cs9     = CCSet(abcd9)
        let csstr1  = CCSet(str1)
        let cscd    = CCSet(cd)
        let csspace = CCSet(space)
        let csempty = CCSet(empty)
        Assert.That(cs1.EndsWith(cs2), Is.True)
        Assert.That(cs2.EndsWith(cs1), Is.True)
        Assert.That(cs3.EndsWith(cs1), Is.False)
        Assert.That(cs4.EndsWith(cs1), Is.False)
        Assert.That(cs5.EndsWith(cs1), Is.False)
        Assert.That(cs6.EndsWith(cs1), Is.False)
        Assert.That(cs7.EndsWith(cs1), Is.False)
        Assert.That(cs8.EndsWith(cs1), Is.False)
        Assert.That(cs9.EndsWith(cs1), Is.False) 
        Assert.That(cs1.EndsWith(cscd), Is.True) 
        Assert.That(csstr1.EndsWith(cscd), Is.True) 
        Assert.That(cs7.EndsWith(cscd), Is.True) 
        Assert.That(cs8.EndsWith(cscd), Is.True) 
        Assert.That(csspace.EndsWith(cs1), Is.False)
        Assert.That(csempty.EndsWith(cs1), Is.False)
        Assert.That(csempty.EndsWith(csempty), Is.False)

    [<Test>]
    let EqualsFS() =
        let cs1 = FCSet(abcd1)
        let cs2 = FCSet(abcd2)
        let cs3 = FCSet(abcd3)
        let cs4 = FCSet(abcd4)
        let cs5 = FCSet(abcd5)
        let cs6 = FCSet(abcd6)
        let cs7 = FCSet(abcd7)
        let cs8 = FCSet(abcd8)
        let cs9 = FCSet(abcd9)
        let csspace = CCSet(space)
        let csempty = CCSet(empty) 
        Assert.That(cs1.Equals(cs2), Is.True)
        Assert.That(cs2.Equals(cs1), Is.True)
        Assert.That(cs3.Equals(cs1), Is.False)
        Assert.That(cs4.Equals(cs1), Is.False)
        Assert.That(cs5.Equals(cs1), Is.False)
        Assert.That(cs6.Equals(cs1), Is.False)
        Assert.That(cs7.Equals(cs1), Is.False)
        Assert.That(cs8.Equals(cs1), Is.False)
        Assert.That(cs9.Equals(cs1), Is.False)
        Assert.That(csspace.Equals(csspace), Is.True)
        Assert.That(csspace.Equals(cs1), Is.False)
        Assert.That(csempty.Equals(cs1), Is.False)
        Assert.That(csempty.Equals(csempty), Is.True)

    [<Test>]
    let EqualsCS() =
        let cs1 = CCSet(abcd1)
        let cs2 = CCSet(abcd2)
        let cs3 = CCSet(abcd3)
        let cs4 = CCSet(abcd4)
        let cs5 = CCSet(abcd5)
        let cs6 = CCSet(abcd6)
        let cs7 = CCSet(abcd7)
        let cs8 = CCSet(abcd8)
        let cs9 = CCSet(abcd9)   
        let csspace = CCSet(space)
        let csempty = CCSet(empty)      
        Assert.That(cs1.Equals(cs2), Is.True)
        Assert.That(cs2.Equals(cs1), Is.True)
        Assert.That(cs3.Equals(cs1), Is.False)
        Assert.That(cs4.Equals(cs1), Is.False)
        Assert.That(cs5.Equals(cs1), Is.False)
        Assert.That(cs6.Equals(cs1), Is.False)
        Assert.That(cs7.Equals(cs1), Is.False)
        Assert.That(cs8.Equals(cs1), Is.False)
        Assert.That(cs9.Equals(cs1), Is.False)     
        Assert.That(csspace.Equals(csspace), Is.True)
        Assert.That(csspace.Equals(cs1), Is.False)
        Assert.That(csempty.Equals(cs1), Is.False)
        Assert.That(csempty.Equals(csempty), Is.True)
        

    [<Test>]
    let ComposeFS() =
        let cs1 = FCSet(abcd1)
        let cs2 = FCSet(abcd2)
        let cs3 = FCSet(abcd3)
        let cs4 = FCSet(abcd4)
        let cs5 = FCSet(abcd5)
        let cs6 = FCSet(abcd6)
        let cs7 = FCSet(abcd7)
        let cs8 = FCSet(abcd8)
        let cs9 = FCSet(abcd9)
        Assert.That(cs1.Compose, Is.EqualTo(abcd1))
        Assert.That(cs2.Compose, Is.EqualTo(abcd1))
        Assert.That(cs2.Compose, Is.Not.EqualTo(abcd3))
        Assert.That(cs3.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs4.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs5.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs6.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs7.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs8.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs9.Compose, Is.Not.EqualTo(abcd1))
        

    [<Test>]
    let ComposeCS() =
        let cs1 = CCSet(abcd1)
        let cs2 = CCSet(abcd2)
        let cs3 = CCSet(abcd3)
        let cs4 = CCSet(abcd4)
        let cs5 = CCSet(abcd5)
        let cs6 = CCSet(abcd6)
        let cs7 = CCSet(abcd7)
        let cs8 = FCSet(abcd8)
        let cs9 = FCSet(abcd9)
        Assert.That(cs1.Compose, Is.EqualTo(abcd1))
        Assert.That(cs2.Compose, Is.EqualTo(abcd1))
        Assert.That(cs2.Compose, Is.Not.EqualTo(abcd3))
        Assert.That(cs3.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs4.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs5.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs6.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs7.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs8.Compose, Is.Not.EqualTo(abcd1))
        Assert.That(cs9.Compose, Is.Not.EqualTo(abcd1))