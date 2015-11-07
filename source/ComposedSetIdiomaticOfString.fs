module ComposedSet.FSharpIdiomatic.ComposedSetIdiomaticOfString
    open System.Text.RegularExpressions
    open ComposedSetIdiomatic
    open Common
    let parts          = ResizeArray<string>()
    let regex          = Regex(@"("")|(\])|(\[)|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)", RegexOptions.Compiled)
    let split composed = regex.Split composed |> Array.filter (System.String.IsNullOrEmpty >> not)
    let decompose      = decomposer parts split        : string -> Decomposed<string>
    let compose        = composer parts String.build   : Decomposed<string> -> string