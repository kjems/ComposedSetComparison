module ComposedSet.FSharpIdiomatic.ComposedSetIdiomaticOfString
    open System.Text.RegularExpressions
    open ComposedSetIdiomatic
    let parts          = ResizeArray<string>()
    let regex          = @"("")|(\])|(\[)|(\t)|(:)|(')|(;)|(-)|(\?)|(!)|(\r)|(\n)|(,)|(\ )|(\.)|(\/)|(\@)|(_)|(\f)"
    let split composed = Regex.Split(composed, regex, RegexOptions.Compiled) |> Array.filter (fun s -> not (System.String.IsNullOrEmpty s))
    let decompose      = decomposer parts split            : string -> Decomposed<string>
    let compose        = composer parts (String.concat "") : Decomposed<string> -> string