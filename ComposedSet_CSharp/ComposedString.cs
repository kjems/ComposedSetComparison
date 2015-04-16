using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Text.RegularExpressions;

namespace ComposedSet.CSharp
{
    using ComposedString = ComposedSet<string, StringComposedSetDatabase>;
    public class StringComposedSetDatabase : BaseComposedSetDatabase<string>
    {
        const string regexSplitter = @"(\.)|(\/)|(\@)|(_)|(\f)";
        static Regex regex = new Regex(regexSplitter, RegexOptions.Compiled);
        public override string[] Split(string composed)
        {
            return regex.Split(composed).Where(s => !string.IsNullOrEmpty(s)).ToArray();
        }

        public override string Compose(List<int> indices)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0, length = indices.Count; i < length; ++i)
            {
                sb.Append(Parts[indices[i]]);
            }
            return sb.ToString();
        }
    }
}