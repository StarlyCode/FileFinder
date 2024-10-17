namespace FileFinder
module Rules =
    type Rule =
        {
            //Name: string
            Patterns: string list
        }

    type RuleName = string

    type RuleSet = Map<RuleName, Rule>

    type Placeholder = string

    type Substitutions = Map<Placeholder, string>

    let ViewRule =
        {
            //Name = "ASP.NET View HTML"
            Patterns = [
                @"Views\{Controller}\{Action}.cshtml"
                @"Views\{Controller}\{Action}_{SubAction}.cshtml"
                @"Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                @"Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
            ]
        }

    let inline (^) f a = f a

    let ResolvePatterns (rule: Rule) (substitutions: Substitutions) =
        rule.Patterns
        |> List.map
            ^ fun pattern ->
                substitutions
                |> Seq.fold
                    (fun (p: string) f -> p.Replace("{" + f.Key + "}", f.Value))
                    pattern