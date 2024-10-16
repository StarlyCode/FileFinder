namespace FileFinder 
module Rules =
    type Rule = 
        {
            //Name: string
            PossibleDirs: string list
            Patterns: string list
        }
    
    type RuleName = string
    
    type RuleSet = Map<RuleName, Rule>

    type ConventionFactor =
        {
            PlaceHolder: string
            Value: string
        }

    type Substitutions = Map<string, string>

    let ViewRule =
        {
            //Name = "ASP.NET View HTML"
            PossibleDirs = [@"C:\Dev\WesternCap\Cricket.Intranet\Views"]
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
                        
    let ApplyFactorsToRule (rule: Rule) (factors: Substitutions) =
        let patternsWithSubstitutions =
            ResolvePatterns rule factors

        let possibleFiles =
            rule.PossibleDirs
            |> List.collect 
                ^ fun dir -> 
                    patternsWithSubstitutions
                    |> List.map 
                        ^ fun pattern -> 
                            System.IO.Path.Combine(dir, pattern)

        possibleFiles