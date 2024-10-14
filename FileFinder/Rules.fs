namespace FileFinder 
module Rules =
    type Rule = 
        {
            Name: string
            PossibleDirs: string list
            Patterns: string list
        }

    type ConventionFactor =
        {
            PlaceHolder: string
            Value: string
        }

    let ViewRule =
        {
            Name = "ASP.NET View HTML"
            PossibleDirs = [@"C:\Dev\WesternCap\Cricket.Intranet\Views"]
            Patterns = [
                @"Views\{Controller}\{Action}.cshtml"
                @"Views\{Controller}\{Action}_{SubAction}.cshtml"
                @"Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                @"Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
            ]
        }
        
    let inline (^) f a = f a

    let ResolvePatterns (rule: Rule) (factors: ConventionFactor list) =
        rule.Patterns 
        |> List.map 
            ^ fun pattern -> 
                factors 
                |> List.fold 
                    (fun (p: string) f -> p.Replace("{" + f.PlaceHolder + "}", f.Value))
                    pattern
                        
    let ApplyFactorsToRule (rule: Rule) (factors: ConventionFactor list) =
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