namespace FileFinder 
module Finder =
    open Rules
    type Finder (rules: Rule list) = 
        member this.FindFiles ruleName (factors: ConventionFactor list) =
            let rule = rules |> List.tryFind ^ fun x -> x.Name = ruleName
            //let rules = rules |> List.filter (fun r -> r.FactorMatches factors)
            //let files = rules |> List.collect (fun r -> r.FindFiles factors)
            //files
            ()