namespace FileFinder 
module Finder =
    open Rules
    type FindResults =
        {
            ExistingFiles: string list
            UnmatchedPatterns: string list
        }
    
    type Finder (rules: RuleSet) = 
        member this.FindFiles (ruleName: RuleName) (substitutions: Substitutions) : Result<FindResults, string> =
            let rule = rules |> Map.tryFind ruleName 
            rule
            |> function
            | Some x -> Ok x
            | None -> $"Invalid rule name: %s{ruleName}" |> Error
            |> Result.map ^ fun rule -> ApplyFactorsToRule rule substitutions
            |> Result.map 
                ^ fun lines -> 
                    lines
                    |> List.partition System.IO.File.Exists
            |> Result.map 
                ^ fun (existingFiles, unmatchedPatterns) ->
                    {
                        ExistingFiles = existingFiles
                        UnmatchedPatterns = unmatchedPatterns 
                    }
            |> fun x -> x
