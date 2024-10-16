namespace FileFinder 
module Finder =
    open Rules
    type FindResults =
        {
            ExistingFiles: string list
            UnmatchedPatterns: string list
        }
    type Finder (rules: Rule list) = 
        member this.FindFiles ruleName (factors: ConventionFactor list) : Result<FindResults, string> =
            let rule = rules |> List.tryFind ^ fun x -> x.Name = ruleName
            rule
            |> function
            | Some x -> Ok x
            | None -> $"Invalid rule name: %s{ruleName}" |> Error
            |> Result.map ^ fun rule -> ApplyFactorsToRule rule factors
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
