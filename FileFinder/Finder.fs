namespace FileFinder
module Finder =
    open Microsoft.Extensions.FileSystemGlobbing.Abstractions
    open System.IO
    open Rules

    type FindResults =
        {
            ExistingFiles: string list
            UnmatchedPatterns: string list
        }

    type private GlobResults =
        {
            Line: string
            ExistingFiles: string list
        }

    let private getGlobHits (lines: string list) =
        lines
        |> List.map
            ^ fun line ->
                let m = Microsoft.Extensions.FileSystemGlobbing.Matcher(System.StringComparison.OrdinalIgnoreCase)
                let fi = new FileInfo(line)
                let rootName = fi.Directory.Root.FullName
                let glob = line.Replace(rootName, "").Replace("\\", "/")
                {
                    Line = line
                    ExistingFiles =
                        m.AddInclude(glob).Execute(new DirectoryInfoWrapper(new DirectoryInfo(rootName)))
                        |> _.Files
                        |> Seq.map (fun x -> x.Path)
                        |> Seq.map ^ fun x -> Path.Combine(rootName, x).Replace("/", "\\")
                        |> List.ofSeq
                }

    let private globHitsToFindResults (x: GlobResults list) =
        {
            ExistingFiles = x |> List.collect (fun x -> x.ExistingFiles)
            UnmatchedPatterns = x |> List.filter (fun x -> x.ExistingFiles.Length = 0) |> List.map (fun x -> x.Line)
        }

    let FindFiles (rules: RuleSet) (sharedSubstitutions: Substitutions) : (RuleName -> Substitutions -> Result<FindResults, string>) =
        if Map.isEmpty rules then raise (new System.ArgumentException("Ruleset must not be empty", "rules"))

        fun (ruleName: RuleName) (substitutions: Substitutions) ->
            let combinedSubstitutions = substitutions |> Seq.fold (fun x y -> x |> Map.add y.Key y.Value) sharedSubstitutions
            let rule = rules |> Map.tryFind ruleName
            rule
            |> function
            | Some x -> Ok x
            | None -> $"Invalid rule name: %s{ruleName}" |> Error
            |> Result.map ^ fun rule -> ResolvePatterns rule combinedSubstitutions
            |> Result.map getGlobHits
            |> Result.map globHitsToFindResults
            |> Result.map (fun x -> { x with ExistingFiles = x.ExistingFiles |> List.distinct})