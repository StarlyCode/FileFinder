namespace FileFinder 
module Finder =
    open Microsoft.Extensions.FileSystemGlobbing.Abstractions
    open Rules

    type FindResults =
        {
            ExistingFiles: string list
            UnmatchedPatterns: string list
        }

    type TempResult = 
        {
            Line: string
            ExistingFiles: string list
        }

    type Finder (rules: RuleSet, sharedSubstitutions: Substitutions) = 
        member this.FindFiles (ruleName: RuleName) (substitutions: Substitutions) : Result<FindResults, string> =
            let rule = rules |> Map.tryFind ruleName 
            rule
            |> function
            | Some x -> Ok x
            | None -> $"Invalid rule name: %s{ruleName}" |> Error
            |> Result.map ^ fun rule -> ResolvePatterns rule substitutions
            |> Result.map 
                ^ fun lines -> 
                    lines
                    |> List.map 
                        ^ fun line -> 
                            let m = Microsoft.Extensions.FileSystemGlobbing.Matcher()
                            let fi = new System.IO.FileInfo(line)
                            let z = line.Replace(fi.Directory.Root.FullName, "").Replace("\\", "/")
                            //let z = (line.Replace("\\", "/"))
                            m.AddInclude z |> ignore
                            {
                                Line = line
                                ExistingFiles = 
                                    m.Execute(new DirectoryInfoWrapper(fi.Directory.Root))
                                    |> _.Files
                                    |> Seq.map (fun x -> x.Path)
                                    |> Seq.map ^ fun x -> System.IO.Path.Combine(fi.Directory.Root.FullName, x).Replace("/", "\\")
                                    |> List.ofSeq
                            }
            |> Result.map 
                ^ fun x ->
                    {
                        ExistingFiles = x |> List.collect (fun x -> x.ExistingFiles)
                        UnmatchedPatterns = x |> List.filter (fun x -> x.ExistingFiles.Length = 0) |> List.map (fun x -> x.Line) 
                    }
            |> fun x -> x
