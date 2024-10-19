namespace FileFinderTests
//Op: Auto
open FileFinder.Finder
open FileFinder.Rules

open TypeExtensions

open FsUnitTyped
open Xunit
//Op: End
module RelativeFinder =
    let cd = System.IO.Directory.GetCurrentDirectory()

    let getFinderForCurrentDirectory(rules) =
        let sharedSubstitutions =
            [
                "CurDir", cd
            ]
            |> Map.ofSeq

        FileFinder.Finder.FindFiles rules sharedSubstitutions

        //FileFinder.Finder.Finder(rules, sharedSubstitutions)

    let relativePath remainder = System.IO.Path.Combine(cd, remainder)

module FinderTests =
    open RelativeFinder
    [<Fact>]
    let ``Finder - single rule`` () =
        let rules =
            Map.empty
            |> Map.add "ruleA"
                {
                    Patterns = [
                        @"{CurDir}\TestFiles\{name}.txt"
                    ]
                }

        let substitutions =
            [
                "name", "a"
            ]
            |> Map.ofList

        let findFiles = getFinderForCurrentDirectory rules

        let exp =
            Ok
                {
                    ExistingFiles =
                        [
                            relativePath @"TestFiles\a.txt"
                        ]
                    UnmatchedPatterns = []
                }

        findFiles "ruleA" substitutions
        |> shouldEqual exp

    [<Fact>]
    let ``Finder - invalid rule name - error`` () =
        let rules =
            Map.empty
            |> Map.add "ruleA"
                {
                    Patterns = [
                        @"{CurDir}\TestFiles\{name}.txt"
                    ]
                }

        let substitutions =
            [
                "name", "a"
            ]
            |> Map.ofList

        let findFiles = getFinderForCurrentDirectory rules

        let exp = Error "Invalid rule name: badName"

        findFiles "badName" substitutions
        |> shouldEqual exp

        
    [<Fact>]
    let ``Finder - empty rule set -`` () =
        let rules =
            Map.empty

        shouldFail<System.Exception> 
            (fun () -> 
                let sharedSubstitutions =
                    [
                        "CurDir", cd
                    ]
                    |> Map.ofSeq

                FileFinder.Finder.FindFiles rules sharedSubstitutions
                |> ignore
            )
        