namespace FileFinderTests
//Op: Auto
open FileFinder.Finder
open FileFinder.Rules


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

module Likeness =
    let FileResultLikeness (x: Result<FileFinder.Finder.FindResults, string>) =
        x
        |> Result.map _.ExistingFiles

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
                        @"{CurDir}\TestFiles\{name}Other.txt"
                    ]
                }

        let substitutions =
            [
                "name", "a"
            ]
            |> Map.ofList

        let findFiles = getFinderForCurrentDirectory rules
        
        let exp = Ok [relativePath @"TestFiles\a.txt"]

        findFiles "ruleA" substitutions
        |> Likeness.FileResultLikeness
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
        
    [<Fact>]
    let ``Finder - shared substitutions are overwritten by specific substitutions`` () =
        let rules =
            Map.empty
            |> Map.add "ruleA"
                {
                    Patterns = [
                        @"{x}"
                    ]
                }

        let sharedSubstitutions =
            [
                "x", "shared"
            ]
            |> Map.ofSeq
            
        let overrideSubs =
            [
                "x", "override"
            ]
            |> Map.ofSeq

        let act = FileFinder.Finder.FindFiles rules sharedSubstitutions "ruleA" overrideSubs

        let exp =
            Ok { ExistingFiles = []
                 UnmatchedPatterns = ["override"] }

        act
        |> shouldEqual exp
