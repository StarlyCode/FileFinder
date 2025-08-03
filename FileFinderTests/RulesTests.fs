namespace FileFinderTests
//Op: Auto
open FileFinder.Rules

open FsUnit
open Xunit
//Op: End
module RulesTests =
    [<Fact>]
    let ``ResolvePatterns - x`` () =
        let viewRule =
            {
                Patterns = [
                    @"{Cricket.Intranet}\Views\{Controller}\{Action}.cshtml"
                    @"{Cricket.Intranet}\Views\{Controller}\{Action}_{SubAction}.cshtml"
                    @"{Cricket.Intranet}\Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                    @"{Cricket.Intranet}\Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
                ]
            }

        let substitutions =
            [
                "Cricket.Intranet", @"C:\Dev\WesternCap\Cricket.Intranet"
                "Controller", "Home"
                "Action", "Index"
            ]
            |> Map.ofList

        ResolvePatterns viewRule substitutions
        |> should equal
            [
                "C:\Dev\WesternCap\Cricket.Intranet\Views\Home\Index.cshtml"
                "C:\Dev\WesternCap\Cricket.Intranet\Views\Home\Index_{SubAction}.cshtml"
                "C:\Dev\WesternCap\Cricket.Intranet\Areas\{Area}\Views\Home\Index.cshtml"
                "C:\Dev\WesternCap\Cricket.Intranet\Areas\{Area}\Views\Home\Index_{SubAction}.cshtml"
            ]