namespace FileFinderTests
//Op: Auto
open FileFinder.Rules

open FsUnit
open Xunit
//Op: End
module RulesTests =
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

    [<Fact>]
    let ``ResolvePatterns - x`` () =
        let factors = 
            [
                {
                    PlaceHolder = "Controller"
                    Value = "Home"
                }
                {
                    PlaceHolder = "Action"
                    Value = "Index"
                }
            ]

        ResolvePatterns ViewRule factors
        |> should equal 
            [
                "Views\Home\Index.cshtml"
                "Views\Home\Index_{SubAction}.cshtml"
                "Areas\{Area}\Views\Home\Index.cshtml"
                "Areas\{Area}\Views\Home\Index_{SubAction}.cshtml"
            ]