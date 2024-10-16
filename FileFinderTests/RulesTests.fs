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
                Name = "ASP.NET View HTML"
                PossibleDirs = [@"C:\Dev\WesternCap\Cricket.Intranet"]
                Patterns = [
                    @"Views\{Controller}\{Action}.cshtml"
                    @"Views\{Controller}\{Action}_{SubAction}.cshtml"
                    @"Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                    @"Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
                ]
            }

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

        ResolvePatterns viewRule factors
        |> should equal 
            [
                "Views\Home\Index.cshtml"
                "Views\Home\Index_{SubAction}.cshtml"
                "Areas\{Area}\Views\Home\Index.cshtml"
                "Areas\{Area}\Views\Home\Index_{SubAction}.cshtml"
            ]

    [<Fact>]
    let ``ApplyFactorsToRule - x`` () =
        let viewRule =
            {
                Name = "ASP.NET View HTML"
                PossibleDirs = [@"C:\Dev\WesternCap\Cricket.Intranet"]
                Patterns = [
                    @"Views\{Controller}\{Action}.cshtml"
                    @"Views\{Controller}\{Action}_{SubAction}.cshtml"
                    @"Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                    @"Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
                ]
            }

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

        ApplyFactorsToRule viewRule factors
        |> should equal 
            [
                "C:\Dev\WesternCap\Cricket.Intranet\Views\Home\Index.cshtml"
                "C:\Dev\WesternCap\Cricket.Intranet\Views\Home\Index_{SubAction}.cshtml"
                "C:\Dev\WesternCap\Cricket.Intranet\Areas\{Area}\Views\Home\Index.cshtml"
                "C:\Dev\WesternCap\Cricket.Intranet\Areas\{Area}\Views\Home\Index_{SubAction}.cshtml"
            ]