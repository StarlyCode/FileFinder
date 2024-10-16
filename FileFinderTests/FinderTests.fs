namespace FileFinderTests
//Op: Auto
open FileFinder.Rules
open FileFinder.Finder

open FsUnitTyped
//open FsUnit
open Xunit
//Op: End
module FinderTests =
    let serialize = Newtonsoft.Json.JsonConvert.SerializeObject
    let AreEqualWinMerge exp act =
        let exp = exp |> serialize
        let act = act |> serialize
        WinMergeEquals.WinMergeEquals.AreEqualWinMerge exp act WinMergeEquals.WhitespaceSimplify.None "Expected" "Actual"
        
    let viewRule =
        {
            PossibleDirs = [@"C:\Dev\WesternCap\Cricket.Intranet"]
            Patterns = [
                @"Views\{Controller}\{Action}.cshtml"
                @"Views\{Controller}\{Action}_{SubAction}.cshtml"
                @"Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                @"Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
            ]
        }

    let cricketFileFinder = 
        Map.empty
        |> Map.add "ASP.NET View HTML" viewRule
        |> FileFinder.Finder.Finder

    [<Fact>]
    let ``Finder - Cricket View - Valid - Finds`` () =
        let substitutions = 
            [
                "Controller", "Home"
                "Action", "Index"
            ]
            |> Map.ofList

        let exp = 
             Ok
              { ExistingFiles =
                 ["C:\Dev\WesternCap\Cricket.Intranet\Views\Home\Index.cshtml"]
                UnmatchedPatterns =
                 ["C:\Dev\WesternCap\Cricket.Intranet\Views\Home\Index_{SubAction}.cshtml";
                  "C:\Dev\WesternCap\Cricket.Intranet\Areas\{Area}\Views\Home\Index.cshtml";
                  "C:\Dev\WesternCap\Cricket.Intranet\Areas\{Area}\Views\Home\Index_{SubAction}.cshtml"] }

        let act = cricketFileFinder.FindFiles "ASP.NET View HTML" substitutions
        //AreEqualWinMerge exp act

        act
        |> shouldEqual exp
