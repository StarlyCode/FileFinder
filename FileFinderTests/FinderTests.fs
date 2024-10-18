namespace FileFinderTests
//Op: Auto
open FileFinder.Rules
open FileFinder.Finder
open TypeExtensions
open FsUnitTyped
open Xunit
//Op: End
module FinderTests =
    let num1_viewRule =
        {
            Patterns = [
                @"{Cricket.Intranet}\Views\{Controller}\{Action}.cshtml"
                @"{Cricket.Intranet}\Views\{Controller}\{Action}_{SubAction}.cshtml"
                @"{Cricket.Intranet}\Areas\{Area}\Views\{Controller}\{Action}.cshtml"
                @"{Cricket.Intranet}\Areas\{Area}\Views\{Controller}\{Action}_{SubAction}.cshtml"
            ]
        }
        
    let num2_SQL =
        {
            Patterns = [
                @"{Cricket.Intranet.FSharp.Data}\{Controller}_{Action}_Data.sql"
                @"{Cricket.Intranet.FSharp}\Controllers\{Controller}_{Action}.sql"
            ]
        }
        
    let num2_SQLAlt =
        {
            Patterns = [
                @"{Cricket.Intranet.FSharp.Data}\{Controller}_{Action}_Data.*.sql"
                @"{Cricket.Intranet.FSharp}\Controllers\{Controller}_{Action}.*.sql"
            ]
        }
        
    let num3_FSharpController =
        {
            Patterns = [
                @"{Cricket.Intranet.FSharp}\Controllers\{Controller}_{Action}.fs"
            ]
        }
        
    let num4_TS =
        {
            Patterns = [
                @"{Cricket.Intranet}\Views\{Controller}_{Action}.ts"
                @"{Cricket.Intranet}\Areas\{Area}\Views\{Controller}_{Action}.ts"
            ]
        }
        
    let num5_ControllerAction =
        {
            Patterns = [
                @"{Cricket.Intranet}\Controllers\{Controller}_{Action}.cs"
                @"{Cricket.Intranet}\Areas\{Area}\Controllers\{Controller}_{Action}.cs"
            ]
        }
        
    let num6_ControllerBase =
        {
            Patterns = [
                @"{Cricket.Intranet}\Controllers\{Controller}.cs"
                @"{Cricket.Intranet}\Areas\{Area}\Controllers\{Controller}.cs"
            ]
        }
        
    let num7_FSharpControllerData =
        {
            Patterns = [
                @"{Cricket.Intranet.FSharp.Data}\{Controller}_{Action}.fs"
                @"{Cricket.Intranet.FSharp.Data}\{Controller}_{Action}_Data.fs"
            ]
        }

    let cricketFileFinder = 
        let rules = 
            Map.empty
            |> Map.add "Cricket.Intranet View HTML" num1_viewRule
            |> Map.add "Cricket.Intranet FSharp SQL" num2_SQL
            |> Map.add "Cricket.Intranet FSharp SQL Alt" num2_SQLAlt
            |> Map.add "Cricket.Intranet FSharp Controller" num3_FSharpController
            |> Map.add "Cricket.Intranet View Typescript" num4_TS
            |> Map.add "Cricket.Intranet Controller Action" num5_ControllerAction
            |> Map.add "Cricket.Intranet Controller Base" num6_ControllerBase
            |> Map.add "Cricket.Intranet FSharp Controller Data" num7_FSharpControllerData

        let sharedSubstitutions = 
            [
                "Cricket.Intranet.FSharp.Data", @"C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data"
                "Cricket.Intranet.FSharp", @"C:\Dev\WesternCap\Cricket.Intranet.FSharp"
                "Cricket.Intranet", @"C:\Dev\WesternCap\Cricket.Intranet"
            ]
            |> Map.ofSeq

        FileFinder.Finder.Finder(rules, sharedSubstitutions)

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

        let act = cricketFileFinder.FindFiles "Cricket.Intranet View HTML" substitutions
        //AreEqualWinMerge exp act

        act
        |> shouldEqual exp
        
    [<Fact>]
    let ``Finder - Cricket View - Velocity/WhiteboardLocation - Finds`` () =
        let substitutions = 
            [
                "Controller", "Velocity"
                "Action", "WhiteboardLocation"
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

        let act = cricketFileFinder.FindFiles "Cricket.Intranet View HTML" substitutions
        //AreEqualWinMerge exp act

        act
        |> shouldEqual exp
        
    [<Fact>]
    let ``Finder - Cricket.Intranet FSharp SQL - Valid - Finds`` () =
        let substitutions = 
            [
                "Controller", "Accessory"
                "Action", "OrderItem"
            ]
            |> Map.ofList
            
        let exp = 
            Ok
              { ExistingFiles =
                 ["C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.sql"]
                UnmatchedPatterns =
                 ["C:\Dev\WesternCap\Cricket.Intranet.FSharp\Controllers\Accessory_OrderItem.sql"] }

        let act = cricketFileFinder.FindFiles "Cricket.Intranet FSharp SQL" substitutions
        //AreEqualWinMerge exp act

        act
        |> shouldEqual exp
        
    [<Fact>]
    let ``Finder - Cricket.Intranet FSharp SQL Alt - Valid - Finds`` () =
        let substitutions = 
            [
                "Controller", "Accessory"
                "Action", "OrderItem"
            ]
            |> Map.ofList
            
        let exp = 
            Ok
              { ExistingFiles =
                 ["C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.ApproveOrder.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.AssignToOrder.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.BudgetByStore.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.Delete.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.DeleteOrder.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.Insert.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.InsertOrder.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.Orders.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.RejectionReasons.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.RemoveFromOrder.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.UnapproveOrder.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.Update.sql";
                  "C:\Dev\WesternCap\Cricket.Intranet.FSharp.Data\Accessory_OrderItem_Data.Vendors.sql"]
                UnmatchedPatterns =
                 ["C:\Dev\WesternCap\Cricket.Intranet.FSharp\Controllers\Accessory_OrderItem.*.sql"] }
        
        let act = cricketFileFinder.FindFiles "Cricket.Intranet FSharp SQL Alt" substitutions
        //AreEqualWinMerge exp act

        act
        |> shouldEqual exp
        
    //[<Fact>]
    //let ``Finder - Dump Rules - x`` () =
    //    let substitutions = 
    //        [
    //            "Controller", "Accessory"
    //            "Action", "OrderItem"
    //        ]
    //        |> Map.ofList
            
    //    let exp = 
    //        Error [""]
        
    //    let act = 
    //        cricketFileFinder.Rules 
    //        |> Seq.map ^ fun r -> cricketFileFinder.FindFiles r.Key substitutions
    //        |> Result.collect
    //    //AreEqualWinMerge exp act

    //    act
    //    |> shouldEqual exp
        
    //[<Fact>]
    //let ``Finder - Bad Rule Name - Error`` () =
    //    cricketFileFinder.FindFiles "Bad name" Map.empty
    //    |> shouldEqual (Error "Invalid rule name: Bad name")
