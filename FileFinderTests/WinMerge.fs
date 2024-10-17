module WinMerge
    let serialize x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)
    let AreEqualWinMerge exp act =
        let exp = exp |> serialize
        let act = act |> serialize
        WinMergeEquals.WinMergeEquals.AreEqualWinMerge exp act WinMergeEquals.WhitespaceSimplify.None "Expected" "Actual"
        