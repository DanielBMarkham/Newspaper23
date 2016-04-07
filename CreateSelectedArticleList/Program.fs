module Main
    open Types
    open Utils
    open Persist
    open System.Net

    let defaultBaseOptions = createNewBaseOptions "CreateSelectedArticleList" "Create the current article list to be shared." [|"Take the article libraries, apply the selection criteria in the config file, and create the article list to show the user."|] defaultVerbosity
    let defaultConfigFileName = createNewConfigEntry "C" "Config File To Use" [|"/C:<filename> -> Config File to use for selection criteria."|] "getAllLinks.libraryprocessconfig"
    let defaultOutputFileName = createNewConfigEntry "O" "Config File To Use" [|"/O:<filename> -> Output File. Where to send the link list."|] "getAllLinks.links"
    let defaultRippedSubdirectory = createNewConfigEntry "R" "Ripped Subdirectory" [|"/R:<filename> -> Ripped Subdirectory. What local prefix to use with the local link files with.";"For use when using the Use Local Link option"|] "ripped"
    let defaultUseLocalLinks = createNewConfigEntry "L" "Use Local Links (bool)" [|"/L:<bool> -> Whether or not to create local links or original links. If you use the flag without a true/false indicator, it assumes yes. Otherwise defaults to false."|] false
    let defaultLocalLinksPrefix = createNewConfigEntry "P" "Local Links Prefix" [|"/P:<string> -> URI Prefix. What to web prefix to use with the local files.";"For use when using the Use Local Link option"|] ""
    let defaultConfigFileExists = false
    let defaultOutputFileExists = false
        
    let loadConfigFromCommandLine (args:string []) =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigFileName=ConfigEntry<_>.populateValueFromCommandLine(defaultConfigFileName, args)
        let newOutputFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFileName, args)
        let newRippedSubdirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultRippedSubdirectory, args)
        let newUseLocalLinks = ConfigEntry<_>.populateValueFromCommandLine(defaultUseLocalLinks, args)
        let newLocalLinksPrefix = ConfigEntry<_>.populateValueFromCommandLine(defaultLocalLinksPrefix, args)
        let newConfigFileExists = System.IO.File.Exists(newConfigFileName.parameterValue)
        let newOutputFileExists = System.IO.File.Exists(newOutputFileName.parameterValue)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            configFileName=newConfigFileName
            outputFileName=newOutputFileName
            rippedSubdirectory=newRippedSubdirectory
            useLocalLinks=newUseLocalLinks
            localLinksPrefix=newLocalLinksPrefix
            configFileExists=newConfigFileExists
            outputFileExists=newOutputFileExists
        }


    let loadLibrary (opts:CreateSelectedArticleListProgramConfig) libraryFileName =
        if opts.configBase.verbose.parameterValue>Verbosity.Normal then printfn "Opening library file %A" libraryFileName
        let inputFileLines, modTime = 
            if System.IO.File.Exists(libraryFileName)
            then
                System.IO.File.ReadAllLines(libraryFileName), System.IO.FileInfo(libraryFileName).LastWriteTimeUtc
            else
                [||], System.DateTime.UtcNow
        let inLines = inputFileLines |>Array.toSeq
        let linkTriples = inLines |> Utils.toSeqTriplesFast |> Seq.toList
        if opts.configBase.verbose.parameterValue>Verbosity.Normal then printfn "There are %A triples in the library %A" linkTriples.Length libraryFileName
        let linkDict = new System.Collections.Generic.Dictionary<string, titleTimeTuple>()
        let titleDict = new System.Collections.Generic.Dictionary<string, linkTimeTuple>()
        linkTriples |> List.iter(fun (a,b,c)->
            if (linkDict.ContainsKey(b)=false) && (titleDict.ContainsKey(a) = false) then
                linkDict.Add(b,{title=a;time=c})
                titleDict.Add(a,{link=b;time=c}))
        if opts.configBase.verbose.parameterValue>Verbosity.Normal then printfn "Filtering for dupes in the library, there are now %A library entries to process" linkDict.Count
        {
            fileLastModified=modTime
            initialList=linkTriples
            byLink=linkDict
            byTitle=titleDict
        }

    let getConfigSectionData (opts:CreateSelectedArticleListProgramConfig) (sConfigSection:string) configFile =
            let configSectionTextLines = sConfigSection.Split([|"\r\n"|], System.StringSplitOptions.None)
            let configSectionTitle = configSectionTextLines.[0].TrimBoth 1 1
            match configSectionTitle with  
                | "Meta" ->
                    configSectionTextLines |> Array.iteri(fun i x->
                        match i with
                            | 0 ->
                                printfn ""
                            | _ ->
                                let lineSplit = x.Split([|':'|])
                                configFile.metaSection.Add(lineSplit.[0],lineSplit.[1]))
                | _ ->
                    let ls = {libraryEntries= new System.Collections.Generic.List<libraryEntry>()}
                    configFile.librarySections.Add(configSectionTitle, ls)
                    configSectionTextLines |> Array.iteri(fun i x->
                        match i with
                            | 0 ->
                                printfn ""
                            | _ ->
                                let lineSplit = x.Split([|','|])
                                let tempSectionName = lineSplit.[0]
                                let tempNumberOfLinksDesired = 
                                    try
                                        System.Int32.Parse(lineSplit.[1])
                                    with _ -> 0
                                let tempLibraryFile = lineSplit.[2].Trim()
                                configFile.librarySections.Item(configSectionTitle).libraryEntries.Add(
                                    {SectionName = tempSectionName; NumberOfLinksDesired=tempNumberOfLinksDesired; LibraryFile = tempLibraryFile}
                                )
                    )
            configFile

    let getConfigData (opts:CreateSelectedArticleListProgramConfig) =
        let configFile = 
            if opts.configFileExists = false
                then
                    ""
                else
                    try
                        System.IO.File.ReadAllText(opts.configFileName.parameterValue)
                    with _->
                        raise (ExpectedResponseFail("Had a problem locating and loading the configuration file at: " + opts.configFileName.parameterValue))
                        ""
        let configFileTemp = {        
                                metaSection= new System.Collections.Generic.Dictionary<string,string>();
                                librarySections= new System.Collections.Generic.SortedList<string, CreateSelectedArticleListConfigLibrarySection>()
                                }
        let sections = configFile.Split([|"\r\n\r\n"|], System.StringSplitOptions.None)
        let rec recAddSections fileLines configFileTemp = 
            match fileLines with
                | [] -> configFileTemp
                | x::xs ->
                    let newConfigFileTemp = getConfigSectionData opts x configFileTemp
                    recAddSections xs newConfigFileTemp
        let returnConfig = recAddSections (Array.toList sections) configFileTemp
        if opts.configBase.verbose.parameterValue>Verbosity.Normal then
            printfn "Configuration Read From File"
            printfn "%A" returnConfig
            printfn ""  
        returnConfig
    
    let doStuff (opts:CreateSelectedArticleListProgramConfig) =
        let configFile = getConfigData opts
        let hoursToRemember = 
            let confightr = 
                try
                    configFile.metaSection.Item("HoursToRemember")
                with _ -> "48.0"
            let htrParsed, htr = Microsoft.FSharp.Core.float.TryParse(confightr) 
            if htrParsed then htr else 48.0
        let outputBuff = new System.Text.StringBuilder(65535)

        configFile.librarySections |> Seq.iteri(fun i x->
            outputBuff.Append ("\r\n\r\n[" + x.Key + "]\r\n") |> ignore
            x.Value.libraryEntries |> Seq.iter(fun y->
                outputBuff.Append ("\r\n<" + y.SectionName + ">\r\n") |> ignore
                try
                    let libName = y.LibraryFile
                    let origLibrary = loadLibrary opts y.LibraryFile
                    let initialArticleCount = origLibrary.byTitle.Count
                    if opts.configBase.verbose.parameterValue>Verbosity.Normal then
                        printfn "library %A loaded" libName
                        printfn "%A Article Count in initial library" initialArticleCount
                    let articlesPerSiteDesired = 
                        y.NumberOfLinksDesired
                    let articlesPerSite = 
                        if articlesPerSiteDesired > initialArticleCount then initialArticleCount else articlesPerSiteDesired
                    let beginSlice = initialArticleCount - articlesPerSite
                    let takeCount = articlesPerSite
                    let slicedList = origLibrary.byTitle |> Seq.skip beginSlice |> Seq.take takeCount |> Seq.toList
                    let orderedFilteredList = slicedList |> List.filter(fun a->
                        let x, y, z = a.Key, a.Value.link, a.Value.time
                        let bParseWorked, articleDate = System.DateTime.TryParse(z)
                        if bParseWorked = false then raise (System.Exception("Date Parsing Failed for '" + x + "'. Supposed date value was '" + z + "'"))
                        let hoursDiff = System.DateTime.UtcNow.Subtract(articleDate)
                        hoursDiff.TotalHours < hoursToRemember
                        )
                    let slicedOrderedFilteredList = orderedFilteredList |> List.rev
                    slicedOrderedFilteredList |> List.iter(fun a->
                        let title,link,time = a.Key, a.Value.link, a.Value.time
                        outputBuff.Append(title + "\r\n") |> ignore
                        if opts.useLocalLinks.parameterValue
                            then
                                if opts.localLinksPrefix.parameterValue.Length > 0
                                    then
                                        outputBuff.Append ((opts.localLinksPrefix.parameterValue + Utils.makeUrlIntoAFilename2 (link, title)) + "\r\n") |> ignore
                                    else
                                        outputBuff.Append ((opts.rippedSubdirectory.parameterValue + "/" + Utils.makeUrlIntoAFilename2 (link, title)) + "\r\n") |> ignore
                            else
                                outputBuff.Append (link + "\r\n") |> ignore
                        )
                    // Report interim progress
                    match opts.configBase.verbose.parameterValue with
                        | Verbosity.Silent ->
                            ()
                        | Verbosity.BatchMinimum ->
                            ()
                        | Verbosity.Minimum ->
                            printfn "LIBRARY: %s" y.LibraryFile
                        | Verbosity.BatchNormal ->
                            printfn "LIBRARY: %s" y.LibraryFile
                            printfn "%A articles after final ordering and slicing" slicedOrderedFilteredList.Length
                        | Verbosity.Normal ->
                            printfn "LIBRARY: %s" y.LibraryFile
                            printfn "%A articles gathered from library" articlesPerSite
                            printfn "%A articles after final ordering and slicing" slicedOrderedFilteredList.Length
                        | Verbosity.BatchVerbose ->
                            printfn "LIBRARY: %s" y.LibraryFile
                            printfn "%A articles gathered from library" articlesPerSite
                            printfn "%A articles after slicing" slicedList.Length
                            printfn "%A articles after filtering for article date" orderedFilteredList.Length
                            printfn "%A articles after final ordering and slicing" slicedOrderedFilteredList.Length
                        | Verbosity.Verbose ->
                            printfn "LIBRARY: %s" y.LibraryFile
                            printfn "%A articles gathered from library" articlesPerSite
                            printfn "%A articles after slicing" slicedList.Length
                            printfn "%A articles after filtering for article date" orderedFilteredList.Length
                            printfn "%A articles after final ordering and slicing" slicedOrderedFilteredList.Length
                        |_ ->
                            ()
                with
                    | :? System.Exception as ex ->
                        outputBuff.Append("Error: " + ex.Message + "\r\n") |> ignore
                        outputBuff.Append ("http://google.com" + "\r\n") |> ignore
                        if opts.configBase.verbose.parameterValue>Verbosity.Normal then
                            let err = "Error: " + ex.Message + "\r\n"
                            System.Console.WriteLine err
                            let err2 = "Inner Error: " + ex.InnerException.Message + "\r\n"
                            System.Console.WriteLine err2
                        ()
                )
        )
        System.IO.File.WriteAllText(opts.outputFileName.parameterValue, outputBuff.ToString())

    [<EntryPoint>]
    let main argv = 
        try
            let opts = loadConfigFromCommandLine argv
            commandLinePrintWhileEnter opts.configBase (opts.printThis)
            doStuff opts
            commandLinePrintWhileExit opts.configBase
            0
        with
            | :? UserNeedsHelp as hex ->
                defaultBaseOptions.printThis
                0
            | :? System.Exception as ex ->
                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                System.Console.WriteLine (ex.StackTrace)
                if ex.InnerException = null
                    then
                        0
                    else
                        System.Console.WriteLine("---   Inner Exception   ---")
                        System.Console.WriteLine (ex.InnerException.Message)
                        System.Console.WriteLine (ex.InnerException.StackTrace)
                        0
