module Main
    open Types
    open Utils
    open Persist
    open HtmlAgilityPack
    open System.Net

    let defaultBaseOptions = createNewBaseOptions "GetLinks" "Capture external links from a news or news aggregation site." [|"using a config crafted for each site, vists and finds as many links as desired."|] defaultVerbosity
    let defaultSiteUrl = createNewConfigEntry "S" "Site URL" [|"/S:<url> -> The URL to load."|] "http://www.news.ycombinator.com"
    let defaultSiteConfigFileName = createNewConfigEntry "C" "Config File" [|"/C:<filename> -> The file that tells us how to process that site."|] ""
    let defaultOutputFileName = createNewConfigEntry "O" "Output File" [|"/O:<filename> -> Where to put all the links we took."|] "temp.getlinksoutput"
    let defaultDesiredLinkCount = createNewConfigEntry "N" "Number of Links to Process" [|"/N:<count> -> How many links to scrape. If the main page doesn't have that many, load up subsequent pages.";"(Assuming the More Link has been defined in the config file)"|] 30
    let defaultSiteConfigFileExists=false

    let loadConfigFromCommandLine (args:string []):GetLinksProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newSiteUrl  =ConfigEntry<_>.populateValueFromCommandLine(defaultSiteUrl, args)
        let newSiteConfigFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultSiteConfigFileName, args)
        let newOutputFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFileName, args)
        let newDesiredLinkCount =ConfigEntry<_>.populateValueFromCommandLine(defaultDesiredLinkCount, args)
        let fixedSiteConfigFileName =
            if newSiteConfigFileName.parameterValue.Length=0 
                then
                    newSiteConfigFileName.swapInNewValue(newSiteUrl.parameterValue.Replace("/","-") + ".getlinksconfig")
                else
                    newSiteConfigFileName
        let newConfigFileExists=System.IO.File.Exists(fixedSiteConfigFileName.parameterValue)
        {
            configBase=newConfigBase
            siteUrl=newSiteUrl
            siteConfigFileName=fixedSiteConfigFileName
            outputFileName=newOutputFileName
            desiredLinkCount=newDesiredLinkCount
            siteConfigFileExists=newConfigFileExists
        }

    let ripLinksOnAPage (opts:GetLinksProgramConfig) config outputData (doc:HtmlDocument) =
        let interimMsg1 = new System.Text.StringBuilder(65535)
        config.configSections.Item("Paths").configEntries |> Seq.iter(fun x->
            interimMsg1.Append((sprintf "%A" x) + "\r\n") |> ignore
            let pathParms = x.Value.Split([|"|||"|],System.StringSplitOptions.None)
            let path = if pathParms.Length > 0 then pathParms.[0] else ""
            let pathExt = if pathParms.Length > 1 then pathParms.[1] else ""
            let temp = doc.DocumentNode.SelectNodes(path)
            let pNodes = if (temp<>null) then temp |> Seq.toArray else Array.empty
            let paras = pNodes |> Array.map(fun x->
                match pathExt with
                    | "InnerText" ->htmlTextClean x.InnerText
                    | "href" ->
                        if x.HasAttributes && x.Attributes.Contains("href")
                            then
                                let hrefTemp = x.Attributes.Item("href").Value.Trim()
                                try
                                    if (hrefTemp.Length < 8) || ((hrefTemp.Substring(0,7) <> "http://") && (hrefTemp.Substring(0,8) <> "https://"))
                                        then
                                            // if all we have is a werver-relative chunk instead of a full URL, we gotta rebuiild a full url
                                            let urlBegin = (config.configSections.Item("Meta").configEntries.Item("RootDomain")).Trim()
                                            if ((urlBegin.GetRight 1) = "/") || ((hrefTemp.GetLeft 1) = "/")
                                                then
                                                    "http://" + urlBegin + hrefTemp
                                                else
                                                    "http://" + urlBegin +  "/"  + hrefTemp
                                        else hrefTemp
                                with _-> raise (UserNeedsHelp("Your config file needs to have a RootDomain entry under the Meta Section in order for the system to resolve relative links"))
                            else
                                ""
                    | "" ->htmlTextClean x.InnerHtml
                    | _ -> htmlTextClean(x.Attributes.Item(pathExt).Value)
                    )
            // add paras to the output list
            let interimMsg2 = new System.Text.StringBuilder(65535)
            paras |> Array.iteri(fun i y->
                outputData.outputSections.Item(x.Key).outputList.Add(y)
                interimMsg2.Append(x.Key + " pulled from html") |> ignore
                interimMsg2.Append("\r\n" + (i.ToString()+". "+y.ToString()) + "\r\n") |> ignore
                )
            opts.configBase.interimProgress.addItem "KeysPulled" (interimMsg2.ToString())
            )
        opts.configBase.interimProgress.addItem "PathItemsPulled" (interimMsg1.ToString())
        outputData

    let ripLinks (opts:GetLinksProgramConfig) config outputData =
        let html = http ("http://" + opts.siteUrl.parameterValue) (config.configSections.Item("Meta").configEntries.stringValueOrEmptyForKey("UserAgent")) (config.configSections.Item("Meta").configEntries.stringValueOrEmptyForKey("Referrer"))
        opts.configBase.interimProgress.addItem "SiteAccessed" ("Page loaded from http://" + opts.siteUrl.parameterValue)
        opts.configBase.interimProgress.addItem "BytesReceived" ("HTML FROM " + "http://" + opts.siteUrl.parameterValue + " " + html.Length.ToString() + " bytes received")
        opts.configBase.interimProgress.addItem "FullHtml" html
        config.configSections.Item("Paths").configEntries |> Seq.iter(fun x->
            outputData.outputSections.Add(x.Key, {outputList=new System.Collections.Generic.List<string>()})
            )
        let doc = new HtmlDocument()
        doc.LoadHtml(html)
        let rec recBuildLinkListUp doc = 
            let outputData = ripLinksOnAPage opts config outputData doc
            let biggestList = outputData.outputSections |> Seq.maxBy(fun x->x.Value.outputList.Count)
            match biggestList.Value.outputList.Count with
                | var1 when var1<opts.desiredLinkCount.parameterValue ->
                    if config.configSections.Item("Meta").configEntries.ContainsKey("MoreLink") = false
                        then
                            outputData
                        else
                            let morePath = config.configSections.Item("Meta").configEntries.Item("MoreLink")
                            let moreLinkNodes = (doc.DocumentNode.SelectNodes(morePath))
                            if moreLinkNodes = null
                                then
                                    outputData
                                else
                                    let moreLinkTemp = moreLinkNodes.[0].Attributes.Item("href").Value
                                    let moreLink = 
                                        try
                                            if (moreLinkTemp.Length < 8) || ((moreLinkTemp.Substring(0,7) <> "http://") && (moreLinkTemp.Substring(0,8) <> "https://"))
                                                then 
                                                    "http://" + (config.configSections.Item("Meta").configEntries.Item("RootDomain")).Trim() + (if moreLinkTemp.Substring(0, 1) = "/"  then "" else "/") + moreLinkTemp
                                                    else moreLinkTemp
                                        with _-> raise (UserNeedsHelp("Your config file needs to have a RootDomain entry under the Meta Section in order for the system to resolve relative links"))
                                    let html = http (moreLink)  "Random" ""
                                    let newDoc = new HtmlDocument();
                                    newDoc.LoadHtml(html)
                                    recBuildLinkListUp newDoc
                | var1 when var1>=opts.desiredLinkCount.parameterValue ->
                    outputData            
        let outputData = recBuildLinkListUp doc
        // limit to only as many as we're asking for
        if outputData.outputSections.["ArticleLinks"].outputList.Count > opts.desiredLinkCount.parameterValue
            then
                outputData.outputSections |> Seq.iter(fun x->
                    x.Value.outputList.RemoveRange(opts.desiredLinkCount.parameterValue-1, x.Value.outputList.Count-opts.desiredLinkCount.parameterValue))
            else
                ()
        // write results to file
        let fw = System.IO.File.CreateText(opts.outputFileName.parameterValue)
        fw.NewLine<-"\r\n"
        fw.WriteLine("Links ripped from " + (config.configSections.Item("Meta").configEntries.Item("Name") + "").ToString() + " on " + System.DateTime.UtcNow.ToString())
        opts.configBase.interimProgress.addItem "SiteProcessed" ("Links ripped from " + (config.configSections.Item("Meta").configEntries.Item("Name") + "").ToString() + " on " + System.DateTime.UtcNow.ToString())
        outputData.outputSections |> Seq.iteri(fun i x->
            fw.WriteLine("")
            fw.WriteLine("")
            fw.WriteLine("[" + x.Key + "]")
            x.Value.outputList |> Seq.iteri(fun j y->
                // HACK. Somehow cr were getting in here inside the titles
                let b = y.Replace("\r", ". ")
                fw.WriteLine(b)
                )
            )
        fw.Flush();
        fw.Close()
        outputData
    let getConfigSectionData opts (sConfigSection:string) configFile =
            let configSectionTextLines = sConfigSection.Split([|"\r\n"|], System.StringSplitOptions.None)
            let configSectionTitle = configSectionTextLines.[0].TrimBoth 1 1
            configFile.configSections.Add(configSectionTitle, {configEntries=new System.Collections.Generic.Dictionary<string, string>()})
            configSectionTextLines |> Array.iteri(fun i x->
                match i with
                    | 0 ->
                        ()
                    | _ ->
                        if x.IndexOf(":") = -1 then raise (ExpectedResponseFail("You need to put semicolons between the name and value in your config file"))
                        let lineSplit = x.Split([|':'|],2)
                        configFile.configSections.Item(configSectionTitle).configEntries.Add(lineSplit.[0].Trim(),lineSplit.[1].Trim())
                )
            configFile
    let getConfigData (opts:GetLinksProgramConfig) =
        let configFile = 
            if opts.siteConfigFileExists
                then
                    try
                        System.IO.File.ReadAllText(opts.siteConfigFileName.parameterValue)
                    with _->
                        raise (ExpectedResponseFail("Had a problem locating and loading the configuration file at: " + opts.siteConfigFileName.parameterValue))
                        ""
                else
                    raise (UserNeedsHelp("Missing Config File Entry"))
        opts.configBase.interimProgress.addItem "ConfigFileCharactersRead"  (configFile.Length.ToString())
        let configFileTemp = {configSections = System.Collections.Generic.Dictionary<string,siteConfigSection>()}
        let sections = configFile.Split([|"\r\n\r\n"|], System.StringSplitOptions.None)
        let rec recAddSections fileLines configFileTemp = 
            match fileLines with
                | [] -> configFileTemp
                | x::xs ->
                    let newConfigFileTemp = getConfigSectionData opts x configFileTemp
                    recAddSections xs newConfigFileTemp
        let returnConfig = recAddSections (Array.toList sections) configFileTemp
        opts.configBase.interimProgress.addItem "ConfigSectionsReadCount" (returnConfig.configSections.Count.ToString())
        returnConfig

    let doStuff (opts:GetLinksProgramConfig) =
        let config = getConfigData opts
        let outputDataHolder = {
            outputSections = new System.Collections.Generic.Dictionary<string, outputSection>()}
        let outputData = ripLinks opts config outputDataHolder
        if outputData.outputSections.Count>0
            then 
                let max = outputData.outputSections |> Seq.maxBy(fun x->x.Value.outputList.Count)
                let maxCount = max.Value.outputList.Count
                let min = outputData.outputSections |> Seq.minBy(fun x->x.Value.outputList.Count)
                let minCount = min.Value.outputList.Count
                if maxCount = minCount 
                then 
                        let msg = maxCount.ToString() + " links with " + (outputData.outputSections.Count-1).ToString() + " pieces of additional information about each link gathered."
                        opts.configBase.interimProgress.addItem "LinksGathered" msg
                else 
                        raise (System.ApplicationException("Number of links, titles, and other Data DOES NOT MATCH.\r\n" + max.Key + "  has " + maxCount.ToString() + "entries while " + min.Key + " has " + minCount.ToString() + ".\r\nCheck your configuration file or run this program with the verbose option set /V"))
                        //
            else
                raise(System.ApplicationException("\r\n\r\nTHERE WERE NO SECTIONS OUTPUT.\r\nPlease check your configuration file.\r\nTry running this program with the /V option"))
        // Report interim progress
        match opts.configBase.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SiteAccessed"))
                printfn "Processing links from: %s" (opts.configBase.interimProgress.getItem("SiteProcessed"))
                ()
            | Verbosity.BatchNormal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("BytesReceived"))
            | Verbosity.Normal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("BytesReceived"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LinksGathered"))
                ()
            | Verbosity.BatchVerbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("BytesReceived"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LinksGathered"))
                ()
            | Verbosity.Verbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("BytesReceived"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LinksGathered"))
                printfn "%s" (opts.configBase.interimProgress.getItem("ConfigSectionsReadCount"))                
                printfn "%s" (opts.configBase.interimProgress.getItem("FullHtml"))
                ()
            |_ ->
                printfn "%s" (opts.configBase.interimProgress.getItem("BytesReceived"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LinksGathered"))
                ()

        ()
    [<EntryPoint>]
    let main argv = 
        let opts = loadConfigFromCommandLine argv
        try
                
            commandLinePrintWhileEnter opts.configBase (opts.printThis)
            doStuff opts
            commandLinePrintWhileExit opts.configBase
            0
        with
            | :? UserNeedsHelp as hex ->
                printfn "%s: %s" defaultBaseOptions.programName hex.Data0
                printfn "========================"
                printfn "Command Line Options:"
                opts.siteConfigFileName.printHelp
                opts.siteUrl.printHelp
                opts.outputFileName.printHelp
                opts.desiredLinkCount.printHelp
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