    open Types
    open Utils
    open Persist
    open HtmlAgilityPack

    let defaultBaseOptions = createNewBaseOptions "Rip Blanks" "Create empty html files based on a list." [|"Create empty html files based on a list."|] defaultVerbosity
    let defaultOutputDirectory = createNewConfigEntry "D" "Destination Directory" [|"/D:<directorypath> -> Directory to place all the blank files in."|] "ripped/blanks"
    let defaultHeaderTemplateFileName = createNewConfigEntry "H" "Header Html Template" [|"/H:<filename> -> Template to stick at the top of the file."|] "ripBlanksHeader.html"
    let defaultFooterTemplateFileName = createNewConfigEntry "F" "Footer Html Template" [|"/F:<filename> -> Template to stick at the bottom of the file."|] "ripBlankdsFooter.html"
    let defaultInputSelectedArticleListFileName = createNewConfigEntry "I" "Input Selected Article List File Name" [|"/I:<filename>      -> Input file in the selectedarticlelist format."|] ""
    let defaultHeaderTemplateFileExists = false
    let defaultFooterTemplateFileExists = false
    let defaultInputSelectedArticleListFileExists = false

    let loadConfigFromCommandLine (args:string []) =
        let newOutputDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputDirectory, args)
        let newInputSelectedListFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultInputSelectedArticleListFileName, args)
        let newHeaderTemplateFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultHeaderTemplateFileName, args)
        let newFooterTemplateFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultFooterTemplateFileName, args)
        let newInputSelectedArticleListFileExists = System.IO.File.Exists(newInputSelectedListFileName.parameterValue)
        let newHeaderTemplateFileExists = System.IO.File.Exists(newHeaderTemplateFileName.parameterValue)
        let newFooterTemplateFileExists = System.IO.File.Exists(newFooterTemplateFileName.parameterValue)    
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        try
            if System.IO.Directory.Exists(newOutputDirectory.parameterValue) = false
                then
                    System.IO.Directory.CreateDirectory(newOutputDirectory.parameterValue) |> ignore
                else
                    ()
        with | :? System.Exception as ex ->
            raise (new System.ApplicationException("Cannot find or create output directory " + newOutputDirectory.parameterValue, ex))
            
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            inputSelectedArticleListFileName = newInputSelectedListFileName
            outputDirectory = newOutputDirectory
            headerTemplateFileName = newHeaderTemplateFileName
            footerTemplateFileName=newFooterTemplateFileName
            inputSelectedArticleListFileExists= newInputSelectedArticleListFileExists
            headerTemplateFileExists = newHeaderTemplateFileExists
            footerTemplateFileExists = newFooterTemplateFileExists
        }


    let processSubSection (subSection:string) = 
        let subSectionLines = subSection.Split([|"\r\n"|], System.StringSplitOptions.None)
        let newSubSectionTitle = (subSectionLines.[0]).TrimBoth 1 1
        let subSectionLinesWithoutTitle = (subSectionLines |> Array.mapi(fun i x->if i >0 then x else ""))  |> Array.filter(fun x->x.Length >0)
        let pairs =
            subSectionLinesWithoutTitle |> Seq.pairwise 
              |> Seq.mapi (fun i x -> i%2=0, x) 
              |> Seq.filter fst 
              |> Seq.map snd |> Seq.toArray
        {
            SubSectionTitle = newSubSectionTitle
            ArticleList = pairs
        }      
    let processMainSections (opts:RipBlanksProgramConfig) (sections:string[]) = 
        let newSections = sections |> Array.map(fun x->
            let newSectionTitle = (x.Split([|"\r\n\r\n"|], System.StringSplitOptions.None).[0]).TrimBoth 1 1
            let subSections = x.Split([|"\r\n\r\n"|], System.StringSplitOptions.None) |> Array.mapi(fun i x->if i >0 then x else "")  |> Array.filter(fun x->x.Length >0)
            let subsects = subSections |> Array.map(fun x->processSubSection x)
            {
                SectionTitle = newSectionTitle
                SubSections = subsects
            }
            )
        {
            Sections = newSections
        }
    let loadArticleList (opts:RipBlanksProgramConfig) =
        let fileText = System.IO.File.ReadAllText opts.inputSelectedArticleListFileName.parameterValue
        let initMainSections = fileText.Split([|"\r\n\r\n\r\n"|], System.StringSplitOptions.None) |> Array.map(fun x->
            if x.GetLeft 4 = "\r\n\r\n" then x.TrimLeft 4 else x)
        let mainSections = initMainSections |> Array.filter(fun x->
            x.Trim().Length <> 0
            )
        processMainSections opts mainSections

    let getArticleCount (articleList:ArticleList) =
        Array.fold(
            fun acc (x:ArticleListSection)->acc + (Array.fold(fun (yacc:int) (y:ArticleListSubSections) ->
                yacc + y.ArticleList.Length
                ) 0 x.SubSections)
            ) 0 articleList.Sections
    let iterateThroughArticles (articleList:ArticleList) (f:(string*string)->unit) =
        articleList.Sections |> Array.iteri(fun i (x:ArticleListSection)->
            x.SubSections |> Array.iteri(fun j (y:ArticleListSubSections) ->
                y.ArticleList |> Array.iteri(fun k (z:string*string)->
                    f z
                    )
                )
            )
    let appendToLastTagOf (doc:HtmlDocument) (tagToSearchFor:string) (tag:HtmlNode) =
        let nodeList = doc.DocumentNode.SelectNodes("//" + tagToSearchFor)
        if nodeList <> null
            then
                if nodeList.Count>0
                    then
                        let lastNode=nodeList.[nodeList.Count-1]
                        try
                            lastNode.AppendChild tag |> ignore
                        with 
                             | :? System.Exception as ex ->()
                    else
                        ()
            else
                ()
    let appendToHead (doc:HtmlDocument) (tag:HtmlAgilityPack.HtmlNode) =
        appendToLastTagOf doc "head" tag
    let addMetaTag (doc:HtmlDocument) name content =
        //let htmlEncodedContent = System.Net.WebUtility.HtmlEncode(content)
        let newMeta = doc.CreateElement("meta")
        newMeta.Attributes.Add("name", name)
        newMeta.Attributes.Add("content", content)
        appendToHead doc newMeta

    let doStuff opts =
        let listToProcess = loadArticleList opts
        let articleCount = getArticleCount listToProcess
        let articleListLoadMessage = sprintf "Selected Article List %s loaded successfully with %i sections and %i articles" opts.inputSelectedArticleListFileName.parameterValue listToProcess.Sections.Length articleCount
        opts.configBase.interimProgress.addItem "ARTICLELISTLOAD" articleListLoadMessage

        iterateThroughArticles listToProcess (fun (a,b) ->
            let articleTitle = a
            let articleLink = b
            let localFileName = Utils.makeUrlIntoAFilename2(b,a)
            //let localFullFilePath = opts.outputDirectory.parameterValue + "/" + localFileName
            let headerHtml = 
                if opts.headerTemplateFileExists
                    then
                        System.IO.File.ReadAllText(opts.headerTemplateFileName.parameterValue)
                    else
                        ""
            let footerHtml = 
                if opts.footerTemplateFileExists
                    then
                        System.IO.File.ReadAllText(opts.footerTemplateFileName.parameterValue)
                    else
                        ""
            let docHtml = headerHtml + footerHtml
            let newDoc = new HtmlAgilityPack.HtmlDocument()
            newDoc.LoadHtml(docHtml)

            let titleTag = newDoc.CreateElement("title")
            titleTag.AppendChild(newDoc.CreateTextNode(articleTitle)) |> ignore
            appendToHead newDoc titleTag            

            addMetaTag newDoc "n23:originalLink" articleLink 
            addMetaTag newDoc "n23:localFileName" localFileName
            addMetaTag newDoc "n23:blankArticleCreationTime" (System.DateTime.UtcNow.ToString())
            addMetaTag newDoc "n23:articleState" "Blank"
            //addMetaTag newDoc "" ""

            let docHeading = newDoc.CreateElement("h1")
            docHeading.AppendChild(newDoc.CreateTextNode(articleTitle)) |> ignore
            appendToLastTagOf newDoc "div" docHeading

            let remoteLink = newDoc.CreateElement("a")
            remoteLink.Attributes.Add("href", articleLink)
            remoteLink.Attributes.Add("rel", "noreferrer")
            let linkUri = new System.Uri(articleLink)
            remoteLink.AppendChild(newDoc.CreateTextNode("Original Article on " + linkUri.Host)) |> ignore
            appendToLastTagOf newDoc "div" remoteLink

            opts.configBase.interimProgress.addItem "PROCESSINGITEM" (sprintf "Processing %s link: %s" articleTitle articleLink)
            
            //newDoc.Save(localFileName, System.Text.Encoding.UTF8)
            newDoc.Save("a", System.Text.Encoding.UTF8)
            opts.configBase.interimProgress.addItem "LOCALFILESAVED" (sprintf "Local file saved: %s" localFileName)
            copyToDestinationDirectory "a" (opts.outputDirectory.parameterValue + System.IO.Path.DirectorySeparatorChar.ToString() + localFileName)
            opts.configBase.interimProgress.addItem "LOCALFILEMOVED" (sprintf "Local file moved to directory %s." opts.outputDirectory.parameterValue)
            ()
            )
        // Report interim progress
        match opts.configBase.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                ()
            | Verbosity.BatchNormal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("ARTICLELISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSINGITEM"))
                ()
            | Verbosity.Normal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("ARTICLELISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSINGITEM"))
                ()
            | Verbosity.BatchVerbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("ARTICLELISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSINGITEM"))
                ()
            | Verbosity.Verbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("ARTICLELISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSINGITEM"))
                ()
            |_ ->
                printfn "%s" (opts.configBase.interimProgress.getItem("ARTICLELISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSINGITEM"))
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
                defaultBaseOptions.printThis
                opts.inputSelectedArticleListFileName.printHelp
                opts.outputDirectory.printHelp
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