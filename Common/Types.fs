module Types
    open HtmlAgilityPack
    open System.Text.RegularExpressions

    type 'a ``[]`` with 
        member x.randomItem = 
            let rnd = new System.Random()
            let idx = rnd.Next(x.Length)
            x.[idx]

    type System.String with
        member x.ContainsAny (possibleMatches:string[]) =
            let ret = possibleMatches |> Array.tryFind(fun y->
                x.Contains(y)
                )
            ret.IsSome
        member x.ContainsAnyRegex(possibleRegexMatches:string[]) =
            let ret = possibleRegexMatches |> Array.tryFind(fun y->
                let rg = new System.Text.RegularExpressions.Regex(y)
                rg.IsMatch(x)
                )
            ret.IsSome
        member x.ContainsRegex(regexMatch:string) =
            let rg = new System.Text.RegularExpressions.Regex(regexMatch)
            rg.IsMatch(x)
        member x.ReplaceWithRegex (regexMatchString:string) (replacementString:string) = 
            System.Text.RegularExpressions.Regex.Replace(x, regexMatchString, replacementString)
        member x.CountOccurences (token:string) = 
            let mts = x.Split([|token|], System.StringSplitOptions.None)
            if mts = null then 0 else mts.Length
        member x.CountOccurencesRegex (regexMatchString:string) =
            let mts = System.Text.RegularExpressions.Regex.Matches(x, regexMatchString)
            if mts = null then 0 else mts.Count
        member this.GetRight (iLen:int) =
            try
                this.Substring(this.Length - iLen, iLen)
            with |_ -> ""
        member this.GetLeft (iLen:int) =
            try
                this.Substring(0, iLen)
            with |_ -> ""
        member this.TrimLeft (iCount:int) =
            this.Substring(iCount, this.Length - iCount)
        member this.TrimRight (iCount:int) =
            this.Substring(0, this.Length - iCount)    
        member this.TrimBoth (iLeft:int) (iRight:int) =
            if iLeft + iRight > this.Length
                then
                    ""
                else
                    (this.TrimLeft iLeft) |> (fun x-> x.TrimRight iRight)
        member this.TrimTo (desiredLength:int) =
            if this.Length <= desiredLength
                then
                    this
                else
                    this.GetLeft desiredLength

    type System.Collections.Generic.Dictionary<'A, 'B> with
        member x.stringValueOrEmptyForKey n = 
            if x.ContainsKey n then x.Item(n).ToString() else ""
        member x.TryFind n = 
            let x,(y:'B) = x.TryGetValue n
            if x then Some y else None
    type HtmlDocument with
        member self.InnerTextOfFirstNodeOrEmpty (path:string) =
            let nds = self.DocumentNode.SelectNodes(path)
            if nds = null || nds.Count = 0
                then ""
                else nds.[0].InnerText
    type HtmlAgilityPack.HtmlAttributeCollection with
        member self.HasAttribute (attributeName:string) =
            self.AttributesWithName("class") |>Seq.isEmpty=false
        member self.AttributeValueOrEmptyString (key:string) =
            if self.Contains(key) then self.Item(key).Value else ""
    type HtmlAgilityPack.HtmlNode with
        member self.HasClass (foo:string) = 
            if self.Attributes.HasAttribute "class"
            then
                let classes = self.Attributes.Item("class").Value
                classes.IndexOf(foo) > -1
            else
                false
        member self.HasOneOfTheseClasses (classList:string []) =
            if self.Attributes.HasAttribute "class"
                then
                    self.Attributes.Item("class").Value.ContainsAny classList
                else
                    false
        member self.AddClass (foo:string) =
            if self.HasClass foo
                then ()
                else
                    let oldClass = if self.Attributes.Contains("class") then self.Attributes.Item("class").Value else ""
                    let newClass = oldClass + " " + foo
                    self.Attributes.Remove("class")
                    self.Attributes.Add("class", newClass)
        member self.RemoveClass (foo:string) = 
            if self.HasClass(foo)
            then
                let oldClass = self.Attributes.Item("class").Value
                let newClass = oldClass.Replace(foo, "")
                self.Attributes.Remove("class")
                self.Attributes.Add("class", newClass)                
            else
                ()
        member self.IsHidden = 
            let style = if self.Attributes.Contains("style") then self.Attributes.Item("style").Value else ""
            let thisIsHidden = style.IndexOf("visibility:hidden") > -1 || style.IndexOf("display:none") > -1
            // need to check parents to see if any of them are hidden
            if thisIsHidden then thisIsHidden else
                if self.Name <> "body" then self.ParentNode.IsHidden else thisIsHidden
        member self.HasTag (tagArray:string []) =
            let foundTag = tagArray |> Array.tryFind(fun x->x.ToLower()=self.Name.ToLower())
            foundTag.IsSome
        member self.HasAttributeEuals (attributeName:string) (attributeValue:string) = 
            if self.HasAttributes
            then
                if self.Attributes.AttributesWithName(attributeName) |> Seq.isEmpty = false
                then
                    false
                else
                    let matchingAtts = self.Attributes.AttributesWithName(attributeName) |> Seq.filter(fun x->
                        x.Value = attributeValue
                        )
                    matchingAtts |> Seq.isEmpty = false
            else
                false
        member self.ChildrenThatHaveAClass (className:string) =
            let filter = "//*[(contains(@class,'" + className + "'))]"
            self.SelectNodes(filter)
        member self.ChildrenWithOneOfTheseClasses (classNames:string []) =
            let filterBuilder = new System.Text.StringBuilder(65535)
            classNames |> Array.iteri(fun i x->
                if i > 0 then filterBuilder.Append(" | ") |> ignore else ()
                filterBuilder.Append ("//*[(contains(@class,'" + x + "'))]") |> ignore
                )
            let filter = filterBuilder.ToString()
            self.SelectNodes(filter)
        member self.ChildrenWithAnAttribue (attributeName:string) =
            let filter = "//*[@" + attributeName + "]"
            self.SelectNodes(filter)
        member self.ChildrenWithOneOfTheseAttributes (attributeNames:string []) =
            let filterBuilder = new System.Text.StringBuilder(65535)
            attributeNames |> Array.iteri(fun i x->
                if i > 0 then filterBuilder.Append(" | ") |> ignore else ()
                filterBuilder.Append ("//*[@" + x + "]") |> ignore
                )
            let filter = filterBuilder.ToString()
            self.SelectNodes(filter)
        member self.ChildrenWithOneOfTheseTags (tagList:string []) =
            let filterBuilder = new System.Text.StringBuilder(65535)
            tagList |> Array.iteri(fun i x->
                if i > 0 then filterBuilder.Append(" | ") |> ignore else ()
                filterBuilder.Append ("//" + x ) |> ignore
                )
            let filter = filterBuilder.ToString()
            self.SelectNodes(filter)
            
    type HtmlAgilityPack.HtmlNodeCollection with
        member self.TextFromFirstNodeOrEmpty = 
            if self.Count > 0 then self.[0].InnerText else ""
        member self.ToArray =
            if self = null then Array.empty
            else if self.Count > 0 then self|>Seq.toArray else Array.empty
    type System.Text.RegularExpressions.MatchCollection with
        member this.toSeq =
            seq {for i = 0 to this.Count - 1 do yield this.[i]}
        member this.toArray =
            [|for i = 0 to this.Count - 1 do yield this.[i] |]
    type System.Text.RegularExpressions.Match with
        member this.lastGroup =
            this.Groups.[this.Groups.Count-1]
        member this.lastIndex =
            this.lastGroup.Index + this.lastGroup.Length
    //
    // Common Exception Types
    //
    exception UserNeedsHelp of string
    exception ExpectedResponseFail of string
    type Verbosity =
        | Silent            = 1
        | BatchMinimum      = 2
        | Minimum           = 3
        | BatchNormal       = 4
        | Normal            = 5
        | BatchVerbose      = 6
        | Verbose           = 7
    //
    // Program Command Line Config Settings
    //
    let getMatchingParameters (args:string []) (symbol:string) = 
        args |> Array.filter(fun x->
                    let argParms = x.Split([|':'|],2)
                    let parmName = (argParms.[0]).Substring(1).ToUpper()
                    if argParms.Length > 0 then parmName=symbol.ToUpper() else false
                    )
    let getValuePartOfMostRelevantCommandLineMatch (args:string []) (symbol:string) =
        let matchingParms = getMatchingParameters args symbol
        if matchingParms.Length > 0
            then
                // if there are multiple entries, last one overrides the rest
                let commandLineParm = matchingParms.[matchingParms.Length-1]
                let parmSections=commandLineParm.Split([|':'|], 2)
                if parmSections.Length<2 then Some "" else Some parmSections.[1]
            else
                None
    type ConfigEntry<'A> =
        {
            commandLineParameterSymbol:string
            commandLineParameterName:string
            parameterHelpText:string[]
            parameterValue:'A
        } with
            member this.printVal =
                printfn "%s: %s" this.commandLineParameterName (this.parameterValue.ToString())
            member this.printHelp =
                printfn "%s" this.commandLineParameterName
                this.parameterHelpText |> Seq.iter(System.Console.WriteLine)
            member this.swapInNewValue x =
                {this with parameterValue=x}
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<Verbosity>), (args:string[])):ConfigEntry<Verbosity>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        let parsedNumValue = System.Int32.Parse("0" + parmValue.Value)
                        let parsedVerbosityValue = enum<Verbosity>(parsedNumValue)
                        defaultConfig.swapInNewValue parsedVerbosityValue
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<string>), (args:string[])):ConfigEntry<string>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue parmValue.Value
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<bool>), (args:string[])):ConfigEntry<bool> =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        if parmValue.Value.ToUpper() = "FALSE" || parmValue.Value = "0" || parmValue.Value.ToUpper() = "F" || parmValue.Value.ToUpper() = "NO"
                            then
                                defaultConfig.swapInNewValue false
                            else
                                defaultConfig.swapInNewValue true
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<int>), (args:string[])):ConfigEntry<int>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        let parmInt = System.Int32.Parse("0" + parmValue.Value)
                        defaultConfig.swapInNewValue parmInt
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<System.Uri>), (args:string[])):ConfigEntry<System.Uri>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue (new System.Uri(parmValue.Value))
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<System.DateTime>), (args:string[])):ConfigEntry<System.DateTime>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue (System.DateTime.Parse(parmValue.Value))
                    else
                        defaultConfig
    type InterimProgress =
        {
            items:System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>
        } with
        member this.addItem key (vl:string) =
            let lookup = 
                if this.items.ContainsKey key then this.items.Item(key)
                    else
                        let newItem = new System.Text.StringBuilder(65535)
                        this.items.Add(key,newItem)
                        newItem
            lookup.Append("\r\n" + vl) |> ignore
        member this.getItem key  =
            if this.items.ContainsKey key
                then
                    this.items.Item(key).ToString()
                else
                    ""
    type ConfigBase =
        {
            programName:string
            programTagLine:string
            programHelpText:string[]
            verbose:ConfigEntry<Verbosity>
            interimProgress:InterimProgress
        }
        member this.printProgramDescription =
            this.programHelpText |> Seq.iter(System.Console.WriteLine)
        member this.printThis =
            printfn "%s" this.programName
            this.programHelpText |> Seq.iter(System.Console.WriteLine)

    type CheckXPathProgramConfig =
        {
            configBase:ConfigBase
            siteUrl:ConfigEntry<string>
        }
        member this.printThis() =
            printfn "CheckXPathConfig Parameters Provided"
            this.configBase.verbose.printVal
            this.siteUrl.printVal
    type CreateArchiveProgramConfig =
        {
            configBase:ConfigBase
            beginDate:ConfigEntry<System.DateTime>
            endDate:ConfigEntry<System.DateTime>
            outputFileName:ConfigEntry<string>
            outputFileExists:bool
            inputHTMLHeaderFileName:ConfigEntry<string>
            inputHTMLHeaderFileExists:bool
            inputHTMLFooterFileName:ConfigEntry<string>
            inputHTMLFooterFileExists:bool
        }
            member this.printProgramHelp =
                this.configBase.printProgramDescription
                this.beginDate.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.endDate.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.outputFileName.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.inputHTMLHeaderFileName.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.inputHTMLFooterFileName.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.configBase.verbose.parameterHelpText |> Seq.iter(System.Console.WriteLine)
            member this.printThis() =
                printfn "CreateArchive Parameters Provided"
                this.configBase.verbose.printVal
                this.beginDate.printVal
                this.endDate.printVal
                this.outputFileName.printVal
                printfn "outputFileExists: %b" this.outputFileExists
                this.inputHTMLHeaderFileName.printVal
                printfn "inputHTMLHeaderFileExists: %b" this.inputHTMLHeaderFileExists
                this.inputHTMLFooterFileName.printVal
                printfn "inputHTMLFooterFileExists: %b" this.inputHTMLFooterFileExists

    type CreateSelectedArticleListProgramConfig =
        {
            configBase:ConfigBase
            configFileName: ConfigEntry<string>
            outputFileName: ConfigEntry<string>
            useLocalLinks: ConfigEntry<bool>
            localLinksPrefix: ConfigEntry<string>
            rippedSubdirectory: ConfigEntry<string>
            configFileExists: bool
            outputFileExists:bool
        }
        member this.printThis() =
            printfn "CreateSelectedArticleList Parameters Provided"
            this.configBase.verbose.printVal
            this.configFileName.printVal
            this.outputFileName.printVal
            this.useLocalLinks.printVal
            this.rippedSubdirectory.printVal
            printfn "configFileExists: %b" this.configFileExists
            printfn "outputFileExists: %b" this.outputFileExists
    type FormatSelectedArticleListProgramConfig =
        {
            configBase:ConfigBase
            inputFileName: ConfigEntry<string>
            outputFileName: ConfigEntry<string>            
            formatType: ConfigEntry<string>
            headerFileName: ConfigEntry<string>
            footerFileName: ConfigEntry<string>
            inputFileExists:bool
            outputFileExists:bool
            headerFileExists:bool
            footerFileExists:bool
        }
        member this.printThis() =
            printfn "FormatSelectedArticleList Parameters Provided"
            this.configBase.verbose.printVal
            this.inputFileName.printVal
            this.outputFileName.printVal
            this.formatType.printVal
            this.headerFileName.printVal
            this.footerFileName.printVal
            printfn "inputFileExists: %b" this.inputFileExists
            printfn "outputFileExists: %b" this.outputFileExists
            printfn "headerFileExists: %b" this.headerFileExists
            printfn "footerFileExists: %b" this.footerFileExists

    type GetLinksProgramConfig =
        {
            configBase:ConfigBase
            siteUrl: ConfigEntry<string>
            siteConfigFileName: ConfigEntry<string>
            outputFileName: ConfigEntry<string>
            desiredLinkCount: ConfigEntry<int>
            siteConfigFileExists:bool
        }
        member this.printThis() =
            printfn "GetLinks Parameters Provided"
            this.configBase.verbose.printVal
            this.siteUrl.printVal
            this.siteConfigFileName.printVal
            this.outputFileName.printVal
            this.desiredLinkCount.printVal
            printfn "siteConfigFileExists: %b" this.siteConfigFileExists

    type GetMetricsProgramConfig =
        {
            configBase:ConfigBase
            inputFileName: ConfigEntry<string>
            outputFileName: ConfigEntry<string>
            inputFileExists: bool
        }
        member this.printThis() =
            printfn "GetMetrics Parameters Provided"
            this.configBase.verbose.printVal
            this.inputFileName.printVal
            this.outputFileName.printVal

    type MarkForTrainingProgramConfig =
        {
            configBase:ConfigBase
        }
        member this.printThis() =
            printfn "MarkForTraining Parameters Provided"
            this.configBase.verbose.printVal

    type ReplaceArticleProgramConfig =
        {
            configBase:ConfigBase
        }
        member this.printThis() =
            printfn "ReplaceArticle Parameters Provided"
            this.configBase.verbose.printVal

    type RipBlanksProgramConfig =
        {
            configBase:ConfigBase
            inputSelectedArticleListFileName:ConfigEntry<string>
            outputDirectory:ConfigEntry<string>
            headerTemplateFileName:ConfigEntry<string>
            footerTemplateFileName:ConfigEntry<string>
            inputSelectedArticleListFileExists:bool
            headerTemplateFileExists:bool
            footerTemplateFileExists:bool
        }
            member this.printProgramHelp =
                this.configBase.printProgramDescription
                this.inputSelectedArticleListFileName.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.outputDirectory.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.headerTemplateFileName.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.footerTemplateFileName.parameterHelpText |> Seq.iter(System.Console.WriteLine)
                this.configBase.verbose.parameterHelpText |> Seq.iter(System.Console.WriteLine)
            member this.printThis() =
                printfn "RipBlanksConfig Parameters Provided"
                this.configBase.verbose.printVal
                this.inputSelectedArticleListFileName.printVal
                this.outputDirectory.printVal
                this.headerTemplateFileName.printVal
                this.footerTemplateFileName.printVal
                printfn "inputSelectedArticleListFileExists: %b" this.inputSelectedArticleListFileExists
                printfn "headerTemplateFileExists: %b" this.headerTemplateFileExists
                printfn "footerTemplateFileExists: %b" this.footerTemplateFileExists

    type RipFullPagesProgramConfig =
        {
            configBase:ConfigBase
            sourceDirectory:ConfigEntry<string>
            destinationDirectory:ConfigEntry<string>
        }
        member this.printThis() =
            printfn "RipFullPages Parameters Provided"
            this.configBase.verbose.printVal
            this.sourceDirectory.printVal
            this.destinationDirectory.printVal

    type RipMarkedUpPagesProgramConfig =
        {
            configBase:ConfigBase
            sourceDirectory:ConfigEntry<string>
            destinationDirectory:ConfigEntry<string>
        }
        member this.printThis() =
            printfn "RipMarkedUpPages Parameters Provided"
            this.configBase.verbose.printVal
            this.sourceDirectory.printVal
            this.destinationDirectory.printVal

    type RipPostProcessedPagesProgramConfig =
        {
            configBase:ConfigBase
            sourceDirectory:ConfigEntry<string>
            destinationDirectory:ConfigEntry<string>
        }
        member this.printThis() =
            printfn "RipPostProcessedPages Parameters Provided"
            this.configBase.verbose.printVal
            this.sourceDirectory.printVal
            this.destinationDirectory.printVal

    type RipProcessedPagesProgramConfig =
        {
            configBase:ConfigBase
            sourceDirectory:ConfigEntry<string>
            destinationDirectory:ConfigEntry<string>
        }
        member this.printThis() =
            printfn "RipProcessedPages Parameters Provided"
            this.configBase.verbose.printVal
            this.sourceDirectory.printVal
            this.destinationDirectory.printVal

    type RipTextConfigAcessMode = URL | SelectedArticleListFile | File | NothingToProcess
    type RipTextProgramConfig =
        {
            configBase:ConfigBase
            inputSelectedArticleListFileName:ConfigEntry<string>
            urlToProcess:ConfigEntry<string>
            mode:RipTextConfigAcessMode
            outputFilePrefix:ConfigEntry<string>
            desiredLinkCountToProcess:ConfigEntry<int>
            rippedSubdirectory:ConfigEntry<string>
            headerHtmlFileName:ConfigEntry<string>
            footerHtmlFileName:ConfigEntry<string>
            inputSelectedArticleListFileExists:bool
            headerHtmlFileExists:bool
            footerHtmlFileExists:bool
        }
        member this.printThis() =
            printfn "RipText Parameters Provided"
            this.configBase.verbose.printVal
            this.inputSelectedArticleListFileName.printVal
            this.urlToProcess.printVal
            printfn "%A" this.mode
            this.outputFilePrefix.printVal
            this.desiredLinkCountToProcess.printVal
            this.rippedSubdirectory.printVal
            this.headerHtmlFileName.printVal
            this.footerHtmlFileName.printVal
            printfn "headerHtmlFileExists: %b" this.headerHtmlFileExists
            printfn "footerHtmlFileExists: %b" this.footerHtmlFileExists
            printfn "inputSelectedArticleListFileExists: %b" this.inputSelectedArticleListFileExists

    type UpdateArticleLibraryProgramConfig =
        {
            configBase:ConfigBase
            inputFileName:ConfigEntry<string>
            //configFileName:ConfigEntry<string>
            libraryFileName:ConfigEntry<string>
            inputFileExists:bool
            //configFileExists:bool
            libraryFileExists:bool
        }
        member this.printThis() =
            printfn "UpdateArticleLibrary Parameters Provided"
            this.configBase.verbose.printVal
            this.inputFileName.printVal
            //this.configFileName.printVal
            this.libraryFileName.printVal
            printfn "inputFileExists: %b" this.inputFileExists
            //printfn "configFileExists: %b" this.configFileExists
            printfn "libraryFileExists: %b" this.libraryFileExists


//    type UpdateArticleLibraryProgramConfig =
//        {
//            inputFileName:string
//            configFileName:string
//            libraryFileName:string
//            verbose:bool
//            inputFileExists:bool
//            configFileExists:bool
//            libraryFileExists:bool
//        }


    //
    //
    //
    type MetricsFileData =
        {
            fileName:string
            lastModifiedTime:string
            sizeInLines:string
        }
    let defaultMetricsFileData = {fileName=""; lastModifiedTime=""; sizeInLines=""}
    type MetricsTimeFrame = 
        | Minutes
        | Hours
        | Days
        | Months
    type MetricsDirectoryData =
        {
            directoryName: string
            lastWriteTime:string
            fileCount: string
            timeFrameFileCount: (string*string)[]
            mostRecentThreeFiles: System.IO.FileSystemInfo[]
        }
    let defaultMetricsDirectoryData = {directoryName=""; lastWriteTime=""; fileCount="0"; timeFrameFileCount = Array.empty; mostRecentThreeFiles=Array.empty}
    type GetMetricsData =
        {
            runTime:string
            getLinksInfo:MetricsFileData[]
            articleLibraryInfo:MetricsFileData[]
            selectedArticleListInfo:MetricsFileData[]
            indexFile:MetricsFileData
        }
    //
    // Config FileData Types
    //
    [<NoComparison>]
    type siteConfigSection = 
        {
            configEntries:System.Collections.Generic.Dictionary<string, string>
        }
    [<NoComparison>]
    type siteConfig = 
        {
            configSections:System.Collections.Generic.Dictionary<string,siteConfigSection>
        }
    [<NoComparison>]
    type outputSection = 
        {
            outputList:System.Collections.Generic.List<string>
        }
    [<NoComparison>]
    type outputStrings = 
        {
            outputSections:System.Collections.Generic.Dictionary<string,outputSection>
        }

    type linkTimeTuple = {
        link:string
        time:string
        }
    type titleTimeTuple = {
        title:string
        time:string
        }
    type fileData =
        {
            fileLastModified:System.DateTime
            initialList: list<string*string*string>
            byLink: System.Collections.Generic.Dictionary<string, titleTimeTuple>
            byTitle: System.Collections.Generic.Dictionary<string, linkTimeTuple>
        }

    let urlFileNameCharacterMap = 
        [
            ("."  , "___-")
            ;("/" , "__-_")
            ;("\\", "__--")
            ;("&" , "_-__")
            ;("?",  "_-_-")
            ;("%",  "_--_")
            ;("+",  "_---")
            ;(":",  "-___")
            ;("$",  "-__-")
            ;("\'", "-_-_")
        ]
    [<NoComparison>]
    type libraryEntry =
        {
            SectionName: string
            NumberOfLinksDesired: int
            LibraryFile: string
        }
    [<NoComparison>]
    type librarySection = 
        {
            libraryEntries:System.Collections.Generic.List<libraryEntry>
        }


    [<NoComparison>]
    type CreateSelectedArticleListConfigLibrarySection = 
        {
            libraryEntries:System.Collections.Generic.List<libraryEntry>
        }
    [<NoComparison>]
    type config = 
        {
            metaSection:System.Collections.Generic.Dictionary<string,string>
            librarySections:System.Collections.Generic.SortedList<string, CreateSelectedArticleListConfigLibrarySection>
        }
    type ArticleListSubSections =
        {
            SubSectionTitle:string
            ArticleList:(string*string)[]
        }
    type ArticleListSection =
        {
            SectionTitle:string
            SubSections:ArticleListSubSections[]
        }
    type ArticleList =
        {
            Sections:ArticleListSection[]   
        }

    type TextInfo =
        {
            TextLength: int
            TextWordsNr:int
            LinksLength:int
            LinksWordsNr:int
            LinksNr:int
            TheoreticalLinesOf65Characters:float
            TheoreticalParagraphsOf3Lines:float
            TheoreticalParagraphsOf5Lines:float
            TheoreticalParagraphsOf50Words:float
            TheoreticalParagraphsOf80Words:float
            LinksToTextRatio:float
            LinksWordsNrToWordsNrRatio:float
        }
    let defaultTextInfo = 
        {
            TextLength = 0
            TextWordsNr = 0
            LinksLength = 0
            LinksWordsNr = 0
            LinksNr = 0
            TheoreticalLinesOf65Characters = 0.0
            TheoreticalParagraphsOf3Lines = 0.0
            TheoreticalParagraphsOf5Lines = 0.0
            TheoreticalParagraphsOf50Words = 0.0
            TheoreticalParagraphsOf80Words = 0.0
            LinksToTextRatio = 0.0
            LinksWordsNrToWordsNrRatio = 0.0
        }
    type TextInfoSmall = 
        {
            Text:string
            Links:string
            LinksNr:int
            CandidatesNr:int
        }
    type ElementsPointsComputation =
        {
            TextComputation:TextInfo
            Points:float
            PointsBeforeLinkRatio:float
        }
    let defaultElementsPointsComputation = 
        {
            TextComputation = defaultTextInfo
            Points = 0.0
            PointsBeforeLinkRatio = 0.0
        }
    type CandidatesDictionaryItem =
        {
            Element:HtmlNode
            Paragraphs:int
            Computation:ElementsPointsComputation option
            Points:float
            PointsBeforeDomOrder3:float
            PointsBeforeSubsAddition2:float
            PointsBeforeLinkRatio1:float
        }
    let defaultCandidatesDictionaryItem(nd) =
        {
            Element = nd
            Paragraphs = 0
            Computation = Some defaultElementsPointsComputation
            Points = 0.0
            PointsBeforeDomOrder3 = 0.0
            PointsBeforeSubsAddition2 = 0.0
            PointsBeforeLinkRatio1 = 0.0
        }
    type CandidatesDictionary = 
        {
            Dict:System.Collections.Generic.Dictionary<string, CandidatesDictionaryItem>
        }
//    let verbositySetHigherThan (confg:'a when 'a:(member configBase:ConfigBase)) (verbosityLevel:Verbosity) =
//        confg.configBase.verbose.parameterValue>verbosityLevel
//    type CreateSelectedArticleListProgramConfig =
//        {
//            configFileName:string
//            outputFileName:string
//            verbose:bool
//            configFileExists:bool
//            outputFileExists:bool
//            useLocalLinks:bool
//            localLinksPrefix:string
//            rippedSubdirectory:string
//        }
//    type FormatSelectedArticleListConfig =
//        {
//            inputFileName:string
//            outputFileName:string
//            formatType:string
//            verbose:bool
//            headerFileName:string
//            footerFileName:string
//            inputFileExists:bool
//            outputFileExists:bool
//            headerFileExists:bool
//            footerFileExists:bool
//        }



//    let getMatchingParameters (args:string []) (symbol:string) = 
//        args |> Array.filter(fun x->
//                    let argParms = x.Split([|':'|],2)
//                    let parmName = (argParms.[0]).Substring(1).ToUpper()
//                    if argParms.Length > 0 then parmName=symbol.ToUpper() else false
//                    )
//    let getValuePartOfMostRelevantCommandLineMatch (args:string []) (symbol:string) =
//        let matchingParms = getMatchingParameters args symbol
//        if matchingParms.Length > 0
//            then
//                // if there are multiple entries, last one overrides the rest
//                let commandLineParm = matchingParms.[matchingParms.Length-1]
//                let parmSections=commandLineParm.Split([|':'|], 2)
//                Some parmSections.[1]
//            else
//                None
//
//    type ConfigEntry<'A> =
//        {
//            commandLineParameterSymbol:string
//            commandLineParameterName:string
//            parameterHelpText:string[]
//            parameterValue:'A
//        } with
//            member this.printVal =
//                printfn "%s: %s" this.commandLineParameterName (this.parameterValue.ToString())
//            member this.swapInNewValue x =
//                {this with parameterValue=x}
//            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<Verbosity>), (args:string[])):ConfigEntry<Verbosity>  =
//                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
//                if parmValue.IsSome
//                    then
//                        let parsedNumValue = System.Int32.Parse("0" + parmValue.Value)
//                        let parsedVerbosityValue = enum<Verbosity>(parsedNumValue)
//                        defaultConfig.swapInNewValue parsedVerbosityValue
//                    else
//                        defaultConfig
//            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<string>), (args:string[])):ConfigEntry<string>  =
//                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
//                if parmValue.IsSome
//                    then
//                        defaultConfig.swapInNewValue parmValue.Value
//                    else
//                        defaultConfig
//            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<bool>), (args:string[])):ConfigEntry<bool> =
//                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
//                if parmValue.IsSome
//                    then
//                        if parmValue.Value.ToUpper() = "FALSE" || parmValue.Value = "0" || parmValue.Value.ToUpper() = "F" || parmValue.Value.ToUpper() = "NO"
//                            then
//                                defaultConfig.swapInNewValue false
//                            else
//                                defaultConfig.swapInNewValue true
//                    else
//                        defaultConfig
//            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<int>), (args:string[])):ConfigEntry<int>  =
//                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
//                if parmValue.IsSome
//                    then
//                        let parmInt = System.Int32.Parse("0" + parmValue.Value)
//                        defaultConfig.swapInNewValue parmInt
//                    else
//                        defaultConfig
//
//    type ConfigBase =
//        {
//            programName:string
//            programTagLine:string
//            programHelpText:string[]
//            verbose:ConfigEntry<Verbosity>
//        }
//        member this.printProgramDescription =
//            this.programHelpText |> Seq.iter(System.Console.WriteLine)
//        member this.printThis =
//            printfn "%s" this.programName
//            this.programHelpText |> Seq.iter(System.Console.WriteLine)



//    type GetLinksProgramConfig =
//        {
//            siteUrl:string
//            siteConfigFile:string
//            outputFile:string
//            verbose:Verbosity
//            desiredLinkCount:int
//        }

//    type RipTextConfig =
//        {
//            inputSelectedArticleListFileName:string
//            urlToProcess:string
//            mode:RipTextConfigAcessMode
//            outputFileprefix:string
//            verbose:Verbosity
//            desiredLinkCountToProcess:int
//            inputSelectedArticleListFileExists:bool
//            rippedSubdirectory:string
//            headerHtmlFile:string
//            footerHtmlFile:string
//            headerHtmlFileExists:bool
//            footerHtmlFileExists:bool
//        }
