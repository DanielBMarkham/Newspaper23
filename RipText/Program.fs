module Main
    open Types
    open Utils
    open Persist
    open HtmlAgilityPack
    open System.Net

    let defaultBaseOptions = createNewBaseOptions "RipText" "Visit an article on the web and return back a mostly plain text version." [|"Uses a scoring system to guess at where the main text is."|] defaultVerbosity
    let defaultInputSelectedArticleListFileName = createNewConfigEntry "I" "Selected Article Input File" [|"/I:<filename> -> The selected article link list to process."|] ""
    let defaultHeaderHtmlFileName = createNewConfigEntry "H" "Header File" [|"/H:<filename> -> The header template to use."|] ""
    let defaultFooterHtmlFileName = createNewConfigEntry "F" "Footer File" [|"/F:<filename> -> The footer template to use."|] ""
    let defaultUrlToProcess = createNewConfigEntry "S" "Site URL" [|"/S:<url> -> The URL to load. Used when debugging and loading one link at a time"|] ""
    let defaultOutputFilePrefix = createNewConfigEntry "P" "Output File Prefix" [|"/P:<string> -> For adding after the local directory and before the file name when creating local files."|] ""
    let defaultDesiredLinkCountToProcess = createNewConfigEntry "N" "Number of Links to Process" [|"/N:<count> -> How many links to scrape. If the main page doesn't have that many, load up subsequent pages.";"(Assuming the More Link has been defined in the config file)"|] 30
    let defaultRippedSubdirectory = createNewConfigEntry "D" "Ripped Subdirectory" [|"/D:<partial directory path> -> The root directory to put these html plain text files once created."|] "ripped"
    let defaultMode = SelectedArticleListFile
    let defaultInputSelectedArticleListFileExists=false
    let defaultHeaderHtmlFileExists = false
    let defaultFooterHtmlFileExists =false

    let loadConfigFromCommandLine (args:string []):RipTextProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newInputSelectedArticleListFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultInputSelectedArticleListFileName, args)
        let newHeaderHtmlFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultHeaderHtmlFileName, args)
        let newFooterHtmlFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultFooterHtmlFileName, args)
        let newUrlToProcess = ConfigEntry<_>.populateValueFromCommandLine(defaultUrlToProcess, args)
        let newOutputFilePrefix = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFilePrefix, args)
        let newDesiredLinkCountToProcess = ConfigEntry<_>.populateValueFromCommandLine(defaultDesiredLinkCountToProcess, args)
        let newRippedSubdirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultRippedSubdirectory, args)
        let newInputSelectedArticleListFileExists= System.IO.File.Exists(newInputSelectedArticleListFileName.parameterValue)
        let newHeaderHtmlFileExists = System.IO.File.Exists(newHeaderHtmlFileName.parameterValue)
        let newFooterHtmlFileExists = System.IO.File.Exists(newFooterHtmlFileName.parameterValue)
        let theresAnUrlToTryToLoad = newUrlToProcess.parameterValue.Length>0
        let newMode = 
            match newInputSelectedArticleListFileExists, theresAnUrlToTryToLoad with
                | false, false ->NothingToProcess
                | false, true ->URL
                | true, false->SelectedArticleListFile
                | true, true->SelectedArticleListFile
        {
            configBase=newConfigBase
            inputSelectedArticleListFileName=newInputSelectedArticleListFileName
            headerHtmlFileName=newHeaderHtmlFileName
            footerHtmlFileName=newFooterHtmlFileName
            urlToProcess=newUrlToProcess 
            outputFilePrefix=newOutputFilePrefix
            desiredLinkCountToProcess=newDesiredLinkCountToProcess
            rippedSubdirectory=newRippedSubdirectory
            mode=newMode
            inputSelectedArticleListFileExists=newInputSelectedArticleListFileExists
            headerHtmlFileExists=newHeaderHtmlFileExists
            footerHtmlFileExists=newFooterHtmlFileExists 
        }

    let getUrlList opts = 
        match opts.mode with
            | NothingToProcess -> Array.empty<string>
            | File -> [||]
            | URL ->
                [|opts.urlToProcess.parameterValue|]
            | SelectedArticleListFile->
                if opts.inputSelectedArticleListFileExists
                    then
                        let fileLines = System.IO.File.ReadAllLines(opts.inputSelectedArticleListFileName.parameterValue)
                        opts.configBase.interimProgress.addItem "LinesToProcess" (fileLines.Length.ToString() + " lines in " + opts.inputSelectedArticleListFileName.parameterValue + " to process.")
                        //if opts.verbose >= Verbosity.BatchVerbose then System.Console.WriteLine(fileLines.Length.ToString() + " lines in " + opts.inputSelectedArticleListFileName.parameterValue + " to process.")
                        let filteredByValidLinks = fileLines |> Array.filter(fun x->
                            x.GetLeft 7 = "http://" || x.GetLeft 8 = "https://" || x.GetLeft 6 = "ftp://")
                            /// TODO: Fix this so it actually checks to see if local file exists
                        let filteredByFileExists = 
                            filteredByValidLinks |> Array.filter(fun x->
                            //let localName = makeUrlIntoAFilename2(x,)
                            //let fqln = fullyQualifiedLocalName opts.rippedSubdirectory.parameterValue opts.outputFilePrefix.parameterValue localName ""
                            //let fullyQualifiedLocalName = System.IO.Path.Combine(opts.rippedSubdirectory.parameterValue,opts.outputFilePrefix.parameterValue,localName)
                            //System.IO.File.Exists(fqln) = false
                            false
                            )
                            
                        let filesNeedingProcessing = if opts.desiredLinkCountToProcess.parameterValue > filteredByFileExists.Length then filteredByFileExists else filteredByFileExists |> Seq.take(opts.desiredLinkCountToProcess.parameterValue) |> Seq.toArray
                        opts.configBase.interimProgress.addItem "FilesNeedingProcessing" (filesNeedingProcessing.Length.ToString() + " lines after filtered for links already done and trimmed to desired length ")
                        //if opts.verbose >= Verbosity.BatchVerbose then System.Console.WriteLine(filesNeedingProcessing.Length.ToString() + " lines after filtered for links already done and trimmed to desired length ")
                        filesNeedingProcessing
                    else
                        [||]

    let validateYearMonthDirectoryExists(dt:System.DateTime) (initialPath:string) =
        let dirSeparator = System.IO.Path.DirectorySeparatorChar.ToString()
        let yearPath = initialPath + dirSeparator + dt.Year.ToString("nnnn")
        let monthPath = initialPath + dirSeparator + dt.Year.ToString("nnnn") + dirSeparator + dt.Month.ToString("nn")
        if System.IO.Directory.Exists(yearPath) = false
            then
                System.IO.Directory.CreateDirectory(yearPath) |> ignore
            else
                ()
        if System.IO.Directory.Exists(monthPath) = false
            then
                System.IO.Directory.CreateDirectory(monthPath) |> ignore
            else
                ()

    let getElementValidText (elem:HtmlNode) (skipCandidatesInTextComputation:bool) =
        let skipList = [|"button"; "canvas"; "embed"; "frame"; "input"; "iframe"; "link"; "map"; "marquee"; "nobr"; "noframes"; "noscript"; "object"; "script"; "select"; "style"; "textarea" |]
        let tagIsInSkipList (tag:string) = (skipList |> Array.tryFind(fun y->y.ToLower()=tag.ToLower())).IsSome
        let regexSpaceReplace = new System.Text.RegularExpressions.Regex("\s+", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
        let textWriteTo = new System.Text.StringBuilder(65536)
        let linksWriteTo = new System.Text.StringBuilder(65536)
        let candidateCount = ref 0
        let hiddenCount = ref 0
        let linkCount = ref 0
        let rec recElementValidText (children:HtmlNodeCollection) (currentWriteTo:System.Text.StringBuilder)   =
            children |> Seq.iter(fun x->
                match x.NodeType with
                    | HtmlNodeType.Text ->
                        currentWriteTo.Append (" " + x.InnerText) |>ignore
                    | HtmlNodeType.Element ->
                        if (tagIsInSkipList x.Name)
                        then
                            ()
                        else if x.Name.ToLower()="a"
                        then
                            linkCount:=!linkCount+1
                            let newCurrentWriteTo = linksWriteTo
                            recElementValidText x.ChildNodes newCurrentWriteTo
                        else if skipCandidatesInTextComputation && (x.Data.ContainsKey("readable_candidate_key"))
                        then
                            candidateCount:=!candidateCount+1
                        else if x.HasClass("readable_class_is_hidden")
                        then
                            hiddenCount:=!hiddenCount+1
                        else
                            ()
                        recElementValidText x.ChildNodes currentWriteTo
                    |_->()
                )
        if (elem.Name.Length>0) && ((tagIsInSkipList elem.Name) = false)
        then
            recElementValidText elem.ChildNodes textWriteTo
        else
            ()
        {
            Text = regexSpaceReplace.Replace(textWriteTo.ToString(), " ")
            Links = regexSpaceReplace.Replace(linksWriteTo.ToString(), " ")
            LinksNr = !linkCount
            CandidatesNr = !candidateCount
        } 
    let getContentNrWords (txt:string) =
        let reg1 = new System.Text.RegularExpressions.Regex("\s+", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
        let matches = reg1.Matches(txt).toArray
        if matches.Length >0 then matches.Length else 0

    let computeTextForElement (element:HtmlNode) (skipCandidateInTextComputation:bool):TextInfo=
        let validText =  getElementValidText element skipCandidateInTextComputation
        let text = validText.Text
        let textLength = text.Length
        let textWordsNr = getContentNrWords text
        
        let links = validText.Links
        let linksNr = validText.LinksNr
        let linksLength = links.Length
        let linksWordsNr = getContentNrWords links

        let theoreticalLinesOf65Characters  = ((float)textLength / 65.0)
        let theoreticalParagraphsOf3Lines = (theoreticalLinesOf65Characters / 3.0)
        let theoreticalParagraphsOf5Lines = (theoreticalLinesOf65Characters / 5.0)
        let theoreticalParagraphsOf50Words = ((float)textWordsNr / 50.0)
        let theoreticalParagraphsOf80Words = ((float)textWordsNr / 80.0)

        let linksToTextRatio = if linksLength > textLength then 1.0 else ((float)linksLength / (float)textLength)
        let linksWordsNrToWordsNrRatio = if linksWordsNr > textWordsNr then 1.0 else ((float)linksWordsNr / (float)textWordsNr)
        {
            TextLength = textLength
            TextWordsNr = textWordsNr
            LinksLength = linksLength
            LinksWordsNr = linksWordsNr
            LinksNr = linksNr
            TheoreticalLinesOf65Characters = theoreticalLinesOf65Characters
            TheoreticalParagraphsOf3Lines = theoreticalParagraphsOf3Lines
            TheoreticalParagraphsOf5Lines = theoreticalParagraphsOf5Lines
            TheoreticalParagraphsOf50Words = theoreticalParagraphsOf50Words
            TheoreticalParagraphsOf80Words = theoreticalParagraphsOf80Words
            LinksToTextRatio = linksToTextRatio
            LinksWordsNrToWordsNrRatio = linksWordsNrToWordsNrRatio
        }



    let divsThatArePs (doc:HtmlAgilityPack.HtmlDocument) = 
        let divs=doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//div")
        divs |> Seq.iter(fun x->
            if x.ChildNodes.Count = 0 || x.ChildNodes.[0].NodeType <> HtmlNodeType.Text
                then
                    let textComputation = computeTextForElement x false
                    if textComputation.TheoreticalLinesOf65Characters > 2.0 && textComputation.TheoreticalParagraphsOf50Words > 0.75
                        then
                            x.AddClass("readable_class_is_paragraph")
                        else ()
                else ()
            )
    let hiddenElements (doc:HtmlAgilityPack.HtmlDocument)= 
        let elemTypesToCheck = [|"ul"; "ol"; "li"; "dd"; "table"; "tr"; "td"; "div"; "h1"; "h2"; "h3"; "h4"; "h5"|]
        elemTypesToCheck |> Array.iter(fun x->
            let nds = doc.DocumentNode.SelectSingleNode("//body").SelectNodes(x)
            nds |> Seq.iter(fun y->
               if y.IsHidden then y.Data.Add("readable_attr_mark_delete", "readable_class_is_hidden")
                )
            )
        true
    let paragraphsInElement (element:HtmlNode) (doc:HtmlDocument) =
        let elemTagsToCheck = [|"p"; "br+br"; "div.readable_class_is_paragraph"; "h2"; "h3"; "h4"; "h5"; "pre"|]
        let pEles = Seq.toArray (doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//p"))
        let brEles = Seq.toArray (doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//br"))
        let divEles = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//div") |> Seq.filter(fun x->x.HasClass("readable_class_is_paragraph"))
        let h2Eles = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//h2")
        let h3Eles = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//h3")
        let h4Eles = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//h4")
        let h5Eles = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//h5")
        let preEles = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//pre")
        let eleList = Array.concat ([pEles;brEles])
        eleList
    let markTextContainingElements (doc:HtmlAgilityPack.HtmlDocument) (candidatesDictionary:CandidatesDictionary) = 
        let eleList = paragraphsInElement doc.DocumentNode doc
        let visibleEleList = eleList |> Array.filter(fun x->not x.IsHidden)
        visibleEleList |> Array.iter(fun x->
            let parent = x.ParentNode
            let parKey = parent.Data.stringValueOrEmptyForKey("readable_candidate_key")
            if parKey.Length > 0 && not (candidatesDictionary.Dict.ContainsKey(parKey))
                then
                let newKey = "k" + (candidatesDictionary.Dict.Count + 1).ToString()
                parent.Data.Add("readable_candidate_key", newKey)
                let newCandidateItem = defaultCandidatesDictionaryItem(parent)
                candidatesDictionary.Dict.Add(newKey, newCandidateItem)
                //	increment paragraphs
                let oldParentEntry = candidatesDictionary.Dict.Item(parKey)
                candidatesDictionary.Dict.Remove(parKey) |> ignore
                candidatesDictionary.Dict.Add(parKey, {oldParentEntry with Paragraphs=oldParentEntry.Paragraphs+1})
                else ()
            )
    let getParagraphsInElement (ele:HtmlNode) =
        let filter = """//p | //br/br | //div[(contains(@class,'readable_class_is_paragraph'))] | //h2 | //h3 | //h4 | //h5 | //pre"""
        let nodes = ele.SelectNodes(filter)
        if nodes = null then Array.empty else if nodes.Count = 0 then Array.empty else nodes |> Seq.toArray

    let computePointsForElement (element, (paragraphs:int option)) = 
        let calculatedParagraphsInElement = 
            if paragraphs.IsNone
                then
                    (getParagraphsInElement element).Length;
                else
                    paragraphs.Value
        let textComputation = computeTextForElement element true
        let pointsBeforeLinkRatio = 
            ( 0.0 +
                ( (float)calculatedParagraphsInElement * 5.0 ) +
                ( textComputation.TheoreticalParagraphsOf3Lines * 4.0) +
                ( textComputation.TheoreticalParagraphsOf5Lines * 1.5 ) * 3.0 +
                ( textComputation.TheoreticalParagraphsOf50Words ) +
                ( textComputation.TheoreticalParagraphsOf80Words * 1.5 )
            ) / 14.0
        let points = 
            ( 0.0 +
                ( pointsBeforeLinkRatio * ( 1.0 - textComputation.LinksToTextRatio )) + 
                ( pointsBeforeLinkRatio * (1.0 - textComputation.LinksWordsNrToWordsNrRatio ))
            ) / 14.0
        {
            TextComputation = textComputation
            Points = points
            PointsBeforeLinkRatio = pointsBeforeLinkRatio
        }
    let calculateSetAndRemoveSomeDictionaryPoints (candidatesDictionary:CandidatesDictionary) = 
        candidatesDictionary.Dict |> Seq.iter(fun x->
            let element = x.Value.Element
            let parent = null
            let pointsComputation = computePointsForElement(element, Some x.Value.Paragraphs)
            // initial delete
            if ( pointsComputation.TextComputation.TheoreticalParagraphsOf3Lines < 1.0) || // less than 50 words
                ( pointsComputation.TextComputation.TheoreticalParagraphsOf3Lines < 2.0 && pointsComputation.TextComputation.LinksToTextRatio >= 0.25 ) //	less than 100 words, and big link ratio
                then
                    candidatesDictionary.Dict.Remove(x.Key) |> ignore
                    ()
                else
                    let oldItem = x.Value
                    let newItem = 
                        { oldItem with
                            Computation = Some pointsComputation
                            Points = pointsComputation.Points
                            PointsBeforeLinkRatio1 = pointsComputation.PointsBeforeLinkRatio
                            PointsBeforeSubsAddition2 = pointsComputation.Points
                        }
                    ()
                    candidatesDictionary.Dict.Remove(x.Key) |> ignore
                    candidatesDictionary.Dict.Add(x.Key,newItem)
            )
        let retArray = new System.Collections.Generic.Stack<CandidatesDictionaryItem>()
        candidatesDictionary.Dict |> Seq.iteri(fun i x->
            let newKey = "k" + i.ToString()
            retArray.Push x.Value
            )
        retArray
    let doTheDomorder (candidatesArray:System.Collections.Generic.Stack<CandidatesDictionaryItem>) =
        let retArray = new System.Collections.Generic.Stack<CandidatesDictionaryItem>()
        candidatesArray |> Seq.iteri(fun i x->
            let oldItem = x
            let newItem = {
                x with PointsBeforeDomOrder3 = oldItem.Points; Points = (float)candidatesArray.Count/(float)i+1.0
                }
            retArray.Push newItem
            )
        // subs Addition
        let newRetArray = retArray.ToArray()
        let subsFilter = retArray |> Seq.map(fun x ->
            let b = newRetArray |> Array.exists(fun y->y=x)
            7//option x
            )
        0
    let findEverythingThatsHidden (doc:HtmlDocument) =
        let filter = """//*[not (contains(@style,'display:none'))]//ul[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//ol[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//li[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//dd[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//table[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//tr[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//td[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//div[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//h1[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//h2[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//h3[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//h4[(contains(@style,'display:none'))] | //*[not (contains(@style,'display:none'))]//h5[(contains(@style,'display:none'))]"""
        let nodes = doc.DocumentNode.SelectSingleNode("//body").SelectNodes(filter)
        if  nodes = null then Array.empty else if nodes.Count > 0 then (nodes  |> Seq.toArray) else Array.empty

    let findProperParent (nd:HtmlNode) = 
        //let filter = """ancestor::article | //ancestor::body | //ancestor::div | //ancestor::dd | //ancestor::li | //ancestor::section | //ancestor::td"""
        let filter ="""ancestor::*"""
        let ancestorList = nd.SelectNodes(filter).ToArray
        let properAncestors = ancestorList |> Array.filter(fun x->
            x.HasTag([|"article";"body";"div";"dd";"li";"section";"td"|])
            )
        if properAncestors.Length > 0 then Some properAncestors.[properAncestors.Length-1] else None

    let createDictionary (nodes:HtmlNode[]) = 
        let dict = new System.Collections.Generic.Dictionary<string, CandidatesDictionaryItem>()
        let nodesWithProperParentsAndAreNotHidden = nodes |> Array.filter(fun x->
            let properParent = findProperParent x
            properParent.IsSome && properParent.Value <> x && x.HasClass("readable_class_is_hidden") = false
            )
        nodesWithProperParentsAndAreNotHidden |> Array.iter(fun x->
            let properParent = (findProperParent x).Value
            let itsGotAKeyInTheElementAndTheKeyIsAlsoInTheDictionary = properParent.Data.ContainsKey("readable_candidate_key") && dict.ContainsKey(properParent.Data.Item("readable_candidate_key"))
            match itsGotAKeyInTheElementAndTheKeyIsAlsoInTheDictionary with
                | true->
                    let oldKey = properParent.Data.Item("readable_candidate_key")
                    let oldItem = dict.Item(oldKey)
                    dict.Remove(oldKey) |> ignore
                    let tempItem = 
                        {
                            Element = properParent
                            Paragraphs = oldItem.Paragraphs+1
                            Computation = oldItem.Computation
                            Points = oldItem.Points
                            PointsBeforeDomOrder3 = oldItem.PointsBeforeDomOrder3
                            PointsBeforeSubsAddition2 = oldItem.PointsBeforeSubsAddition2
                            PointsBeforeLinkRatio1 = oldItem.PointsBeforeLinkRatio1
                        }
                    dict.Add(oldKey, tempItem)
                | false->
                    let newKey = "k" + dict.Count.ToString()
                    properParent.Data.Add("readable_candidate_key", newKey)
                    let newDictItem = 
                        {
                            Element = properParent
                            Paragraphs = 1
                            Computation = None
                            Points = 0.0
                            PointsBeforeDomOrder3 = 0.0
                            PointsBeforeSubsAddition2 = 0.0
                            PointsBeforeLinkRatio1 = 0.0
                        }
                    dict.Add(newKey,newDictItem)
            )
        dict
    let getTextContainingElements (nd:HtmlNode) = 
        let nodes = getParagraphsInElement nd
        let nodesWithoutHiddenOnes = nodes |> Array.filter(fun x->
            x.HasClass("readable_class_is_hidden") = false
            )
        createDictionary nodesWithoutHiddenOnes

    let findHereGetTargetElement (doc:HtmlDocument):CandidatesDictionaryItem option =
        // find divs that are really Ps
        let temp = doc.DocumentNode.SelectSingleNode("//body").SelectNodes("//div")
        let pNodes = if (temp<>null) then temp |> Seq.toArray else Array.empty
        let divsWithOneTextChild = pNodes |> Array.filter(fun x->
            x.ChildNodes.Count = 1 && x.ChildNodes.[0].NodeType = HtmlNodeType.Text
            )
        let divsThatLookStatisticallyLikeContent = divsWithOneTextChild |> Array.filter(fun x->
            let textInfo = computeTextForElement x false
            (textInfo.TheoreticalLinesOf65Characters > 2.0) && (textInfo.TheoreticalParagraphsOf50Words > 0.75)
            )
        divsThatLookStatisticallyLikeContent |> Array.iter(fun x->
            x.AddClass("readable_class_is_paragraph")
            )
        let hiddenStuff = findEverythingThatsHidden doc
        hiddenStuff |> Array.iter(fun x->
            x.Attributes.Add("readable_attr_mark_delete", "1")
            x.AddClass("readable_class_is_hidden")
            )

        let textContainingElementsArr = 
            let candidatesDictionaryInitialLength = ref 0
            let candidateArray = doc.DocumentNode.SelectNodes("""//p | //br/br | //div[(contains(@class,'readable_class_is_paragraph'))] | //h2 | //h3 | //h4 | //h5 | //pre""").ToArray
            let returnDict = new System.Collections.Generic.Dictionary<string,CandidatesDictionaryItem>()
            candidateArray |> Array.iteri(fun i x->
                if x.HasClass("readable_class_is_hidden") = true 
                    then
                        ()
                    else
                        let parent = findProperParent x
                        if parent.IsNone
                            then
                                ()
                            else
                                if parent.Value.HasClass("readable_class_is_hidden") = true
                                    then
                                        ()
                                    else
                                        let dictKey:string option = if parent.Value.Data = null then None else if parent.Value.Data.ContainsKey("readable_candidate_key") then Some (parent.Value.Data.Item("readable_candidate_key")) else None
                                        // Does the parent have a readable candidate key? Does the current element have a dictionary item?
                                        let parentHaveReadableKey = if isNull(dictKey) then false else dictKey.IsSome
                                        let doesCurrentElementHaveADictionaryItem =  if isNull(dictKey) then false else returnDict.ContainsKey(dictKey.Value)
                                        match parentHaveReadableKey, doesCurrentElementHaveADictionaryItem with
                                            | false, false ->
                                                // parent not marked, element not in dict
                                                // create new parent key
                                                let newKey = "k" + (!candidatesDictionaryInitialLength).ToString()
                                                candidatesDictionaryInitialLength:=!candidatesDictionaryInitialLength+1
                                                parent.Value.Data.Add("readable_candidate_key", newKey)
                                                // create new dictionary item for element
                                                let newDictItem = 
                                                    {
                                                        Element = parent.Value
                                                        Paragraphs = 1
                                                        Computation = None
                                                        Points = 0.0
                                                        PointsBeforeDomOrder3 = 0.0
                                                        PointsBeforeSubsAddition2 = 0.0
                                                        PointsBeforeLinkRatio1 = 0.0
                                                    }
                                                returnDict.Add(newKey, newDictItem)                                                        
                                            | false, true ->
                                                // there is no parent item, but there's already a child item
                                                // it's an error, but go ahead and create a parent item anyway
                                                let newKey = "k" + (!candidatesDictionaryInitialLength).ToString()
                                                candidatesDictionaryInitialLength:=!candidatesDictionaryInitialLength+1
                                                parent.Value.Data.Add("readable_candidate_key" , newKey)
                                                ()
                                            | true, true ->
                                                // there's a parent item and this has already been added. Increment count
                                                // increment paragraph count
                                                let oldItem = returnDict.Item(dictKey.Value)
                                                let newItem = {oldItem with Paragraphs=oldItem.Paragraphs+1}
                                                returnDict.Remove(dictKey.Value) |> ignore
                                                returnDict.Add(dictKey.Value,newItem)
                                            | true, false ->
                                                // there's a parent item and this element isn't in the dictionary.
                                                // add it
                                                let newDictItem = 
                                                    {
                                                        Element = parent.Value
                                                        Paragraphs = 1
                                                        Computation = None
                                                        Points = 0.0
                                                        PointsBeforeDomOrder3 = 0.0
                                                        PointsBeforeSubsAddition2 = 0.0
                                                        PointsBeforeLinkRatio1 = 0.0
                                                    }
                                                returnDict.Add(dictKey.Value,newDictItem)
                )
            returnDict |> Seq.toArray

        let textElementsDictionaryArr =  textContainingElementsArr
        let filterOutNoiseDictionaryArr = textElementsDictionaryArr |> Array.filter(fun x->
            let value = x.Value
            let pointsComputation = computePointsForElement(value.Element, Some value.Paragraphs)
            let itsLessThan50Words = pointsComputation.TextComputation.TheoreticalParagraphsOf3Lines < 1.0
            let itsLessThan100Words = pointsComputation.TextComputation.TheoreticalParagraphsOf3Lines < 2.0
            let itsLessThan300Words = pointsComputation.TextComputation.TheoreticalParagraphsOf3Lines < 6.0
            let realWordCount = Utils.wordCount(x.Value.Element.InnerText)
            let itsGotABigLinkRatio = pointsComputation.TextComputation.LinksToTextRatio >= 0.25
            let itsGotAHugeLinkRatio = pointsComputation.TextComputation.LinksToTextRatio >= 0.75
            (itsLessThan50Words = false)
            && ((itsLessThan100Words && itsGotABigLinkRatio) =false)
            && ((itsLessThan300Words && itsGotAHugeLinkRatio) = false)
            && ((realWordCount < 150 && itsGotABigLinkRatio) = false)
            && ((Utils.wordCount x.Value.Element.InnerText > 7))
            )
        let newDict = new System.Collections.Generic.Dictionary<string, CandidatesDictionaryItem>()
        filterOutNoiseDictionaryArr |> Array.iter(fun x->
            let pointsComputation = computePointsForElement(x.Value.Element, Some x.Value.Paragraphs)
            let newItem = 
                {
                            Element = x.Value.Element
                            Paragraphs = x.Value.Paragraphs
                            Computation = Some pointsComputation
                            Points = pointsComputation.Points
                            PointsBeforeDomOrder3 = 0.0
                            PointsBeforeSubsAddition2 = pointsComputation.Points
                            PointsBeforeLinkRatio1 = pointsComputation.PointsBeforeLinkRatio
                }
            newDict.Add(x.Key, x.Value)
            )
        let candidatesArray = newDict |> Seq.toArray
        let candidatesLength = candidatesArray.Length
        let candidatesArrayDomOrderComputations = candidatesArray |> Array.mapi(fun i x->
            let newKey = x.Key
            let ptsBeforeDomOrder3 = x.Value.Points
            let newValue = 
                { x.Value with
                    PointsBeforeDomOrder3 = ptsBeforeDomOrder3
                    Points = (float)candidatesLength / ((float)i+1.0)
                }
            let newItem = new System.Collections.Generic.KeyValuePair<string, CandidatesDictionaryItem>(newKey, newValue)
            newItem
            )
        let arrayWithSubsAdded = candidatesArrayDomOrderComputations |> Array.map(fun x->
            let childrenWhoAreAlsoInTheArray = candidatesArrayDomOrderComputations |> Array.filter(fun y->
                x.Value.Element.ChildNodes.Contains(y.Value.Element)
                )
            let newPoints = 
                x.Value.Points +
                if (childrenWhoAreAlsoInTheArray |> Seq.length) > 0
                then
                    let firstChild = childrenWhoAreAlsoInTheArray |> Seq.head
                    let firstChildComputations = computePointsForElement(firstChild.Value.Element,(Some firstChild.Value.Paragraphs))
                    let xComputations = computePointsForElement(x.Value.Element, (Some x.Value.Paragraphs))
                    1.0 * (0.66 * firstChildComputations.Points)
                    * (1.0 - (2.0 * xComputations.TextComputation.LinksToTextRatio))
                    * (1.0 - (2.0 * xComputations.TextComputation.LinksWordsNrToWordsNrRatio))
                else
                    0.0
            let newKey = x.Key
            let newVal = {x.Value with Points=newPoints}
            let newItem = new System.Collections.Generic.KeyValuePair<string, CandidatesDictionaryItem>(newKey, newVal)
            newItem
            )
        let subsAddedArraySortedByPoints = arrayWithSubsAdded |> Array.sortBy(fun x->
            -x.Value.Points
        )
        if subsAddedArraySortedByPoints.Length > 5
            then
                //	common parent element
                //	if some of the first N elements have the same parent, maybe that parent is the correct candidate
                let firstElement = subsAddedArraySortedByPoints.[0].Value.Element
                let commonParent = findProperParent firstElement
                let commonParentNr= ref 0
                let totalPoints = ref subsAddedArraySortedByPoints.[0].Value.Points
                subsAddedArraySortedByPoints |> Array.iteri(fun i x->
                    let parent = findProperParent x.Value.Element
                    totalPoints:=!totalPoints + 
                        if i =0 
                        then 0.0 // skip first one
                        else
                            if x.Value.Element = if commonParent.IsSome then commonParent.Value else x.Value.Element
                            then
                                commonParentNr:=!commonParentNr+1
                                0.0 
                            else if parent = commonParent then 
                                commonParentNr:=!commonParentNr+1
                                x.Value.Points
                            else 0.0
                    )
                let enoughElementsWithCommonParent = ((float)!commonParentNr > System.Math.Floor((float)subsAddedArraySortedByPoints.Length/2.0)) || (!commonParentNr > 4)
                match enoughElementsWithCommonParent with
                    | true->
                        let commonParentPoints = 
                            1.0 *
                            (computePointsForElement(commonParent.Value,None)).Points
                        ()
                    | false->
                        ()
                if subsAddedArraySortedByPoints.Length>0 then Some subsAddedArraySortedByPoints.[0].Value else None
            else
                if subsAddedArraySortedByPoints.Length>0 then Some subsAddedArraySortedByPoints.[0].Value else None
    let getHtmlToProcess (elements:HtmlNode []) =
        let returnHtml = new System.Text.StringBuilder(65536)
        elements |> Array.iteri(fun i x->
            let readableDeleteClass = (x.ChildrenThatHaveAClass "readable__delete").ToArray
            readableDeleteClass |> Array.iter(fun x->x.SetAttributeValue("readable_attr_mark_delete", "1")|>ignore)
            let readableKeepClass = (x.ChildrenThatHaveAClass "readable_keep").ToArray
            readableKeepClass |> Array.iter(fun x->x.SetAttributeValue("readable_attr_mark_keep", "1")|>ignore)

            let kidsThatAreHeaders = x.ChildrenWithOneOfTheseClasses([|"h1";"h2";"h3";"h4";"h5";"h6"|]).ToArray
            kidsThatAreHeaders |> Array.iteri(fun i y->
                let textComputation = computeTextForElement y false
                match x.Name with
                    | "h1" ->
                        if textComputation.TheoreticalLinesOf65Characters > 2.0 then y.Attributes.Add("readable_attr_only_content", "1") else ()
                    | "h2" ->
                        if textComputation.TheoreticalParagraphsOf3Lines > 2.0 then y.Attributes.Add("readable_attr_only_content", "1") else ()
                    | "h5" ->
                        if textComputation.TheoreticalParagraphsOf3Lines > 5.0 then y.Attributes.Add("readable_attr_only_content", "1") else ()
                    |_ ->()
                )
            let kidsThatHaveStylingContent = x.ChildrenWithOneOfTheseTags([|"b"; "i"; "e"; "strong"; "em"|]).ToArray
            kidsThatHaveStylingContent |> Array.iteri(fun i y->
                let textComputation = computeTextForElement y false
                if textComputation.TheoreticalParagraphsOf5Lines > 5.0 then y.Attributes.Add("readable_attr_only_content", "1")
                )
            // Check for big images. I don't think I can do this with WebClient
            // images in links
            let imagesInLinks = x.SelectNodes("//a/img").ToArray
            imagesInLinks |> Array.iter(fun x->
                x.Attributes.Add("readable_attr_big_image","1")
                let parentNode = x.ParentNode
                parentNode.Attributes.Add("readable_attr_mark_delete", "1")
                )
            //	table/ul/ol/div not enough words
            let trinketElementList = x.ChildrenWithOneOfTheseTags([|"ul"; "ol"; "table"; "div"; "section"; "aside"; "header"|]).ToArray
            //let KeepProcessing = ref true
            trinketElementList |> Array.iteri(fun i y->
                if (y.HasClass "readable_class_is_paragraph") =  true
                    then
                        ()
                    else
                        let textComputation = computeTextForElement y false
                        let kidsThatAreBigImages = y.SelectNodes("//img[(contains(@class,'" + "readable_class_big_image" + "'))]").ToArray.Length
                        let kidsThatAreAHeading = y.ChildrenWithOneOfTheseTags([|"h1"; "h2"; "h3"; "h4"; "h5"; "h6"|]).ToArray.Length

                        let isProbablyJustAnImage = kidsThatAreBigImages > 0 && textComputation.LinksNr < 4
                        let isAListWithLinksNotMoreThanFive = (y.Name.Contains("ul") || y.Name.Contains("ol")) && (textComputation.LinksNr< 6)
                        let hasMoreThan15WordsWithALowLinkRatio = textComputation.TextWordsNr > 15 && textComputation.LinksToTextRatio < 0.25
                        let isMoreThan2WordsWithZeroLinks = textComputation.TextWordsNr > 1 && textComputation.LinksNr < 1

                        if isMoreThan2WordsWithZeroLinks ||
                            (isMoreThan2WordsWithZeroLinks && hasMoreThan15WordsWithALowLinkRatio) ||
                            (isMoreThan2WordsWithZeroLinks && hasMoreThan15WordsWithALowLinkRatio &&  isAListWithLinksNotMoreThanFive) ||
                            (isMoreThan2WordsWithZeroLinks && hasMoreThan15WordsWithALowLinkRatio &&  isAListWithLinksNotMoreThanFive && isProbablyJustAnImage)
                            then
                                ()
                            else
                                if textComputation.LinksToTextRatio < 0.5
                                    then
                                        if textComputation.TheoreticalParagraphsOf50Words > 3.0 ||
                                            (textComputation.TheoreticalParagraphsOf50Words > 3.0 && textComputation.TheoreticalParagraphsOf3Lines > 3.0) ||
                                            (textComputation.TheoreticalParagraphsOf50Words > 3.0 && textComputation.TheoreticalParagraphsOf3Lines > 3.0 && kidsThatAreBigImages > 1) ||
                                            (textComputation.TheoreticalParagraphsOf50Words > 3.0 && textComputation.TheoreticalParagraphsOf3Lines > 3.0 && kidsThatAreBigImages > 1 && kidsThatAreAHeading > 0)
                                        then
                                            ()
                                        else
                                            y.Attributes.Add("readable_attr_mark_delete", "1")
                                            ()
                                    else
                                        y.Attributes.Add("readable_attr_mark_delete", "1")
                                        ()
                )
            let keepSomeElementsList = x.ChildrenWithOneOfTheseTags([|"pre"; "code"; "nobr"; "video"|]).ToArray
            keepSomeElementsList |> Array.iteri(fun i y->
                y.Attributes.Add("readable_attr_mark_keep", "1")
                )
            let objectsFromCertainSites = x.SelectNodes("//object/param[name='movie']").ToArray
            objectsFromCertainSites |> Array.iteri(fun i y->
                let attValue = y.Attributes.Item("value").Value
                if attValue.ContainsAny([|"flickr.com";"vimeo.com";"yahoo.com";"youtube.com"|])
                    then
                        y.ParentNode.Attributes.Add("readable_attr_mark_keep", "1")
                    else
                        ()
                )
            let embedsAndIframesFromCertainSites= x.ChildrenWithOneOfTheseTags([|"embed[src]"; "iframe[src]"|]).ToArray |> Array.iter(fun z->
                let attValue = z.Attributes.Item("src").Value
                if attValue.ContainsAny([|"flickr.com";"vimeo.com";"yahoo.com";"youtube.com"|])
                    then
                        z.Attributes.Add("readable_attr_mark_keep", "1")
                    else
                        ()
                )
            let itsAHeader = x.HasTag([|"h1";"h2";"h3";"h4";"h5";"h6"|])
            let itsAParagraphTableOrList = x.HasTag([|"p";"table";"ul";"ol"|])
            let htmlToAppend = 
                if itsAParagraphTableOrList
                    then
                        " " + "<" + x.Name.ToLower() + ">" + x.InnerHtml + "</" + x.Name.ToLower() + ">"
                    else
                        x.OuterHtml
            returnHtml.Append(htmlToAppend) |> ignore
            )
        returnHtml.ToString()
    let findHere (doc:HtmlDocument):string =
        let readableAttributes = [| "readable_attr_mark_keep"; "readable_attr_mark_delete"; "readable_attr_only_content"; "readable_attr_big_image"; |]
        let readableClasses = [| "readable_class_is_hidden"; "readable_class_is_paragraph"; "readable_class_big_image" |]
        // clear any previous work
        readableAttributes |> Array.iteri(fun i x->
            let filter = "//*[(contains(@class,'" + x + "\'))]"
            let nodesWithMatchingAttributes = doc.DocumentNode.SelectSingleNode("//body").SelectNodes(filter).ToArray
            nodesWithMatchingAttributes |> Array.iter(fun y->
                y.Attributes.Remove(x)
                )
            )
        readableClasses |> Array.iter(fun x->
            let filter = "//*[(contains(@class,'" + x + "'))]"
            let nodesWithMatchingClasses = doc.DocumentNode.SelectSingleNode("//body").SelectNodes(filter).ToArray
            nodesWithMatchingClasses |> Array.iter(fun y->
                y.RemoveClass(x)
                )
            )
        let target = findHereGetTargetElement doc
        let prevTest (nd:HtmlNode) = 
            if nd.NodeType <> HtmlNodeType.Element
            then
                false
            else
                let hasAGoodTag = nd.HasTag [|"h1";"h2";"h3";"h4";"h5";"h6";"p";"blockquote"|]
                let hasBeenIdentifiedAsAReadableParagraph = nd.HasClass("readable_class_is_paragraph")
                let hasAHeader = 
                    let h1Filter = "//h1"
                    let hasAnH1 = nd.SelectNodes(h1Filter).ToArray.Length>0
                    let headerFilter = "//header"
                    let hasAHeader = nd.SelectNodes(headerFilter).ToArray.Length>0
                    hasAnH1 || hasAHeader
                let isWasACandidate = nd.Data.ContainsKey("readable_candidate_key")
                if hasAGoodTag &&
                    hasBeenIdentifiedAsAReadableParagraph &&
                    hasAHeader &&
                    isWasACandidate
                then
                    true
                else
                    let textComputation = computeTextForElement nd false
                    if textComputation.TheoreticalParagraphsOf3Lines > 2.0 && textComputation.LinksToTextRatio < 0.10
                    then
                        true
                    else
                        false
        if target.IsNone 
        then
            ""
        else
            target.Value.Element.Attributes.Add("readable_the_target","1") |> ignore
            if (target.Value.Element.HasAttributeEuals "dir" "rtl")
            then
                //makeRTL()
                ()
            else
                ()
            let html = getHtmlToProcess([|target.Value.Element|])
            html
    let findEndTag (html:string) (tag:string) (startTagPosition:int):int option = 
        if startTagPosition < 0
            then
                None
            else
                let itsASelfClosingTag = tag.ContainsAny([|"img";"br";"hr";"embed"|])
                if itsASelfClosingTag 
                    then
                        let rg = new System.Text.RegularExpressions.Regex(">")
                        let endTagPosition = rg.Match(html, startTagPosition)
                        Some endTagPosition.lastIndex
                    else
                        let regexOpen = new System.Text.RegularExpressions.Regex("<" + tag + "[^a-z0-9]")
                        let regexClose = new System.Text.RegularExpressions.Regex("</" + tag + ">")
                        let regexCloseMain  = new System.Text.RegularExpressions.Regex("</" + tag + ">")

                        let mainMatches = regexCloseMain.Matches(html, startTagPosition).toArray
                        if mainMatches.Length = 0
                            then
                                None
                            else
                                let foundOne = mainMatches |> Array.tryFindIndex(fun y->
                                    let wholeTagString = y.Value
                                    let openNextRecord = regexOpen.Match(wholeTagString)
                                    let openNextRecordLength = if openNextRecord.Success then openNextRecord.Length else 0
                                    let closeNextRecord = regexClose.Match(wholeTagString)
                                    let closeNextRecordLength = if closeNextRecord.Success then closeNextRecord.Length else 0
                            
                                    let openNextRecordLastIndex = openNextRecord.lastIndex
                                    let closeNextRecordLastIndex = closeNextRecord.lastIndex
                                    if openNextRecordLastIndex > 0 && openNextRecordLastIndex = closeNextRecord.lastIndex
                                        then
                                            true
                                        else
                                            false
                                    )
                                if foundOne.IsSome then Some mainMatches.[foundOne.Value].lastIndex else None

    let processHtml (htmlInput:string) =         
        let htmlRemoveComments = htmlInput.ReplaceWithRegex "<!--[\s\S]*?-->" ""
        let htmlNormalize1 = htmlRemoveComments.ReplaceWithRegex "<\s+" "<"
        let htmlNormalize2 = htmlNormalize1.ReplaceWithRegex "\s+>" ">"
        let htmlNormalize3 = htmlNormalize2.ReplaceWithRegex "\s+\/>" "/>"
        let htmlNormalize4 = htmlNormalize3.ReplaceWithRegex "\s*=\s*" "="
        let htmlNormalize5 = htmlNormalize4.ReplaceWithRegex "(&nbsp;)+" " "
        let holdRepository = new System.Collections.Generic.Dictionary<string, string>()
        let readableRegex = new System.Text.RegularExpressions.Regex("""<([a-z0-9]+)([^>]+?)readable_attr_(big_image|mark_keep|mark_delete|only_content)="1"([^>]*)>""")
        let holdRepositoryLength = ref 0

        let rec processReadableTags (accHtml:string) =
            let readableTagMatch = readableRegex.Match(accHtml)
            if readableTagMatch.Success = false
                then
                    accHtml
                else
                    let whole = readableTagMatch.Groups.[0]
                    let operation = readableTagMatch.Groups.[3]
                    let tag = readableTagMatch.Groups.[1]
                    let startTagPosition = readableTagMatch.lastIndex - whole.Length
                    let endTagPosition = findEndTag htmlNormalize5 tag.Value startTagPosition
                    if endTagPosition.IsNone
                        then
                            accHtml
                        else
                            let endTagPositionValue = endTagPosition.Value
                            let subHtml = readableTagMatch.Value.Substring(startTagPosition, endTagPosition.Value)
                            match operation.Value with
                                | "big_image" ->
                                    holdRepository.Add("i" + holdRepositoryLength.ToString(), "<div class=\"readableBigImage\">" + subHtml + "</div>")
                                    let newAcc = accHtml.Substring(0, startTagPosition) + "[=readable_hold(" + holdRepositoryLength.ToString() + "')]" + accHtml.Substring(endTagPositionValue)
                                    holdRepositoryLength:=!holdRepositoryLength+1
                                    processReadableTags newAcc
                                | "mark_keep" ->
                                    holdRepository.Add("i" + holdRepositoryLength.ToString(), subHtml)
                                    let newAcc = "" + accHtml.Substring(0, startTagPosition) + "[=readable_hold(" + holdRepositoryLength.ToString() + ")]" + accHtml.Substring(endTagPositionValue)
                                    holdRepositoryLength:=!holdRepositoryLength+1
                                    processReadableTags newAcc
                                | "mark_delete" ->
                                    let newAcc = "" + accHtml.Substring(0, startTagPosition) + " " + accHtml.Substring(endTagPositionValue)
                                    processReadableTags newAcc
                                | "only_content" ->
                                    let replacement1 = System.Text.RegularExpressions.Regex.Replace(subHtml, "^<[^>]+>", "")
                                    let replacement2 = System.Text.RegularExpressions.Regex.Replace(replacement1, "<\/[^>]+>$", "")
                                    let newAcc = "" + accHtml.Substring(0, startTagPosition) + "<div>" + replacement2 + "</div>" + accHtml.Substring(endTagPositionValue)
                                    processReadableTags newAcc
                                |_ ->
                                    ""
        let readableTagsProcessed = processReadableTags htmlNormalize5

        let stripThingsWeCouldntCareLessAbout1 = readableTagsProcessed.ReplaceWithRegex "<(button|canvas|embed|frame|input|iframe|link|map|marquee|nobr|noframes|noscript|object|script|select|style|textarea)[\s\S]*?<\/\1>" ""
        let stripThingsWeCouldntCareLessAbout2 = stripThingsWeCouldntCareLessAbout1.ReplaceWithRegex "<(button|canvas|embed|frame|input|iframe|link|map|marquee|nobr|noframes|noscript|object|script|select|style|textarea)[\s\S]*?>" ""

        let replaceBoldItalic1 = stripThingsWeCouldntCareLessAbout2.ReplaceWithRegex "<span style=\"font-weight:\s*bold[^>]*>([^>]+?)<\/span>" "<b>$1</b>"
        let replaceBoldIalic2 = replaceBoldItalic1.ReplaceWithRegex "<span style=\"font-style:\s*italic[^>]*>([^>]+?)<\/span>" "<i>$1</i>"

        // Remove most all attributes from elements
        let attributePreg = new System.Text.RegularExpressions.Regex("\s*([a-z0-9_-]+)=\"[^\"]*\"")
        let attributeRegexEvaluator (currentMatch:System.Text.RegularExpressions.Match):string =
            let v = currentMatch.Value
            let m = currentMatch.Groups.[0].Value
            let k = currentMatch.Groups.[1].Value
            let a = currentMatch.Groups.[2].Value
            let tag = k.ToLower()

            match tag with
                | "a" ->
                    let inside = attributePreg.Replace(a,(fun (mt:System.Text.RegularExpressions.Match)->
                        let matchesId = mt.Value.ContainsRegex("^(href|id|name|title)$")
                        if matchesId = true then mt.Value else ""
                        ))
                    "" + "<" + k + " target=\"_blank\"" + inside + ">"
                | "img" ->
                    let inside = attributePreg.Replace(a,(fun (mt:System.Text.RegularExpressions.Match)->
                        let matchesId = mt.Value.ContainsRegex("^(height|id|src|width|title)$")
                        if matchesId = true then mt.Value else ""
                        ))
                    "" + "<" + k + inside + "/>"
                | "td" | "th" ->
                    let inside = attributePreg.Replace(a,(fun (mt:System.Text.RegularExpressions.Match)->
                        let matchesId = mt.Value.ContainsRegex("^(colspan|id|rowspan)$")
                        if matchesId = true then mt.Value else ""
                        ))
                    "" + "<" + k + inside + "/>"
                |_ ->
                    let inside = attributePreg.Replace(a,(fun (mt:System.Text.RegularExpressions.Match)->
                        let matchesId = mt.Value.ContainsRegex("^(id)$")
                        if matchesId = true then mt.Value else ""
                        ))
                    "" + "<" + k + inside + ">"
        let attributeCleaned = System.Text.RegularExpressions.Regex.Replace(replaceBoldIalic2, "<([a-z0-9_-]+)( [^>]+)>", attributeRegexEvaluator)
        let makeBRsOutOfEmptyPs = attributeCleaned.ReplaceWithRegex "<p>\s*<\/p>" "<br/><br/>"
        let deleteSoftBRs = makeBRsOutOfEmptyPs.ReplaceWithRegex "<br[^>]*?soft[^>]*>" ""
        let normalizeBRsandHRs = deleteSoftBRs.ReplaceWithRegex "<(br|hr)[^>]*>" "<$1/>"
        let removeAllSelfClosingElementsExceptForBRandHR = normalizeBRsandHRs.ReplaceWithRegex "<(?!(br|hr|img))([^>]+)\/>" ""
        let removeTagsOfSomeElements = removeAllSelfClosingElementsExceptForBRandHR.ReplaceWithRegex "<\/?(body|center|fieldset|font|form|span)([^>]*)>" ""
        let removeElementsWithUnderscoreInTheirDefinition = removeTagsOfSomeElements.ReplaceWithRegex "<\/?([a-z]+)(_|:)([a-z]+)([^>]*)>" ""
        let makePsOutOfTextWithBRsSplatteredThroughout = removeElementsWithUnderscoreInTheirDefinition.ReplaceWithRegex "<br\/>\s*<br\/>" "</p><p>"
        let removeBrAfterP = makePsOutOfTextWithBRsSplatteredThroughout.ReplaceWithRegex "<\/(div|h\d|ol|p|table|ul)>\s*<br\/>" "</$1>"
        let removeBrAfterP2 = removeBrAfterP.ReplaceWithRegex "<br\/>\s*<(div|h\d|ol|p|table|ul)>" "</$1>"
        let removeBrInsideAPOrDiv = removeBrAfterP2.ReplaceWithRegex "<(p|div)>\s*<br\/>\s*<\/\1>" ""
        let removeEmptyLIs = removeBrInsideAPOrDiv.ReplaceWithRegex "<li[^>]*>\s*<\/li>" ""
        let removeEmptyULs = removeEmptyLIs.ReplaceWithRegex "<(ul|ol)[^>]*>\s*<\/\1>" ""
        let removeEmptyDivsAndPs = removeEmptyULs.ReplaceWithRegex "<(div|p)[^>]*>\s*<\/\1>" ""
        let removeUselessLineFeedCarriageReturnPairs = removeEmptyDivsAndPs.Replace("\r\n", "")

        // put stuff back
        let readableRegex = new System.Text.RegularExpressions.Regex("\[=readable_hold\(([0-9]+)\)\]")
        let putReadableStuffBackCallback (mt:System.Text.RegularExpressions.Match) =
            let regmatch = mt.Groups.[0]
            let key = mt.Groups.[1].Value
            let lookupItem = holdRepository.TryFind ("i" + key)
            let temp = if lookupItem.IsSome then lookupItem.Value else ""
            let insideRegEx = new System.Text.RegularExpressions.Regex("<([^>]+?)style=\"[^\">]+\"([^>]*)>")
            let ret = insideRegEx.Replace(temp,"<$1$2>")
            ret
        let finalReplaced = readableRegex.Replace(removeUselessLineFeedCarriageReturnPairs, putReadableStuffBackCallback)
        finalReplaced
    let processLinks links (opts:RipTextProgramConfig) =
        links |> Array.iter(fun x->
            let link = x
            opts.configBase.interimProgress.addItem "FILEATTEMPTED" (sprintf "Attempting to process link: %s" link)
            //let newFileName = opts.rippedSubdirectory.parameterValue + "/" + (Utils.makeUrlIntoAFilename link)
            let tempLocalName = makeUrlIntoAFilename x
            let newFileName = fullyQualifiedLocalName opts.rippedSubdirectory.parameterValue opts.outputFilePrefix.parameterValue tempLocalName ""
            opts.configBase.interimProgress.addItem "NEWFILENAMECREATED" (sprintf "Created New FileName: %s" newFileName)
            if newFileName.Length > 242
                then
                    let pathTooLongMessage = sprintf "PATH TOO LONG. CANT SAVE. PATH IS %i characters." newFileName.Length
                    opts.configBase.interimProgress.addItem "PATHTOOLONG" pathTooLongMessage
                    () 
                else
                    try
                        match (x.GetRight 4) with
                            | ".pdf" ->
                                let fileUri = System.Uri.TryCreate(link, System.UriKind.Absolute)
                                let fixedFileName = (newFileName.TrimRight 4) + ".pdf"
                                if fst fileUri then downloadFile "Random" "Random" (snd fileUri)  fixedFileName else ()
                                opts.configBase.interimProgress.addItem "FILEPROCESSED" (sprintf "Successfully processed link: %s" link)
                            | ".htm" | "html" | ".php" |_->
                                let doc = loadDoc x
                                opts.configBase.interimProgress.addItem "HTMLLOADED" (sprintf "Html downloaded without errors")
                                // sanity check. Sometimes they stream pdfs from a php or other link
                                if doc.DocumentNode.InnerText.StartsWith("%PDF")
                                    then
                                        let fileUri = System.Uri.TryCreate(link, System.UriKind.Absolute)
                                        let fixedFileName = (newFileName.TrimRight 4) + ".pdf"
                                        if fst fileUri then downloadFile "Random" "Random" (snd fileUri)  fixedFileName else ()
                                        opts.configBase.interimProgress.addItem "FILEPROCESSED" (sprintf "Successfully processed link: %s" link)
                                    else
                                        let title = doc.InnerTextOfFirstNodeOrEmpty("//title")
                                        let description = 
                                            let nds = doc.DocumentNode.SelectNodes("//meta[@name=\'description\']")
                                            if nds <> null && nds.Count >0 then nds.[0].Attributes.Item("content").Value else ""
                                        let footer = "Text-clipping service provided by Newspaper23.com"
                                        let htmlPrefix = 
                                            if opts.headerHtmlFileExists
                                                then
                                                    let temp = System.IO.File.ReadAllText(opts.headerHtmlFileName.parameterValue)
                                                    opts.configBase.interimProgress.addItem "HEADERTEMPLATELOAD" ("Html header template " + opts.headerHtmlFileName.parameterValue + " loaded")
                                                    temp
                                                else
                                                    let temp = """<!DOCTYPE html><html lang="en"><head><link rel="stylesheet" type="text/css" href="http://www.newspaper23.com/css/main.css"></head><body class="articleBody"><div class="articleContainer">"""
                                                    opts.configBase.interimProgress.addItem "HEADERTEMPLATELOAD" ("Html header template " + opts.headerHtmlFileName.parameterValue + " FAILED TO LOAD")
                                                    temp
                                        let htmlSuffix = 
                                            if opts.footerHtmlFileExists
                                                then
                                                    let temp= System.IO.File.ReadAllText(opts.footerHtmlFileName.parameterValue)
                                                    opts.configBase.interimProgress.addItem "FOOTERTEMPLATELOAD" ("Html footer template " + opts.footerHtmlFileName.parameterValue + " loaded")
                                                    temp
                                                else
                                                    let temp="""</div><script>(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){ (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o), m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)  })(window,document,'script','http://www.google-analytics.com/analytics.js','ga');  ga('create', 'UA-29994656-1', 'auto');  ga('send', 'pageview');</script><script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js"></script><script type="text/javascript">window.jQuery || document.write('<script type="text/javascript" src="http://newspaper23.com/js/jquery-2.0.3.min.js"><\/script>')</script><script src="http://www.newspaper23.com/js/main.js" type="text/javascript"></script></body></html>"""
                                                    opts.configBase.interimProgress.addItem "FOOTERTEMPLATELOAD" ("Html footer template " + opts.footerHtmlFileName.parameterValue + " FAILED TO LOAD")
                                                    temp
                                        let processedHtml =
                                            try
                                                let targetHtml = findHere doc
                                                opts.configBase.interimProgress.addItem "FINDHERECOMPLETED" "findHere Returned"
                                                let temp = processHtml targetHtml
                                                opts.configBase.interimProgress.addItem "TARGETHTMLCOMPLETED" "targetHtml Returned"
                                                temp
                                            with
                                                | :? System.Exception as ex ->
                                                    let temp = "<h1>" + ex.Message + "</h1>" + "<p>" + ex.StackTrace + "</p>"
                                                    opts.configBase.interimProgress.addItem "PROCESSHTMLFAIL" "Procssing Html Failed"
                                                    temp
                                        let framedHtml = htmlPrefix + processedHtml + htmlSuffix

                                        let newDoc = new HtmlAgilityPack.HtmlDocument()
                                        newDoc.OptionDefaultStreamEncoding<- new System.Text.UTF8Encoding(true)
                                        newDoc.LoadHtml(framedHtml)
                                    
                                        let heading = newDoc.CreateElement("h1")
                                        heading.AddClass("ripTextMainHeading")
                                        heading.AppendChild(newDoc.CreateTextNode(title)) |> ignore                        
                                        let subHead = newDoc.CreateElement("h2")
                                        subHead.AddClass("ripTextSubHead")
                                        let headerLink = newDoc.CreateElement("a")
                                        headerLink.Attributes.Add("href", link)
                                        let url = new System.Uri(link)
                                        headerLink.AppendChild(newDoc.CreateTextNode(url.Host.ToLower())) |> ignore
                                        subHead.AppendChild(headerLink) |> ignore
                                        let lastChild = 
                                            let temp = newDoc.DocumentNode.SelectSingleNode("//body/div")
                                            if temp <> null then temp else newDoc.DocumentNode.SelectSingleNode("//*")
                                        lastChild.PrependChild(subHead) |>ignore
                                        lastChild.PrependChild(heading) |>ignore

                                        let footerGraph = newDoc.CreateElement("p")
                                        let footerAnchor = newDoc.CreateElement("a")
                                        footerAnchor.SetAttributeValue("href", "http://newspaper23.com") |> ignore
                                        footerGraph.AddClass("ripTextFooterGraph")
                                        footerAnchor.AppendChild(newDoc.CreateTextNode(footer)) |> ignore
                                        footerGraph.AppendChild(footerAnchor) |> ignore

                                        lastChild.AppendChild(footerGraph) |> ignore                                    
                                        try
                                            opts.configBase.interimProgress.addItem "HTMLFILECONTENTS" newDoc.DocumentNode.OuterHtml
                                            newDoc.Save(newFileName)
                                            opts.configBase.interimProgress.addItem "FILEPROCESSED" (sprintf "Successfully processed link: %s" link)
                                        with
                                            // can't save anything to disk. Nothing to do. Keep going.
                                            | :? System.Exception as ex ->
                                            opts.configBase.interimProgress.addItem "CANTSAVE" ("Can't save anything to disk for file " + link)
                                            System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                                            System.Console.WriteLine ("Processing file: " + x)
                                            System.Console.WriteLine ()
                                            ()
                    with
                        | :? System.Exception as ex ->
                            let htmlLoadFailMessage = 
                                "Failure loading Html Document: " + ex.Message + " - " + ex.StackTrace + 
                                    if ex.InnerException = null then "" else ex.InnerException.Message + " - " + ex.InnerException.StackTrace
                            opts.configBase.interimProgress.addItem "HTMLLOADFAIL" htmlLoadFailMessage
                            System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                            System.Console.WriteLine (ex.StackTrace)
                            // default doc when the server refuses to send anything
                            let doc = new HtmlAgilityPack.HtmlDocument()
                            let title = "Document Did Not Load"
                            let description = 
                                opts.configBase.interimProgress.addItem "SERVERNOLOAD" ("Server refuses to load file " + link)
                                let tempDescription = "The server refused to load this document - " + ex.Message + " - " + ex.StackTrace
                                if ex.InnerException = null
                                     then
                                        tempDescription
                                    else
                                        tempDescription + "\r\n<br/><br/>" + " Inner " + ex.InnerException.Message + "-" + ex.InnerException.StackTrace
                            let footer = "For more information, please write your congressman"
                            // just do some dummy stuff for now
                            let newDoc = new HtmlAgilityPack.HtmlDocument()
                            let heading = newDoc.CreateElement("h1")
                            heading.AppendChild(newDoc.CreateTextNode(title)) |> ignore
                            let mainGraph = newDoc.CreateElement("p")
                            mainGraph.AppendChild(newDoc.CreateTextNode(description)) |> ignore
                            let mainFrame = newDoc.CreateElement("iframe")
                            mainFrame.Attributes.Add("src", link)
                            mainFrame.Attributes.Add("height", "75%")
                            mainFrame.Attributes.Add("width", "100%")
                            let footerGraph = newDoc.CreateElement("p")
                            footerGraph.AppendChild(newDoc.CreateTextNode(footer)) |> ignore
                            let directLink = newDoc.CreateElement("a")
                            directLink.AppendChild(newDoc.CreateTextNode("Original Article")) |> ignore
                            directLink.Attributes.Add("href", link) |> ignore
                            newDoc.DocumentNode.AppendChild(heading) |> ignore
                            newDoc.DocumentNode.AppendChild(mainGraph) |> ignore
                            newDoc.DocumentNode.AppendChild(mainFrame) |> ignore
                            newDoc.DocumentNode.AppendChild(directLink) |> ignore
                            newDoc.DocumentNode.AppendChild(footerGraph) |> ignore
                            try
                                newDoc.Save(newFileName)
                            with
                                // can't save anything to disk. Nothing to do. Keep going.
                                | :? System.Exception as ex ->
                                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                                System.Console.WriteLine ("Processing file: " + x)
                                System.Console.WriteLine ()
                            ()
                    )

    let doStuff (opts:RipTextProgramConfig) =
        let linksToProcess = getUrlList opts
        processLinks linksToProcess opts
        
        // Report interim progress
        match opts.configBase.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                ()
            | Verbosity.Minimum ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                ()
            | Verbosity.BatchNormal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEATTEMPTED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PATHTOOLONG"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEPROCESSED"))
                ()
            | Verbosity.Normal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FilesNeedingProcessing"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEATTEMPTED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PATHTOOLONG"))
                printfn "%s" (opts.configBase.interimProgress.getItem("HTMLLOADED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEPROCESSED"))
            | Verbosity.BatchVerbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LinesToProcess"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FilesNeedingProcessing"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEATTEMPTED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("NEWFILENAMECREATED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PATHTOOLONG"))
                printfn "%s" (opts.configBase.interimProgress.getItem("HTMLLOADED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("HTMLLOADFAIL"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FINDHERECOMPLETED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("TARGETHTMLCOMPLETED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSHTMLFAIL"))                
                printfn "%s" (opts.configBase.interimProgress.getItem("HEADERTEMPLATELOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FOOTERTEMPLATELOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEPROCESSED"))
                ()
            | Verbosity.Verbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LinesToProcess"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FilesNeedingProcessing"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEATTEMPTED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("NEWFILENAMECREATED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PATHTOOLONG"))
                printfn "%s" (opts.configBase.interimProgress.getItem("HTMLLOADED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("HTMLLOADFAIL"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FINDHERECOMPLETED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("TARGETHTMLCOMPLETED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("PROCESSHTMLFAIL"))                
                printfn "%s" (opts.configBase.interimProgress.getItem("HEADERTEMPLATELOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FOOTERTEMPLATELOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEPROCESSED"))
                printfn "---- HTML CONTENTS ---- \r\n%s" (opts.configBase.interimProgress.getItem("HTMLFILECONTENTS"))
            
                ()
            |_ ->
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEATTEMPTED"))
                printfn "%s" (opts.configBase.interimProgress.getItem("SERVERNOLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("CANTSAVE"))
                printfn "%s" (opts.configBase.interimProgress.getItem("HEADERTEMPLATELOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FOOTERTEMPLATELOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("FILEPROCESSED"))
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
                opts.inputSelectedArticleListFileName.printHelp
                opts.headerHtmlFileName.printHelp
                opts.footerHtmlFileName.printHelp
                opts.urlToProcess.printHelp
                opts.outputFilePrefix.printHelp
                opts.desiredLinkCountToProcess.printHelp
                opts.rippedSubdirectory.printHelp
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

//    [<EntryPoint>]
//    let main argv = 
//        try
//            let opts = parseCommandLineArgs (Array.toList argv)
//            if opts.verbose >= Verbosity.BatchNormal then printfn "RipText: Rip plain text from html files. Ran at %A"  System.DateTime.UtcNow
//            if opts.verbose > Verbosity.BatchNormal then
//                printfn "Options Selected:"
//                printfn "%A" opts
//                printfn ""
//            let linksToProcess = getUrlList opts
//            processLinks linksToProcess opts
//            0 // return an integer exit code
//        with
//            | :? UserNeedsHelp as hex -> 
//                System.Console.WriteLine("You'd like help")
//                System.Console.WriteLine("Good luck with that")
//                System.Console.WriteLine("/V:<1-7> to change verbosity settings")
//                System.Console.WriteLine("/I:<filename> to set the selected article list input file")
//                System.Console.WriteLine("/P:<string> to set the prefix to put in place for ripped files")
//                System.Console.WriteLine("/S:<url> to manually pass in an URL to process. Overrides using selectedarticle list file.")
//                System.Console.WriteLine("/N:<integer> number of links desired")
//                System.Console.WriteLine("")
//                0
//            | :? ExpectedResponseFail as reason ->
//                System.Console.WriteLine("Expected Response Fail")
//                System.Console.WriteLine(reason)
//                0
//            | :? System.Exception as ex ->
//                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
//                0
//




//
//    let rec recParseCommandLineArgs argv options:RipTextConfig=
//        match argv with
//            | [] -> options
//            | x::xs ->
//                let sX = x.ToString()
//                match Utils.getLeft sX 2 with
//                | "/?" | " ?" | "-?" | "--help"  ->
//                    raise (UserNeedsHelp("General"))
//                | "/I" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with inputSelectedArticleListFileName=argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/H" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with headerHtmlFile =argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/F" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with footerHtmlFile=argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/S" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with urlToProcess=argParms.[1]; mode=URL}
//                    recParseCommandLineArgs xs newOptions
//                | "/P" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with outputFileprefix=argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/N" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with desiredLinkCountToProcess=System.Int32.Parse("0" + argParms.[1])}
//                    recParseCommandLineArgs xs newOptions
//                | "/V" ->
//                    let newOptions = 
//                        try
//                            let argParms = sX.Split([|':'|],2)
//                            let parmVerbosityLevel = System.Int32.Parse("0" + argParms.[1])
//                            {options with verbose=enum<Verbosity>(parmVerbosityLevel)}
//                        with _->
//                            {options with verbose=Verbosity.Verbose}
//                    if newOptions.verbose >= Verbosity.BatchVerbose then
//                        printfn "===================="
//                        printfn "VERBOSE SETTINGS ON %A" newOptions.verbose
//                        printfn "===================="
//                    recParseCommandLineArgs xs newOptions
//                |_ ->
//                    printfn "The option %A is unrecognized" x
//                    recParseCommandLineArgs xs options



//
//    let parseCommandLineArgs argv =
//        let defaultOptions = {
//            inputSelectedArticleListFileName=""
//            urlToProcess=""
//            mode = SelectedArticleListFile
//            outputFileprefix=""
//            verbose=Verbosity.Normal
//            desiredLinkCountToProcess = 1
//            inputSelectedArticleListFileExists=false
//            rippedSubdirectory = "ripped"
//            headerHtmlFile=""
//            footerHtmlFile=""
//            headerHtmlFileExists=false
//            footerHtmlFileExists=false
//            }
//        // make sure there's a config file and output file
//        let initialReturn = recParseCommandLineArgs argv defaultOptions
//        let newInputFileExists = if initialReturn.inputSelectedArticleListFileName.Length >0 then System.IO.File.Exists(initialReturn.inputSelectedArticleListFileName) else false
//        let newHeaderHtmlFileExists = if initialReturn.headerHtmlFile.Length >0 then System.IO.File.Exists(initialReturn.headerHtmlFile) else false
//        let newFooterHtmlFileExists = if initialReturn.footerHtmlFile.Length >0 then System.IO.File.Exists(initialReturn.footerHtmlFile) else false
//        let newMode = match initialReturn.inputSelectedArticleListFileName, initialReturn.urlToProcess with
//                        | "", "" ->
//                            raise (ExpectedResponseFail("No URL or selected article list was provided. There is nothing to process."))
//                        | "", _ ->
//                            URL
//                        | _, "" ->  
//                            SelectedArticleListFile
//                        | _, _->
//                            SelectedArticleListFile        
//        {initialReturn with mode= newMode; inputSelectedArticleListFileExists = newInputFileExists; headerHtmlFileExists=newHeaderHtmlFileExists; footerHtmlFileExists=newFooterHtmlFileExists}
