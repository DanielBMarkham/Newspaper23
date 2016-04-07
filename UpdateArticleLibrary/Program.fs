module main
    open Types
    open Utils
    open Persist
    open HtmlAgilityPack
    open System.Net

    let defaultBaseOptions = createNewBaseOptions "UpdateArticleLibrary" "Take the new article links from aggregate sites and save them in a library." [|"Use the plain text ArticleLibrary format."|] defaultVerbosity
    let defaultInputFileName = createNewConfigEntry "I" "Input File" [|"/I:<filename> -> The incoming link list to process."|] ""
    //let defaultConfigFileName = createNewConfigEntry "C" "Input File" [|"/C:<filename> -> Configuration file."|] ""
    let defaultLibraryFileName = createNewConfigEntry "L" "Input File" [|"/L:<filename> -> Library file to update."|] ""
    let defaultInputFileExists = false
    //let defaultConfigFileExists = false
    let defaultLibraryFileExists = false

    let loadConfigFromCommandLine (args:string []):UpdateArticleLibraryProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newInputFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultInputFileName, args)
        //let newConfigFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultConfigFileName, args)
        let newLibraryFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultLibraryFileName, args)
        let newInputFileExists = System.IO.File.Exists(newInputFileName.parameterValue)
        //let newConfigFileExists = System.IO.File.Exists(newConfigFileName.parameterValue)
        let checkedNewLibraryFileName =
            if newLibraryFileName.parameterValue.Length = 0
                then
                    newLibraryFileName.swapInNewValue (newInputFileName.parameterValue.Replace("/","-") + ".articlelibrary")
                else
                    newLibraryFileName
        let newLibraryFileExists = System.IO.File.Exists(newLibraryFileName.parameterValue)
        {
            configBase=newConfigBase
            inputFileName=newInputFileName
            //configFileName=newConfigFileName
            libraryFileName=checkedNewLibraryFileName
            inputFileExists=newInputFileExists
            //configFileExists=newConfigFileExists
            libraryFileExists=newLibraryFileExists
        }


    let loadIncomingFile (opts:UpdateArticleLibraryProgramConfig) =
        if opts.inputFileExists
            then 
                let nRipTime = System.IO.FileInfo(opts.inputFileName.parameterValue).CreationTimeUtc
                let tempIncomingNewLinks = System.IO.File.ReadAllText(opts.inputFileName.parameterValue)
                let incomingNewLinks = tempIncomingNewLinks.TrimRight 2 // there's an additional line return on the output file of the other prog
                let inSections = incomingNewLinks.Split([|"\r\n\r\n"|], System.StringSplitOptions.None)
                let sections = Array.sub inSections 1 (inSections.Length-1)
                let libraryIncomingLinksSectionCountMessage = sprintf "Incoming Links Section Count is %i" sections.Length
                opts.configBase.interimProgress.addItem "LIBRARYINCOMINGSECTIONCOUNT" libraryIncomingLinksSectionCountMessage
                let nArticles = new System.Collections.Generic.Dictionary<string, string[]>()
                sections |> Array.iteri(fun i x->
                    let trimmedX = x.TrimLeft 2
                    let sectionItems = trimmedX.Split([|"\r\n"|], System.StringSplitOptions.None)
                    let newSectionTitle = sectionItems.[0].TrimBoth 1 1
                    let newSectionItems = Array.sub sectionItems 1 (sectionItems.Length-1) 
                    if (newSectionTitle.Trim().Length > 0) && (nArticles.ContainsKey(newSectionTitle) = false)
                        then
                            nArticles.Add(newSectionTitle, newSectionItems)
                        else
                            ()

                    )
                let linkTriples = Array.toList(nArticles.Item("ArticleTitles") |> Array.mapi(fun i x->
                    (x, nArticles.Item("ArticleLinks").[i], nRipTime.ToString()) 
                    ))
                let incomingFileTriplesCountMessage = sprintf "There are %i initial triples in the incoming file" linkTriples.Length
                opts.configBase.interimProgress.addItem "INCOMINGFILETRIPLESCOUNT" incomingFileTriplesCountMessage
                let linkDict = new System.Collections.Generic.Dictionary<string, titleTimeTuple>()
                let titleDict = new System.Collections.Generic.Dictionary<string, linkTimeTuple>()
                linkTriples |> List.iter(fun (a,b,c)->
                    if (linkDict.ContainsKey(b)=false) && (titleDict.ContainsKey(a)=false) then
                        linkDict.Add(b,{title=a;time=c})
                        titleDict.Add(a,{link=b;time=c}))
                //if opts.verbose then printfn "After filtering for either duplicate titles or links, there are now %A entries in the incoming file" linkDict.Count
                {
                    fileLastModified=nRipTime
                    initialList=linkTriples
                    byLink=linkDict
                    byTitle=titleDict
                }
            else
                raise(ExpectedResponseFail("The Incoming Input file named " + opts.inputFileName.parameterValue + " was not found"))
        
    let filterIncomingToAddList (opts:UpdateArticleLibraryProgramConfig) origLibrary incomingFileData =
        let returnInitialList = incomingFileData.initialList |> List.filter(fun x->
            let a,b,c = x
            (origLibrary.byTitle.ContainsKey(a)=false) && (origLibrary.byLink.ContainsKey(b)) = false)
        let dupesAgainstSelfMessage = sprintf "After filtering the incoming file data for dupes against itself, there are %i links" returnInitialList.Length
        opts.configBase.interimProgress.addItem "DUPESAGAINSTSELF" dupesAgainstSelfMessage
        let returnByLink = System.Collections.Generic.Dictionary<string, titleTimeTuple>()
        let returnByTitle= System.Collections.Generic.Dictionary<string, linkTimeTuple>()
        returnInitialList |> List.iter(fun x->
            let a,b,c = x
            if ((returnByLink.ContainsKey(b) = false) && (returnByTitle.ContainsKey(a)) = false)
                &&
                ((origLibrary.byLink.ContainsKey(b) = false) && (origLibrary.byTitle.ContainsKey(a)) = false)
                then
                    returnByLink.Add(b, {title=a;time=c})
                    returnByTitle.Add(a, {link=b;time=c})
            )
        let newReturnList = returnInitialList |> List.filter(fun x->    
            let a,b,c = x
            returnByTitle.ContainsKey(a)
            )
        {
            fileLastModified=System.DateTime.UtcNow
            initialList=newReturnList
            byLink=returnByLink
            byTitle=returnByTitle
        }


    let doStuff (opts:UpdateArticleLibraryProgramConfig) =
        let origLibrary = loadLibrary opts
        let libLoadMessage = sprintf "Article library: %s loaded. %i Article Count in initial library" opts.libraryFileName.parameterValue origLibrary.byTitle.Count
        opts.configBase.interimProgress.addItem "LIBRARYLOAD" libLoadMessage
        
        let incomingFileData = loadIncomingFile opts
        let incomingFileLoadMessage = sprintf "Incoming GetLinks List: %s loaded. %i links in list" opts.inputFileName.parameterValue incomingFileData.initialList.Length
        opts.configBase.interimProgress.addItem "LINKLISTLOAD" incomingFileLoadMessage

        let listToAdd = filterIncomingToAddList opts origLibrary incomingFileData
        let dupeFilterMessage = sprintf "After filtering for dupes, there are %i articles to add to the library" listToAdd.initialList.Length
        opts.configBase.interimProgress.addItem "DUPEFILTERINFO" dupeFilterMessage

        let appendPrefix = 
            if opts.libraryFileExists then "\r\n" else ""
        let textToAdd =  
            if listToAdd.initialList.Length >0 then
                appendPrefix +
                (listToAdd.initialList |> List.map(fun x->
                    let a,b,c=x
                    a + "\r\n" + b + "\r\n" + c) |> String.concat "\r\n")
            else ""
        System.IO.File.AppendAllText(opts.libraryFileName.parameterValue, textToAdd)
        opts.configBase.interimProgress.addItem "WRITESUCCESS" "File opened and text appended successfully"

        // Report interim progress
        match opts.configBase.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                ()
            | Verbosity.BatchNormal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("WRITESUCCESS"))
                ()
            | Verbosity.Normal ->
                printfn "%s" (opts.configBase.interimProgress.getItem("DUPEFILTERINFO"))
                printfn "%s" (opts.configBase.interimProgress.getItem("WRITESUCCESS"))
            | Verbosity.BatchVerbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("LIBRARYLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LINKLISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("DUPEFILTERINFO"))
                printfn "%s" (opts.configBase.interimProgress.getItem("WRITESUCCESS"))
                ()
            | Verbosity.Verbose ->
                printfn "%s" (opts.configBase.interimProgress.getItem("LIBRARYLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LIBRARYINCOMINGSECTIONCOUNT"))
                printfn "%s" (opts.configBase.interimProgress.getItem("LINKLISTLOAD"))
                printfn "%s" (opts.configBase.interimProgress.getItem("INCOMINGFILETRIPLESCOUNT"))
                printfn "%s" (opts.configBase.interimProgress.getItem("DUPESAGAINSTSELF"))
                printfn "%s" (opts.configBase.interimProgress.getItem("DUPEFILTERINFO"))
                printfn "%s" (opts.configBase.interimProgress.getItem("WRITESUCCESS"))
                ()
            |_ ->
                printfn "%s" (opts.configBase.interimProgress.getItem("DUPEFILTERINFO"))
                printfn "%s" (opts.configBase.interimProgress.getItem("WRITESUCCESS"))
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
                opts.inputFileName.printHelp
                opts.libraryFileName.printHelp
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


//
//    [<EntryPoint>]      
//    let main argv = 
//        try
//            let opts = parseCommandLineArgs (Array.toList argv)
//            if opts.verbose then
//                printfn "Options Selected:"
//                printfn "%A" opts
//                printfn ""
//            let origLibrary = loadLibrary opts
//            if opts.verbose then
//                printfn "Existing library loaded"
//                printfn "%A Article Count in initial library" origLibrary.byTitle.Count
//            let incomingFileData = loadIncomingFile opts 
//            if opts.verbose then
//                let maxIncomingCount = incomingFileData.initialList.Length
//                printfn "There are %A articles in the incoming list" maxIncomingCount
//            let listToAdd = filterIncomingToAddList opts origLibrary incomingFileData
//            if opts.verbose then
//                printfn "After filtering for dupes, there are %A articles to add to the library" (listToAdd.initialList.Length)
//            let appendPrefix = 
//                if opts.libraryFileExists then "\r\n" else ""
//            let textToAdd =  
//                if listToAdd.initialList.Length >0 then
//                    appendPrefix +
//                    (listToAdd.initialList |> List.map(fun x->
//                        let a,b,c=x
//                        a + "\r\n" + b + "\r\n" + c) |> String.concat "\r\n")
//                else ""
//            if opts.verbose then printfn "Opening file %A to append the new articles to the end" opts.libraryFileName
//            System.IO.File.AppendAllText(opts.libraryFileName, textToAdd)
//            if opts.verbose then printfn "File opened and text appended successfully"
//            //System.Console.WriteLine("Press Any Key to Exit")
//            //System.Console.ReadKey() |> ignore
//            0 // return an integer exit code
//        with
//            | :? UserNeedsHelp as hex -> 
//                System.Console.WriteLine("You'd like help")
//                System.Console.WriteLine("Good luck with that")
//                System.Console.WriteLine("/V for verbose output")
//                System.Console.WriteLine("/S:<url> to set the target site url")
//                System.Console.WriteLine("/C:<filename> to set the config file used. Config file is expected to have Windows line returns \r\n")
//                System.Console.WriteLine("/O:<filename> to set the output file created/overwritten")
//                System.Console.WriteLine("/N:<integer> number of links desired")
//                System.Console.WriteLine("")
//                //System.Console.WriteLine("Press Any Key to Exit")
//                //System.Console.ReadKey() |> ignore
//                0
//            | :? ExpectedResponseFail as reason ->
//                System.Console.WriteLine("Expected Response Fail")
//                System.Console.WriteLine(reason)
//                0
//            | :? System.Exception as ex ->
//                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
//                System.Console.WriteLine (ex.StackTrace)
//                if ex.InnerException = null
//                    then
//                        0
//                    else
//                        System.Console.WriteLine("---   Inner Exception   ---")
//                        System.Console.WriteLine (ex.InnerException.Message)
//                        System.Console.WriteLine (ex.InnerException.StackTrace)
//                        0

//    let rec recParseCommandLineArgs argv (options:UpdateArticleLibraryProgramConfig)=
//        match argv with
//            | [] -> options
//            | x::xs ->
//                let sX = x.ToString()
//                match Utils.getLeft sX 2 with
//                | parm when parm = "/?" || parm = "?" || parm = "-?" || parm = "-help"->
//                    raise (UserNeedsHelp("General"))
//                | "/I" ->
//                    let argParms = sX.Split([|':'|],2)
//                    // need to replace subdirectories with hypens to make work on local filesystem
//                    let newOptions = {options with inputFileName=argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/L" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with libraryFileName=argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/C" ->
//                    let argParms = sX.Split([|':'|],2)
//                    let newOptions = {options with configFileName=argParms.[1]}
//                    recParseCommandLineArgs xs newOptions
//                | "/V" ->
//                    let newOptions = {options with verbose=true}
//                    printfn "===================="
//                    printfn "VERBOSE SETTINGS ON"
//                    printfn "===================="
//                    recParseCommandLineArgs xs newOptions
//                |_ ->
//                    printfn "The option %A is unrecognized" x
//                    recParseCommandLineArgs xs options

//    let parseCommandLineArgs argv =
//        let defaultOptions = {
//            inputFileName="news.ycombinator.com.output"
//            configFileName="news.ycombinator.com.config"
//            libraryFileName="news.ycombinator.com.articlelibrary"
//            verbose=false
//            inputFileExists=false
//            configFileExists=false
//            libraryFileExists=false
//            }
//        // make sure there's a config file and output file
//        let initialReturn = recParseCommandLineArgs argv defaultOptions
//        let newConfig = 
//            if (initialReturn.configFileName.Length =0) 
//                then initialReturn.inputFileName.Replace("/","-") + ".libraryconfig"
//                else initialReturn.configFileName
//        let newInput = 
//            if (initialReturn.libraryFileName.Length=0) 
//                then initialReturn.inputFileName.Replace("/","-") + ".articlelibrary"
//                else initialReturn.inputFileName
//        let newLibrary = 
//            if (initialReturn.libraryFileName.Length=0)
//                then "dummylibrary.articlelibrary"
//                else initialReturn.libraryFileName            
//        { initialReturn with
//            libraryFileName=newLibrary
//            configFileName=newConfig
//            inputFileName=newInput
//            inputFileExists=System.IO.File.Exists(newInput)
//            configFileExists=System.IO.File.Exists(newConfig)
//            libraryFileExists=System.IO.File.Exists(newLibrary)
//        }


