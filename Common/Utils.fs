module Utils
    open Types
    open System
    open System.Text.RegularExpressions
    open System.Net
    open HtmlAgilityPack

    let inline isNull x = x = Unchecked.defaultof<_>
    let htmlTextClean (str:string) =
        let trimmed = str.Trim()
        let eliminateNonPrintableCharacters = Regex.Replace(trimmed, @"\p{Cc}", " ");
        let eliminateDupeSpaces = Regex.Replace(eliminateNonPrintableCharacters, @"[ ]{2,}", " ")
        eliminateDupeSpaces
    let wordCount (str:String) =
        let trimmed = str.Trim()
        let eliminateNonPrintableCharacters = Regex.Replace(trimmed, @"\p{Cc}", " ");
        let eliminateDupeSpaces = Regex.Replace(eliminateNonPrintableCharacters, @"[ ]{2,}", " ")
        let wc = eliminateDupeSpaces.Split(' ')
        wc.Length
    // Return string between two tokens. Any error defaults to input string
    let stripSub (strInput:String) (strStartToken:String) (strEndToken:String) = 
        let start = strInput.IndexOf(strStartToken)+strStartToken.Length in
        let stop = strInput.IndexOf(strEndToken, start) in
        if ( (strStartToken = "") && (strEndToken = "") ) then strInput else strInput.Substring(start, stop-start)
//    let trimSub (strInput:String) (iTrimLeft:int) (iTrimRight:int) = 
//        try
//            strInput.TrimBoth iTrimLeft iTrimRight
//        with |_ -> strInput
    let agentArray = [|
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)";
            "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)"
        |]
    let referralArray = [|
            "";
            "";
            "";
            "";
            "";
            "";
            "";
            "";
            "";
            ""
        |]
    let makeWebClient (userAgent:string) (referral:string) = 
        let Client = new System.Net.WebClient();
        let newUserAgentHeader = match userAgent with
                                    | "Modern"->"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)"
                                    | "Ancient" ->"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)"
                                    | "CellPhone" ->"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)"
                                    | "Random" ->agentArray.randomItem
                                    |_ as agent->"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)"
        let newReferrer = match referral with
                                    | "None" -> ""
                                    | "Random" -> referralArray.randomItem
                                    |_ as ref->referral
        Client.Headers.Add(HttpRequestHeader.UserAgent, newUserAgentHeader)
        Client.Headers.Add(HttpRequestHeader.Referer, newReferrer)
        Client.Headers.Add(HttpRequestHeader.Accept, "text/html,application/xhtml+xml,application/xml")
        Client.Headers.Add(HttpRequestHeader.AcceptLanguage, "en-us,en;q=0.5")
        if Client = null
            then
                Console.WriteLine("Trying to make WebClient. Client is NULL!")
                raise (new System.ArgumentNullException("Client", "Inside Utils.makeWebClient. System.Net did not return with a valid client"))
            else 
                Client
    let toSeqTriplesFast (items:seq<_>) =
        use e = items.GetEnumerator()
        let rec loop() =
            seq {
                if e.MoveNext() then
                let a = e.Current
                if e.MoveNext() then 
                    let b = e.Current
                    if e.MoveNext() then
                    let c = e.Current
                    yield (a, b, c)
                    yield! loop()
            }
        loop()

    let makeUrlIntoAFilename (url:string) = 
        let core, suffix = 
            match (url.GetRight 4) with
                | ".pdf" -> (url.TrimRight 4), ".pdf"
                | ".htm" -> (url.TrimRight 4), ".htm"
                | "html" -> (url.TrimRight 5), ".html"
                | ".php" ->(url.TrimRight 4), ".html"
                | ".asp" ->(url.TrimRight 4), ".html"
                |_ -> url,".html"
        let tempBuff = new System.Text.StringBuilder(core)
        tempBuff.EnsureCapacity(url.Length*4) |> ignore
        urlFileNameCharacterMap |> List.iter(fun x->
            let badString = fst x
            let replaceString = snd x
            tempBuff.Replace(badString, replaceString) |> ignore
            )
        tempBuff.ToString() + suffix
    let makeUrlIntoAFilename2 (url:string, title:string) = 
        let core, suffix = 
            match (url.GetRight 4) with
                | ".pdf" -> (url.TrimRight 4), ".pdf"
                | ".htm" -> (url.TrimRight 4), ".htm"
                | "html" -> (url.TrimRight 5), ".html"
                | ".php" ->(url.TrimRight 4), ".html"
                | ".asp" ->(url.TrimRight 4), ".html"
                |_ -> url,".html"
        let sortedWordList =
            let wordList= core.Split([|' '|])
            wordList |>Array.sortBy (fun x ->
                x.Length)
        let topThreeWords = 
            match sortedWordList.Length with
                | 0-> ""
                | 1-> sortedWordList.[0]
                | 2-> sortedWordList.[0] + "_" + sortedWordList.[1]
                |_ -> sortedWordList.[0] + "_" + sortedWordList.[1] + "_" + sortedWordList.[2]
        let dateString = 
            let now = DateTime.UtcNow
            now.Year.ToString() + now.Month.ToString() + now.Day.ToString()
        let randNumString = 
            let rnd = new System.Random()
            rnd.Next(10000).ToString()
        let ret = dateString + "_" + topThreeWords + "_" + randNumString
        (ret.TrimTo 240) + suffix

    let makeFilenameIntoAnUrl (filename:string) = 
        let stripEnding = filename.Replace(".html", "")
        let tempBuff = new System.Text.StringBuilder(stripEnding)
        tempBuff.EnsureCapacity(stripEnding.Length*4) |> ignore
        urlFileNameCharacterMap |> List.iter(fun x->
            let badString = snd x
            let replaceString = fst x
            tempBuff.Replace(badString, replaceString) |> ignore
            )
        tempBuff.ToString().TrimTo 240

    let fullyQualifiedLocalName subDirectory filePrefix localName fileSuffix = 
        System.IO.Path.Combine(subDirectory,(filePrefix + localName + fileSuffix))

    let getYearMonthDirectoryString (dt:System.DateTime) (dirSeparator:string) = 
        dt.Year.ToString("####") + dirSeparator + dt.Month.ToString("##") + dirSeparator


    let loadLibraryByName sName = 
        let inputFileLines, modTime = 
            if System.IO.File.Exists(sName)
            then
                System.Console.Write ("File " + sName + " Exists")
                System.IO.File.ReadAllLines(sName), System.IO.FileInfo(sName).LastWriteTimeUtc
            else
                System.Console.Write ("File " + sName + " Does NOT Exist")
                [||], System.DateTime.UtcNow
        let inLines = inputFileLines |>Array.toSeq
        let linkTriples = inLines |> toSeqTriplesFast |> Seq.toList
        System.Console.Write (" and has " + linkTriples.Length.ToString() + " triples.")
        let linkDict = new System.Collections.Generic.Dictionary<string, titleTimeTuple>()
        let titleDict = new System.Collections.Generic.Dictionary<string, linkTimeTuple>()
        linkTriples |> List.iter(fun (a,b,c)->
            if (linkDict.ContainsKey(b)=false) && (titleDict.ContainsKey(a) = false) then
                linkDict.Add(b,{title=a;time=c})
                titleDict.Add(a,{link=b;time=c}))
        System.Console.Write (" Taking out dupes leaves " + linkDict.Count.ToString() + " triples.\n")
        {
            fileLastModified=modTime
            initialList=linkTriples
            byLink=linkDict
            byTitle=titleDict
        }

    let loadLibrary (opts:UpdateArticleLibraryProgramConfig) =
        //if opts.verbose then printfn "Opening library file %A" opts.libraryFileName
        let inputFileLines, modTime = 
            if opts.libraryFileExists
            then
                System.IO.File.ReadAllLines(opts.libraryFileName.parameterValue), System.IO.FileInfo(opts.libraryFileName.parameterValue).LastWriteTimeUtc
            else
                [||], System.DateTime.UtcNow
        let inLines = inputFileLines |>Array.toSeq
        let linkTriples = inLines |> toSeqTriplesFast |> Seq.toList
        //if opts.verbose then printfn "There are %A triples in the current library" linkTriples.Length
        let linkDict = new System.Collections.Generic.Dictionary<string, titleTimeTuple>()
        let titleDict = new System.Collections.Generic.Dictionary<string, linkTimeTuple>()
        linkTriples |> List.iter(fun (a,b,c)->
            if (linkDict.ContainsKey(b)=false) && (titleDict.ContainsKey(a) = false) then
                linkDict.Add(b,{title=a;time=c})
                titleDict.Add(a,{link=b;time=c}))
        //if opts.verbose then printfn "Filtering for dupes in the library, there are now %A library entries to match against" linkDict.Count
        {
            fileLastModified=modTime
            initialList=linkTriples
            byLink=linkDict
            byTitle=titleDict
        }
    let listLibArticlesByDOM sName nDay = (loadLibraryByName sName).initialList |> List.filter(fun (a,b,c)->System.DateTime.Parse(c).Day=nDay) |> List.iteri(fun i (a,b,c)->System.Console.WriteLine(i.ToString() + ". " + a));;
    let countLibArticlesByDOM sName nDay = (loadLibraryByName sName).initialList |> List.filter(fun (a,b,c)->System.DateTime.Parse(c).Day=nDay) |> List.length
    let listLibArticlesByDOY sName nDay = (loadLibraryByName sName).initialList |> List.filter(fun (a,b,c)->System.DateTime.Parse(c).DayOfYear=nDay) |> List.iteri(fun i (a,b,c)->System.Console.WriteLine(i.ToString() + ". " + a));;
    let countLibArticlesByDOY sName nDay = (loadLibraryByName sName).initialList |> List.filter(fun (a,b,c)->System.DateTime.Parse(c).DayOfYear=nDay) |> List.length
    let libArticleCountByDOM sName = [|1..31|] |> Array.map(fun x->(x,(countLibArticlesByDOM sName x)))  |> Array.filter(fun (a,b)-> b <> 0)
    let libArticleCountByDOY sName = [|1..366|] |> Array.map(fun x->(x,(countLibArticlesByDOY sName x)))  |> Array.filter(fun (a,b)-> b <> 0)
    let libArticleAvgByDay sName = (libArticleCountByDOM sName) |> Array.averageBy(fun (a,b)->(float)b)
    let stdDevArray arr avg =
        sqrt (Array.fold (fun acc (num,elem) -> acc + (float elem - avg) ** 2.0 ) 0.0 arr / float arr.Length)
    let libArticleStdByDay sName = stdDevArray (libArticleCountByDOM sName) (libArticleAvgByDay sName)
    let libDailyStats sName = ( (libArticleAvgByDay sName) , (libArticleStdByDay sName) )
    let lds name = libDailyStats ("C:\Users\Daniel_Main\Dropbox\source\git-repos\\newspaper23\UpdateArticleLibrary\\bin\Debug\\" + name + ".articlelibrary")

    let commandLinePrintWhileEnter (opts:ConfigBase) fnPrintMe =
                // Entering program command line report
            match opts.verbose.parameterValue with
                | Verbosity.Silent ->
                    ()
                | Verbosity.BatchMinimum ->
                    printfn "%s" opts.programName
                | Verbosity.Minimum ->
                    printfn "Begin %s. %s" opts.programName opts.programTagLine
                | Verbosity.BatchNormal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                | Verbosity.Normal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    printfn "Verbosity: Normal" 
                | Verbosity.BatchVerbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()
                | Verbosity.Verbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()
                |_ ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()

    let commandLinePrintWhileExit (baseOptions:ConfigBase) =
            // Exiting program command line report
        match baseOptions.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                printfn "End %s" baseOptions.programName
            | Verbosity.BatchNormal ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.Normal ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.BatchVerbose ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.Verbose ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            |_ ->
                ()

    let defaultVerbosity  =
        {
            commandLineParameterSymbol="V"
            commandLineParameterName="Verbosity"
            parameterHelpText=[|"/V:[0-9]           -> Amount of trace info to report. 0=none, 5=normal, 9=max."|]           
            parameterValue=Verbosity.Normal
        }
//    let createNewVerbosityParameter (commandLineArg:string) = 
//        try
//            let argParms = commandLineArg.Split([|':'|],2)
//            let parmVerbosityLevel = System.Int32.Parse("0" + argParms.[1])
//            let newVerbose = {defaultVerbosity with parameterValue=enum<Verbosity>(parmVerbosityLevel)}
//            newVerbose
//        with _->
//            let newVerbose = {defaultVerbosity with parameterValue=Verbosity.Verbose}
//            newVerbose
//    let createNewStringParamter (oldParameter:ConfigEntry<string>) (commandLineArg:string) =
//        try
//            let argParms = commandLineArg.Split([|':'|],2)
//            let newParm = {oldParameter with parameterValue=argParms.[1]}
//            newParm
//        with _->
//            {oldParameter with parameterValue=""}
//    let createNewIntParamter (oldParameter:ConfigEntry<int>) (commandLineArg:string) =
//        try
//            let argParms = commandLineArg.Split([|':'|],2)
//            let parmInt = System.Int32.Parse("0" + argParms.[1])
//            let newParm = {oldParameter with parameterValue=parmInt}
//            newParm
//        with _->
//            {oldParameter with parameterValue=0}



    let createNewBaseOptions programName programTagLine programHelpText verbose =
        {
            programName = programName
            programTagLine = programTagLine
            programHelpText=programHelpText
            verbose = verbose
            interimProgress = {items=new System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()}
        }

    let createNewConfigEntry commandlineSymbol commandlineParameterName parameterHelpText initialValue =
        {
            commandLineParameterSymbol=commandlineSymbol
            commandLineParameterName=commandlineParameterName
            parameterHelpText=parameterHelpText
            parameterValue=initialValue
        }

    let isLinuxFileSystem =
        let os = Environment.OSVersion
        let platformId = os.Platform
        match platformId with
            | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE | PlatformID.Xbox -> false
            | PlatformID.MacOSX | PlatformID.Unix -> true
            | _ ->false
    let copyToDestinationDirectory (localFileName:string) (copyTo:string) =
        if System.IO.File.Exists(localFileName) = false
            then
                ()
            else
                if not isLinuxFileSystem
                    then
                        let systemProc = new System.Diagnostics.Process()
                        systemProc.EnableRaisingEvents<-false
                        systemProc.StartInfo.FileName<-"cmd.exe"
                        systemProc.StartInfo.Arguments<-("/C copy " + localFileName + " " + copyTo)
                        systemProc.Start() |> ignore
                        systemProc.WaitForExit()                
                    else
                        let systemProc = new System.Diagnostics.Process()
                        systemProc.EnableRaisingEvents<-false
                        systemProc.StartInfo.FileName<-"/bin/cp"
                        systemProc.StartInfo.Arguments<-(" " + localFileName + " " + copyTo)
                        //System.Console.WriteLine (systemProc.StartInfo.FileName + systemProc.StartInfo.Arguments)
                        systemProc.Start() |> ignore
                        systemProc.WaitForExit()

                
