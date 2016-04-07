module Main
    open Types
    open Utils
    open Persist
    open HtmlAgilityPack
    open System.Net

    let defaultBaseOptions = createNewBaseOptions "GetMetrics" "Get SysOps Metrics to Report on a web dashboard" [|"Uses a templated html file and parm substitution to create operations dashboard."|] defaultVerbosity
    let defaultInputFileName = createNewConfigEntry "I" "Input File" [|"/I:<filename> -> Where to find the template file."|] "getMetricsTemplate.html"
    let defaultOutputFileName = createNewConfigEntry "O" "Output File" [|"/O:<filename> -> Where to put the report file we are generating."|] "Metrics.html"
    let defaultInputFileExists=false

    let loadConfigFromCommandLine (args:string []):GetMetricsProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newInputFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultInputFileName, args)
        let newOutputFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFileName, args)
        let newInputFileExists=System.IO.File.Exists(newInputFileName.parameterValue)
        {
            configBase=newConfigBase
            inputFileName=newInputFileName
            outputFileName=newOutputFileName
            inputFileExists=newInputFileExists
        }


    let defaultSystemMetrics = {
                runTime=""
                //rippedStagingAreaInfo = {defaultMetricsDirectoryData with directoryName=""; fileCount="0"}
                //rippedProductionAreaInfo= {defaultMetricsDirectoryData with directoryName=""; fileCount="0"}
                getLinksInfo = Array.empty
                articleLibraryInfo = Array.empty
                selectedArticleListInfo = Array.empty
                indexFile=defaultMetricsFileData
            }
    let createFileInfoListFromFileExtension ext dir =
        if System.IO.Directory.Exists(dir) = false
            then
                Array.empty
            else
                let directoryInfo = new System.IO.DirectoryInfo(dir)
                let files = directoryInfo.GetFiles(ext)
                files |> Array.mapi(fun i x->
                    {
                        fileName = x.Name
                        sizeInLines = x.Length.ToString()
                        lastModifiedTime = x.LastAccessTime.ToString()
                    }
                    )

    let computeDirectoryInfo  (dirPath:string) (numReportingSlots:int) (reportingGroupByLength:MetricsTimeFrame) =
        let timeFrameName = 
            match reportingGroupByLength with
                | Minutes -> "minutes"
                | Hours -> "hours"
                | Days -> "days"
                | Months -> "months"
        let newFileCount, newLastWriteTime, newTimeFrameFileCount, newMostRecentThreeFiles = 
            if System.IO.Directory.Exists(dirPath)
                then
                    let dir = new System.IO.DirectoryInfo(dirPath)
                    let files = dir.GetFileSystemInfos()
                    let newNewMostRecentThreeFiles = 
                        if files.Length > 2
                            then
                                files |> Array.sortBy(fun x->x.CreationTime) |> Array.rev |> Array.toSeq |> Seq.take 3 |> Seq.toArray
                            else
                                Array.empty
                    let fileData = files |> Array.map(fun x->
                        let fileAge = (System.DateTime.Now- x.CreationTime)
                        let totalForSlot = 
                            match reportingGroupByLength with
                                | Minutes -> (int)fileAge.TotalMinutes
                                | Hours -> (int)fileAge.TotalHours
                                | Days -> (int)fileAge.TotalDays
                                | Months -> (int)(fileAge.TotalDays/30.0)

                        x.Name, totalForSlot
                        )
                    let timeSlotArray = Array.zeroCreate<int> numReportingSlots
                    fileData |> Array.iteri(fun i x->
                        let arrayIndexToIncrement = 
                            if (snd x < 1)
                                then
                                    ref 0
                                else if (snd x > numReportingSlots)
                                    then
                                        ref (numReportingSlots-1)
                                    else
                                        ref ((snd x) - 1)
                        timeSlotArray.[!arrayIndexToIncrement]<-(timeSlotArray.[!arrayIndexToIncrement]) + 1
                        )
                    let newTimeFrameFileCount = timeSlotArray |> Array.mapi(fun i x->
                        if i = numReportingSlots - 1
                            then
                                (">" + (numReportingSlots-1).ToString() + " " + timeFrameName,x.ToString())
                            else
                                (i.ToString() + "-" + (i+1).ToString() + " " + timeFrameName, x.ToString())
                        )
                    files.Length.ToString(), dir.LastWriteTime.ToString(), newTimeFrameFileCount, newNewMostRecentThreeFiles
                else
                    "0", "", Array.empty, Array.empty
        {
            directoryName=dirPath; 
            lastWriteTime=newLastWriteTime; 
            fileCount=newFileCount; 
            timeFrameFileCount = newTimeFrameFileCount
            mostRecentThreeFiles=newMostRecentThreeFiles
        }
    let computeSystemMetrics opts = 
        let dirSeparator = System.IO.Path.DirectorySeparatorChar.ToString()
        let stagingAreaInfo = computeDirectoryInfo "ripped" 24 Hours
        let productionAreaDir = ".." + dirSeparator + "ripped"
        let productionAreaInfo = computeDirectoryInfo productionAreaDir 24 Hours
        let newRunTime = System.DateTime.Now.ToString()
        let newGetLinksInfo = createFileInfoListFromFileExtension "*.getlinksoutput"  "."
        {defaultSystemMetrics with 
            runTime=newRunTime; 
            //rippedStagingAreaInfo=stagingAreaInfo 
            //rippedProductionAreaInfo=productionAreaInfo;
            getLinksInfo=newGetLinksInfo 
            }
    let createFileInfoSection (title:string) (dat:MetricsFileData[]) =
        let sb = new System.Text.StringBuilder(65535)
        sb.Append("<h3>" + title + "</h3>\r\n") |> ignore
        if dat.Length = 0 then
            sb.Append("<p>No files were returned</p>") |> ignore
        sb.Append("<table id='getMetricsGetLinksOutputFileInfo'><thead><tr><td class='getMetricsFileName'>File Name</td><td class='getMetricsLastModifiedTime'>Last Modified Time</td><td class='getMetricsSizeInLines'>File Size in bytes </td>" + "</tr></thead>\r\n") |> ignore
        dat |> Array.iteri(fun i x->
            sb.Append("<tr'><td class='getMetricsFileName'>" + x.fileName + "</td><td class='getMetricsLastModifiedTime'>" + x.lastModifiedTime + "</td><td class='getMetricsSizeInLines'>" + x.sizeInLines + " bytes </td>" + "</tr>\r\n") |> ignore
            )
        sb.Append("</table>\r\n") |> ignore
        sb.ToString()

    let createFileAgingSection (title:string) (dirPath:string) (numSlots:int) (slotSize:MetricsTimeFrame) =
        let sb = new System.Text.StringBuilder(65535)
        let agingGroupByData = computeDirectoryInfo dirPath numSlots slotSize
        sb.Append("<h3>" + title + "</h3>\r\n") |> ignore
        sb.Append("<p>Directory Path: <em>" + agingGroupByData.directoryName + "</em>. File count: " + agingGroupByData.fileCount + ". Last write time: " + agingGroupByData.lastWriteTime + "</p>") |> ignore
        if agingGroupByData.timeFrameFileCount.Length = 0 then
            sb.Append("<p>The aging information array was empty</p>") |> ignore
        if agingGroupByData.mostRecentThreeFiles.Length = 0 then
            sb.Append("<p>Less than 3 files to report on</p>") |> ignore
        agingGroupByData.mostRecentThreeFiles |> Array.iteri(fun i x ->
            sb.Append("<p>" + i.ToString() + ". "  + x.Name  + "  " + (x.CreationTime.ToString()) + "</p>") |> ignore
            )
        sb.Append("<table id='getMetricsFileAging'><thead><tr><td class='getMetricsFileAgingTimePeriod'>Time Period</td><td class='getMetricsFileAgingPeriodTotal'>Total</td>" + "</tr></thead>\r\n") |> ignore
        agingGroupByData.timeFrameFileCount |> Array.iteri(fun i x->
            sb.Append("<tr><td class='getMetricsFileAging'>" + (fst x) + "</td><td class='getMetricsFileAgingPeriodTotal'>" + (snd x) + "</td>" + "</tr>\r\n") |> ignore
            )
        sb.Append("</table>\r\n") |> ignore
        sb.ToString()
    let getIndexFileInfo = 
        let fileToBeScoped = "../index.html"
        if System.IO.File.Exists(fileToBeScoped) = false
            then
                ""
            else
                let fileInfo = new System.IO.FileInfo(fileToBeScoped)
                let lastWriteTime = fileInfo.LastWriteTime
                let numberOfListItems = 
                    try
                        let doc = new HtmlAgilityPack.HtmlDocument()
                        doc.Load(fileToBeScoped)
                        let listItems = doc.DocumentNode.SelectNodes("//li")
                        listItems.Count
                    with
                        | :? System.Exception as ex ->
                            0
                ("Last Write Time: " + lastWriteTime.ToString() + ".  Number of list items: " + numberOfListItems.ToString())
                        

    let computeNewFileContents (opts:GetMetricsProgramConfig) metrics =
        let templateFileContents = System.IO.File.ReadAllText(opts.inputFileName.parameterValue)
        let addRunTime = templateFileContents.Replace("#$#runTime#$#", metrics.runTime)
        let addIndexFileInfo = addRunTime.Replace("#$#indexFileInfo#$#", getIndexFileInfo)
        // File info -- stuff based on file extensions
        let fileInfo = System.Text.RegularExpressions.Regex.Replace(addIndexFileInfo, "\#\$\#getLinksFileInfo.*\#\$\#",(fun (mt:System.Text.RegularExpressions.Match)->
            let parmSplit = mt.Value.Split([|"|||"|], System.StringSplitOptions.None)
            if parmSplit.Length < 3
                then
                    ""
                else
                    let pattern = parmSplit.[1]
                    let title = parmSplit.[2].TrimRight 3
                    let metrics = createFileInfoListFromFileExtension ("*" + pattern)  "."
                    let sortedMetrics = metrics |> Array.sortBy(fun x->x.lastModifiedTime)
                    let stuffIt = createFileInfoSection title sortedMetrics
                    stuffIt
            ))
        // File Aging info -- summary of meta attributes of directory with large number of files
        let AgingInfo = System.Text.RegularExpressions.Regex.Replace(fileInfo, "\#\$\#getFileAgingInfo.*\#\$\#",(fun (mt:System.Text.RegularExpressions.Match)->
            let parmSplit = mt.Value.Split([|"|||"|], System.StringSplitOptions.None)
            if parmSplit.Length < 5
                then
                    ""
                else
                    let dirPath = parmSplit.[1]
                    let numSlots =
                        try
                            System.Int32.Parse(parmSplit.[2])
                        with
                            | :? System.Exception as ex ->
                                0
                    let slotSizeName = parmSplit.[3]
                    let slotSize:MetricsTimeFrame = 
                        match slotSizeName.ToLower() with
                            | "minutes" -> Minutes
                            | "hours" -> Hours
                            | "days" -> Days
                            | "months" -> Months
                            |_ -> Days
                    let title = parmSplit.[4].TrimRight 3
                    let stuffIt = createFileAgingSection title dirPath numSlots slotSize
                    stuffIt
            ))

        AgingInfo

    let doStuff (opts:GetMetricsProgramConfig) =
        let metrics = computeSystemMetrics opts
        let newFileContents = computeNewFileContents opts metrics
        System.IO.File.WriteAllText(opts.outputFileName.parameterValue, newFileContents)
        
        // Report interim progress
        match opts.configBase.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                ()
            | Verbosity.BatchNormal ->
                ()
            | Verbosity.Normal ->
                ()
            | Verbosity.BatchVerbose ->
                ()
            | Verbosity.Verbose ->
                ()
            |_ ->
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
                opts.outputFileName.printHelp
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