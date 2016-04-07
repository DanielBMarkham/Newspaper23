module Main
    open Types
    open Utils
    open Persist
    open HtmlAgilityPack
    open System.Net

    let defaultBaseOptions = createNewBaseOptions "CreateArchive" "Create an archive file that references all the old front pages." [|"The template used is hard-coded."|] defaultVerbosity
    let defaultBeginDate = createNewConfigEntry "B" "Begin Date" [|"/B:<date-string> -> Date to begin the archive. Must parse to a .NET date value.";"Default is 9-11-2014"|] (System.DateTime.Parse("9-11-2014"))
    let defaultEndDate = createNewConfigEntry "E" "End Date" [|"/E:<date-string> -> Date to end the archive. Must parst to a .NET date value.";"Default is today"|] (System.DateTime.Now)
    let defaultOutputFileName = createNewConfigEntry "O" "Header File" [|"/O:<filename> -> Filename to send the archive to. Generates HTML format."; "Default is archives.html"|] "archives.html"
    let defaultOutputFileExists = false
    let defaultHeaderFileName = createNewConfigEntry "H" "Header File" [|"/H:<filename> -> File to use for header."; "Default is ArchivesHeader.html"|] "ArchivesHeader.html"
    let defaultHeaderFileExists = false
    let defaultFooterFileName = createNewConfigEntry "F" "Header File" [|"/F:<filename> -> File to use for footer."; "Default is ArchivesFooter.html"|] "ArchivesFooter.html"
    let defaultFooterFileExists = false

    let loadConfigFromCommandLine (args:string []):CreateArchiveProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newBeginDate = ConfigEntry<_>.populateValueFromCommandLine(defaultBeginDate, args)
        let newEndDate = ConfigEntry<_>.populateValueFromCommandLine(defaultEndDate, args)
        let newOutputFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFileName, args)
        let newHeaderFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultHeaderFileName, args)
        let newFooterFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultFooterFileName, args)

        let newOutputFileExists = System.IO.File.Exists(newOutputFileName.parameterValue)
        let newHeaderFileExists = System.IO.File.Exists(newHeaderFileName.parameterValue)
        let newFooterFileExists = System.IO.File.Exists(newFooterFileName.parameterValue)

        {
            configBase=newConfigBase
            beginDate=newBeginDate
            endDate=newEndDate
            outputFileName=newOutputFileName
            outputFileExists=newOutputFileExists
            inputHTMLHeaderFileName=newHeaderFileName
            inputHTMLHeaderFileExists=newHeaderFileExists
            inputHTMLFooterFileName=newFooterFileName
            inputHTMLFooterFileExists=newFooterFileExists
        }
    type TimeSpanWrapper = { timeSpan : System.TimeSpan } with
      static member (+)(d:System.DateTime, tw) = d + tw.timeSpan
      static member Zero = { timeSpan = System.TimeSpan(0L) }
    type System.DateTime with
        static member Min (x:System.DateTime) (y:System.DateTime) =
            if x < y then x else y
        static member Max (x:System.DateTime) (y:System.DateTime) =
            if x > y then x else y
        member x.NextMonthNumber =
            if x.Month = 12 then 1 else x.Month+1
        member x.LastMonthNumber =
            if x.Month=1 then 12 else x.Month-1
    let dateRange dateStart dateEnd =
        let endDate = dateEnd
        let startDate = dateStart
        let oneDay = { timeSpan = System.TimeSpan(1,0,0,0) }
        seq {
                for date in startDate .. oneDay .. endDate do
                // here you can add filtering for only certain days, like business working days, etc.
                yield date
            }

    let makeIntoALink (date:System.DateTime) (suffix:string) =
        let ret ="index_" + date.ToString("MM") + "_" + date.ToString("dd") + "_" + date.ToString("yy") + "_" + suffix + ".html"
        ret
    let makeDateTextTag (date:System.DateTime) (suffix:string) =
        let ret = date.ToString("MM") + "-" + date.ToString("dd") + " " + suffix
        ret
    let writeTableCellsForADate (date:System.DateTime) (stringBuffer:System.Text.StringBuilder) =
        stringBuffer.Append("<td>\r\n") |> ignore
        stringBuffer.Append(date.ToString("MMMM d"))  |> ignore
        stringBuffer.Append("</td>\r\n") |> ignore
        stringBuffer.Append("<td>\r\n") |> ignore
                    
        stringBuffer.Append("""<a class='' rel='noreferrer' href='""" + (makeIntoALink date "am") + """'>""") |> ignore
        stringBuffer.Append(makeDateTextTag date "AM") |> ignore
        stringBuffer.Append("</a>\r\n") |> ignore

        stringBuffer.Append("</td>\r\n") |> ignore
        stringBuffer.Append("<td>\r\n") |> ignore
        stringBuffer.Append("""<a class='' rel='noreferrer' href='""" + (makeIntoALink date "pm") + """'>""") |> ignore
        stringBuffer.Append(makeDateTextTag date "PM") |> ignore
        stringBuffer.Append("</a>\r\n") |> ignore
        stringBuffer.Append("</td>\r\n") |> ignore

    let writeDates (opts:CreateArchiveProgramConfig) (stringBuffer:System.Text.StringBuilder) =
        let daysToCover = dateRange opts.beginDate.parameterValue opts.endDate.parameterValue
        let monthsToCover = daysToCover |> Seq.groupBy(fun x->x.Year.ToString("D4") + x.Month.ToString("D2"))
        monthsToCover |> Seq.iter(fun x->
            let currentMonthNumber = System.Int32.Parse((fst x).GetRight 2)
            let currentMonth = new System.DateTime(2010,currentMonthNumber, 1)
            let currentMonthName = currentMonth.ToString("MMMM", System.Globalization.CultureInfo.InstalledUICulture)
            let currentYearNumber = System.Int32.Parse((fst x).GetLeft 4)
            let currentYearString = (fst x).GetLeft 4
            
            let firstDayOfThisPeriod = System.DateTime.Max opts.beginDate.parameterValue (new System.DateTime(currentYearNumber, currentMonthNumber, 01))
            let firstDayOfNextMonth = new System.DateTime((if currentMonthNumber = 12 then currentYearNumber+1 else currentYearNumber), firstDayOfThisPeriod.NextMonthNumber, 01)
            let lastDayOfThisMonth = firstDayOfNextMonth.Subtract(new System.TimeSpan(1,0,0,0))
            let lastDayOfThisPeriod = System.DateTime.Min opts.endDate.parameterValue lastDayOfThisMonth
            let lengthOfThisPeriod = (lastDayOfThisPeriod - firstDayOfThisPeriod).Days + 1
            let halfLengthOfThisPeriod = (float)lengthOfThisPeriod/2.0
            let halfwayThroughThisPeriod = firstDayOfThisPeriod.AddDays(((float)halfLengthOfThisPeriod - 0.5))

            stringBuffer.Append("<h3>" + currentYearString + " " + currentMonthName + "</h3>\r\n") |> ignore        
            stringBuffer.Append("<table class='archivesTable'>\r\n") |> ignore
            (snd x) |> Seq.iter(fun dayInCurrentMonth->
                if dayInCurrentMonth <= halfwayThroughThisPeriod
                then
                    stringBuffer.Append("<tr>\r\n") |> ignore
                    writeTableCellsForADate dayInCurrentMonth stringBuffer
                    let secondColumnDay = dayInCurrentMonth.AddDays(halfLengthOfThisPeriod)
                    if secondColumnDay.Month = currentMonthNumber
                        then
                            writeTableCellsForADate secondColumnDay stringBuffer
                        else
                            ()
                    stringBuffer.Append("</tr>\r\n") |> ignore
                else
                    ()
                )
            stringBuffer.Append("</table>\r\n") |> ignore
            )
        ()
    let doStuff (opts:CreateArchiveProgramConfig) =
        let outgoingFileStream = System.IO.File.CreateText(opts.outputFileName.parameterValue)
        let stringBuffer = new System.Text.StringBuilder(65535)
        writeDates opts stringBuffer
        let headerText = if opts.inputHTMLHeaderFileExists then System.IO.File.ReadAllText(opts.inputHTMLHeaderFileName.parameterValue) else ""
        let footerText = if opts.inputHTMLFooterFileExists then System.IO.File.ReadAllText(opts.inputHTMLFooterFileName.parameterValue) else ""
        outgoingFileStream.Write(headerText + stringBuffer.ToString() + footerText)
        outgoingFileStream.Flush()
        outgoingFileStream.Close()
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
                opts.beginDate.printHelp
                opts.endDate.printHelp
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
