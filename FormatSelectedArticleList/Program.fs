module Main
    open Types
    open Utils
    open Persist
    open System.Web.Util

    let defaultBaseOptions = createNewBaseOptions "FormatSelectedArticleList" "Take a selected article list and turn it into a file to be published." [|"Takes an article list and transforms it into various publishable formats, like html or json."|] defaultVerbosity
    let defaultInputFileName = createNewConfigEntry "I" "Input File" [|"/I:<filename> -> The article list to process."|] "bloop.articlelist"
    let defaultOutputFileName = createNewConfigEntry "O" "Output File" [|"/O:<filename> -> Output File. Where to send the link list."|] "bloop.html"
    let defaultFormatType = createNewConfigEntry "T" "Format Type" [|"/T:[html|json] -> How to format the link data."|] "HTML"
    let defaultHeaderFileName = createNewConfigEntry "H" "Header File" [|"/H:<filename> -> The header template to use."|] "header1.html"
    let defaultFooterFileName = createNewConfigEntry "F" "Footer File" [|"/F:<filename> -> The footer template to use."|] "footer1.html"
    let defaultInputFileExists= false
    let defaultOutputFileExists=false
    let defaultHeaderFileExists=false
    let defaultFooterFileExists=false

    let loadConfigFromCommandLine (args:string []) =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newInputFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultInputFileName, args)
        let newOutputFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFileName, args)
        let newFormatType =ConfigEntry<_>.populateValueFromCommandLine(defaultFormatType, args)
        let newHeaderFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultHeaderFileName, args)
        let newFooterFileName =ConfigEntry<_>.populateValueFromCommandLine(defaultFooterFileName, args)
        let newInputFileExists= System.IO.File.Exists(newInputFileName.parameterValue)
        let newOutputFileExists=System.IO.File.Exists(newOutputFileName.parameterValue)
        let newHeaderFileExists=System.IO.File.Exists(newHeaderFileName.parameterValue)
        let newFooterFileExists=System.IO.File.Exists(newFooterFileName.parameterValue)
        {
            configBase=newConfigBase
            inputFileName=newInputFileName
            outputFileName=newOutputFileName
            formatType=newFormatType
            headerFileName=newHeaderFileName
            footerFileName=newFooterFileName
            inputFileExists=newInputFileExists
            outputFileExists=newOutputFileExists
            headerFileExists=newHeaderFileExists
            footerFileExists=newFooterFileExists
        }



    let processNavHeader (articleList:ArticleList) (sb:System.Text.StringBuilder) =
        let dtNow = System.DateTime.Now
        let mastheadDate = dtNow.ToString("dddd, MMMM d, yyyy")
        sb.Append "                <div data-role=\"header\" data-position=\"fixed\" data-fullscreen=\"true\">\r\n" |> ignore
        sb.Append ("                    <h1><a href=\"#\" onclick=\"window.location='#Top';\">Newspaper23 Alpha - " + mastheadDate + "</a></h1>\r\n") |> ignore
        sb.Append "                    <a href=\"archives.html\" data-icon=\"grid\" class=\"ui-btn-left\">Archives</a>\r\n" |> ignore
        sb.Append "                    <a href=\"#\" data-icon=\"gear\" class=\"ui-btn-right\">Options</a>\r\n" |> ignore
        sb.Append "                    <div data-role=\"navbar\">\r\n" |> ignore
        sb.Append "                        <ul class=\"tabs\">\r\n" |> ignore
        articleList.Sections |> Array.iteri(fun i x->
            let id = "#tabs1-" + i.ToString()
            sb.Append ("                            <li><a href=\"" + id + "\"" + ">\r\n") |> ignore
            sb.Append ("                                " + x.SectionTitle + "\r\n") |> ignore
            sb.Append ("                            </a></li>\r\n") |> ignore
            )
        sb.Append "                        </ul>\r\n" |> ignore
        sb.Append "                    </div><!-- /navbar -->\r\n" |> ignore
        sb.Append "                </div><!-- /header -->		\r\n" |> ignore
        ()

    let createHtmlFile (opts:FormatSelectedArticleListProgramConfig) (stringBuffer:System.Text.StringBuilder) articleList =
        let headerText = System.IO.File.ReadAllText(opts.headerFileName.parameterValue)
        let footerText = System.IO.File.ReadAllText(opts.footerFileName.parameterValue)
        stringBuffer.Append headerText |> ignore
        processNavHeader articleList stringBuffer
        stringBuffer.Append "                <div class=\"container\">\r\n" |> ignore
        articleList.Sections |> Array.iteri(fun i x->
            stringBuffer.Append ("                    <div id=\"tabs1-" + i.ToString() + "\">\r\n") |> ignore
            stringBuffer.Append ("                        <h1><a name=\"" + x.SectionTitle + "\">" + x.SectionTitle + "</a></h1>\r\n") |> ignore
            x.SubSections |> Array.iteri(fun j y->
                stringBuffer.Append ("                        <h2>" + y.SubSectionTitle + "</h2>\r\n") |> ignore
                stringBuffer.Append ("                        <ul class=\"unstyled\">\r\n") |> ignore
                y.ArticleList |> Array.iteri(fun k z->
                        let articleUrl = (snd z)
                        let articleTitle = (fst z)
                        let urlToLocalFile = "http://newspaper23.com/ripped/" + (Utils.getYearMonthDirectoryString System.DateTime.Now "/") + Utils.makeUrlIntoAFilename2(articleUrl, articleTitle)
                        stringBuffer.Append ("                            <li class=\"articleListItem\"><a href=\"" + urlToLocalFile + "\">" + articleTitle + "</a>") |> ignore
                        stringBuffer.Append("""<a class="inline-google-search-icon" rel="noreferrer" href="http://www.google.com/search?q=""") |> ignore
                        //System.Console.WriteLine ("getting readee to htmldecode " + articleTitle)
                        let regTerms =
                            try
                                 System.Web.HttpUtility.HtmlDecode(articleTitle)
                            with
                                | :? System.Exception as ex ->
                                    System.Console.WriteLine("ERROR IN HTML DECODE")
                                    System.Console.WriteLine(ex.Message)
                                    System.Console.WriteLine(ex.StackTrace)
                                    System.Console.WriteLine("")
                                    System.Console.WriteLine("Inner")
                                    System.Console.WriteLine("")
                                    System.Console.WriteLine(ex.InnerException.Message)
                                    System.Console.WriteLine(ex.InnerException.StackTrace)
                                    articleTitle.Replace(" ","+")
                        stringBuffer.Append(System.Web.HttpUtility.UrlEncode(regTerms)) |> ignore
                        stringBuffer.Append("\">Search</a>") |> ignore
                        stringBuffer.Append("""<a class='inline-google-cache-icon' rel='noreferrer' href='""" + "cache:" + articleUrl + """'>""") |> ignore
                        stringBuffer.Append("""<a class='original-article-link' rel='noreferrer' href='""" + articleUrl + """'>""") |> ignore
                        stringBuffer.Append("Original Article</a>") |> ignore
                        stringBuffer.Append ("</li>\r\n") |> ignore
                    )
                stringBuffer.Append ("                        </ul> <!-- ul for article list !-->\r\n") |> ignore
                )
            stringBuffer.Append ("                    </div> <!-- div for tabs1-" + i.ToString() + " !-->\r\n") |> ignore
            )
        stringBuffer.Append "                </div> <!-- container !-->\r\n" |> ignore
        stringBuffer.Append footerText |> ignore
        stringBuffer.ToString()
    let createJsonFile (opts:FormatSelectedArticleListProgramConfig) (stringBuffer:System.Text.StringBuilder) articleList = 
        stringBuffer.Append("{\r\n") |> ignore
        articleList.Sections |> Array.iteri(fun i x->
            stringBuffer.Append("    \"SectionTitle\" : \"" + x.SectionTitle + "\",\r\n") |> ignore
            stringBuffer.Append("    \"SubSections\" : [\r\n") |> ignore
            x.SubSections |> Array.iteri(fun j y->
                stringBuffer.Append("        {\r\n") |> ignore
                stringBuffer.Append("            \"SubSectionTitle\" : \"" + y.SubSectionTitle + "\",\r\n") |> ignore
                stringBuffer.Append("            \"ArticleList\" : [ \r\n") |> ignore
                y.ArticleList |> Array.iteri(fun k z->
                        stringBuffer.Append ("                {\r\n") |> ignore
                        stringBuffer.Append ("                    \"Title\" : \"" + fst z + "\",\r\n") |> ignore
                        stringBuffer.Append ("                    \"Link\" : \"" + snd z + "\"\r\n") |> ignore
                        stringBuffer.Append ("                },\r\n") |> ignore
                    )
                stringBuffer.Remove(stringBuffer.Length-3,3).Append "\r\n" |> ignore
                stringBuffer.Append("            ]\r\n") |> ignore
                stringBuffer.Append("        },\r\n") |> ignore
                )
            stringBuffer.Remove(stringBuffer.Length-3,3).Append "\r\n" |> ignore
            stringBuffer.Append("    ],\r\n") |> ignore
            )
        stringBuffer.Remove(stringBuffer.Length-3,3).Append "\r\n" |> ignore
        stringBuffer.Append("}\r\n") |> ignore
        stringBuffer.ToString()

    let processSubSection (subSection:string) = 
        let subSectionLines = subSection.Split([|"\r\n"|], System.StringSplitOptions.None)
        let newSubSectionTitle = subSectionLines.[0].TrimBoth 1 1
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
    let processMainSections (opts:FormatSelectedArticleListProgramConfig) (sections:string[]) = 
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
    let loadArticleList (opts:FormatSelectedArticleListProgramConfig) =
        let fileText = System.IO.File.ReadAllText opts.inputFileName.parameterValue
        let initMainSections = fileText.Split([|"\r\n\r\n\r\n"|], System.StringSplitOptions.None) |> Array.map(fun x->
            if x.GetLeft 4 = "\r\n\r\n" then x.TrimLeft 4 else x)
        let mainSections = initMainSections |> Array.filter(fun x->
            x.Trim().Length <> 0
            )
        processMainSections opts mainSections

    let doStuff (opts:FormatSelectedArticleListProgramConfig) =
        let stringBuffer = new System.Text.StringBuilder(65535)
        let articleList = loadArticleList opts
        let outputFileContents = 
            match opts.formatType.parameterValue with
                | "HTML" -> createHtmlFile opts stringBuffer articleList
                | "JSON" -> createJsonFile opts stringBuffer articleList
                |_ -> createJsonFile opts stringBuffer articleList
        System.IO.File.WriteAllText(opts.outputFileName.parameterValue, outputFileContents)
        let lineCount = outputFileContents.ToString().CountOccurences "\r"
        let fileSize = outputFileContents.Length
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
                printfn "Lines written: %i" lineCount
            | Verbosity.BatchVerbose ->
                printfn "Lines written: %i" lineCount
                printfn "Total bytes written: %i" fileSize
            | Verbosity.Verbose ->
                printfn "Lines written: %i" lineCount
                printfn "Total bytes written: %i" fileSize
            |_ ->
                printfn "Lines written: %i" lineCount
                printfn "Total bytes written: %i" fileSize

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