    open Types
    open Utils
    open Persist
    open HtmlAgilityPack

    let defaultBaseOptions = createNewBaseOptions "CheckXPath" "Load a web page and check various XPath strings against it." [|"Load web pages and test XPath against them."|] defaultVerbosity
    let defaultSiteUrl = createNewConfigEntry "U" "URL To Load (Optional)" [|"/U:<url> -> URL to attempt to load on program start."|] ""

    let loadConfigFromCommandLine (args:string []):CheckXPathProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newSiteUrl = ConfigEntry<_>.populateValueFromCommandLine(defaultSiteUrl, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            siteUrl = newSiteUrl
        }
    let doStuff (opts:CheckXPathProgramConfig) =
        let selectedUrl = // Create an URL, either from the Command Line switch or ask for one
            if opts.siteUrl.parameterValue.Length>0
                then
                    opts.siteUrl.parameterValue
                else
                    System.Console.Write("What URL to load: ")
                    let urlToOpen = System.Console.ReadLine()
                    urlToOpen
        if selectedUrl.Length=0 // If you have an URL, process, otherwise bail out
            then
                ()
            else // Check to make sure you have an http on the front
                let url = if (selectedUrl.GetLeft 7) <> """http://""" & (selectedUrl.GetLeft 8) <> """https://""" then ("""http://""" + selectedUrl) else selectedUrl
                let testDoc = loadDoc(url)
                let mutable continueProcessing=true
                // Inner loop. User keeps putting XPaths in, system returns HTML Nodes
                while continueProcessing do
                    System.Console.Write("\r\nWhat path to check: ")
                    let testPath = System.Console.ReadLine()
                    continueProcessing<-(testPath.Length>0)
                    if continueProcessing
                        then
                            let nodesList = 
                                try
                                    testDoc.DocumentNode.SelectNodes(testPath).ToArray
                                with // If XPath returns some kind of weirdness, just tell the user and carry on
                                    | :? System.Exception as ex ->
                                        printfn "EXCEPTION %s" ex.Message
                                        if ex.InnerException <> null then printfn "INNER %s" ex.InnerException.Message else ()
                                        Array.empty
                            if nodesList.Length =0 // Print out the nodes if you've got 'em
                                then
                                    printfn "No nodes returned from this query"
                                else
                                    printfn "%x Nodes selected" nodesList.Length
                                    printfn "=============================================="
                                    nodesList |> Seq.iteri(fun i x ->printfn "%u. %s \r\n\r\n" i x.OuterHtml)
                        else
                            ()

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