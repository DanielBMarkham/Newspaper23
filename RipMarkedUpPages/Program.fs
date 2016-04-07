    open Types
    open Utils
    open Persist
    open HtmlAgilityPack

    let defaultBaseOptions = createNewBaseOptions "Rip MarkedUp Pages" "Take full pages that have been downloaded and score them" [|"Pages are downloaded in full by Rip Full Pages Program."|] defaultVerbosity
    let defaultSourceDirectory = createNewConfigEntry "S" "Source Directory" [|"/S:<directorypath> -> Directory to find all the downloaded pages."|] ""
    let defaultOutputDirectory = createNewConfigEntry "D" "Destination Directory" [|"/D:<directorypath> -> Directory to place all the scored files."|] ""

    let loadConfigFromCommandLine (args:string []):RipMarkedUpPagesProgramConfig =
        let newOutputDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputDirectory, args)
        let newSourceDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultSourceDirectory, args)
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            sourceDirectory = newSourceDirectory
            destinationDirectory = newOutputDirectory
        }
    let doStuff (opts:RipMarkedUpPagesProgramConfig) =
        let sourceDir = new System.IO.DirectoryInfo(opts.sourceDirectory.parameterValue)
        let filesThatMightNeedProcessing = sourceDir.GetFiles()
        let targetDir = new System.IO.DirectoryInfo(opts.destinationDirectory.parameterValue)
        let filesAlreadyProcessed = targetDir.GetFiles()
        let filesToProcess = filesThatMightNeedProcessing |> Array.filter(fun x->
            (filesAlreadyProcessed |> Array.exists(fun y->x.Name=y.Name)
            )
        )
        // CODE GOES HERE
        printfn "%i files to process" filesToProcess.Length
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
                System.Console.WriteLine(" Inner ")
                System.Console.WriteLine ("Program terminated abnormally " + ex.InnerException.Message)
                0
