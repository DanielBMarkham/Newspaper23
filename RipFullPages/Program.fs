    open Types
    open Utils
    open Persist
    open HtmlAgilityPack

    let defaultBaseOptions = createNewBaseOptions "Rip Full Pages" "Take the empty html files from Rip Blanks and try to download the full version of the file from the net" [|"Download html files."|] defaultVerbosity
    let defaultSourceDirectory = createNewConfigEntry "S" "Source Directory" [|"/S:<directorypath> -> Directory to find all the blank files."|] ""
    let defaultOutputDirectory = createNewConfigEntry "D" "Destination Directory" [|"/D:<directorypath> -> Directory to place all the fully downloaded files."|] ""

    let loadConfigFromCommandLine (args:string []):RipFullPagesProgramConfig =
        let newOutputDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputDirectory, args)
        let newSourceDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultSourceDirectory, args)
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            sourceDirectory = newSourceDirectory
            destinationDirectory = newOutputDirectory
        }

    let subtractArrays sourceArray arrayToSubtract f =
        let itemSplit = sourceArray |> Array.partition(fun x->
            (arrayToSubtract |> Array.exists(fun y->(f x y)))
            )
        snd itemSplit

    let doStuff (opts:RipFullPagesProgramConfig) =
        let sourceDir = new System.IO.DirectoryInfo(opts.sourceDirectory.parameterValue)
        let filesThatMightNeedProcessing = sourceDir.GetFiles() 
        printfn "%i files that might need processing" filesThatMightNeedProcessing.Length
        let targetDir = new System.IO.DirectoryInfo(opts.destinationDirectory.parameterValue)
        let filesAlreadyProcessed = targetDir.GetFiles()
        printfn "%i files already processed" filesAlreadyProcessed.Length
        let filesToProcess = subtractArrays filesThatMightNeedProcessing filesAlreadyProcessed (fun x y->x.Name=y.Name)
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
