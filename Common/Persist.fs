module Persist
    open HtmlAgilityPack
    open Types
    open Utils

    /// Fetch the contents of a web page
    let http (url:string) (userAgent:string) (referral:string) = 
        try
            let Client = makeWebClient userAgent referral
            let strm = Client.OpenRead(url)
            let sr = new System.IO.StreamReader(strm)
            let html = sr.ReadToEnd()
            let ret = htmlTextClean html
            ret
        with
            | :? System.Exception as ex ->
                System.Console.WriteLine("Exception in Utils.http trying to load " + url)
                System.Console.WriteLine(ex.Message)
                System.Console.WriteLine(ex.StackTrace)
                if ex.InnerException = null
                    then
                        ""
                    else
                        System.Console.WriteLine("Inner")
                        System.Console.WriteLine(ex.InnerException.Message)
                        ""
    let loadDoc url =
        let doc = new HtmlDocument()
        let htmlResponse = http url "Random" "https://www.google.com"
        let regScript = new System.Text.RegularExpressions.Regex("<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>")
        let hRStripScript = regScript.Replace(htmlResponse, "")
        let cleanText = htmlTextClean hRStripScript
        doc.LoadHtml cleanText
        doc
    let downloadFile userAgent referral (url:System.Uri) (fileName:string) = 
        let Client = makeWebClient userAgent referral
        Client.DownloadFile(url, fileName)
        ()
