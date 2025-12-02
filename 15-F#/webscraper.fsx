open System
open System.IO
open System.Net.Http
open System.Text.RegularExpressions
open System.Threading.Tasks

// -------------------------------------
// Fake browser headers
// -------------------------------------
let httpClient = new HttpClient()
httpClient.DefaultRequestHeaders.Add(
    "User-Agent",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
)

// -------------------------------------
// Download HTML with async
// -------------------------------------
let downloadAsync (url: string) = task {
    try
        let! html = httpClient.GetStringAsync(url)
        return url, html
    with ex ->
        return url, $"ERROR: {ex.Message}"
}

// -------------------------------------
// Extract <title>
// -------------------------------------
let extractTitle (html: string) =
    let m = Regex.Match(html, "<title>(.*?)</title>", RegexOptions.IgnoreCase)
    if m.Success then m.Groups.[1].Value
    else "NO TITLE"

// -------------------------------------
// Process one URL
// -------------------------------------
let processUrl (url: string) = task {
    let! (u, html) = downloadAsync url
    if html.StartsWith("ERROR:") then
        return u, "DOWNLOAD ERROR"
    else
        return u, extractTitle html
}

// -------------------------------------
// MAIN
// -------------------------------------
let urls =
    if File.Exists("urls.txt") then
        File.ReadAllLines("urls.txt")
        |> Array.map _.Trim()
        |> Array.filter (fun s -> s <> "")
    else
        [| "https://example.com" |]

printfn "Starting async scraping...\n"

let tasks =
    urls
    |> Array.map processUrl
    |> Task.WhenAll

let results = tasks.Result

printfn "=== RESULTS ===\n"
for (u, t) in results do
    printfn "URL:   %s" u
    printfn "TITLE: %s\n" t

printfn "Done!"
