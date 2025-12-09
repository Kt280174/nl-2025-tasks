open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text

let port = 5000
let listener = new TcpListener(IPAddress.Any, port)
listener.Start()
printfn "Server started on port %d" port

let logFile = "server_log.txt"

let rec handleClient (stream: NetworkStream) =
    let buffer = Array.zeroCreate<byte> 1024
    let read = stream.Read(buffer, 0, buffer.Length)

    if read > 0 then
        let msg = Encoding.UTF8.GetString(buffer, 0, read)
        printfn "Logged: %s" msg

        File.AppendAllText(logFile, msg + Environment.NewLine)

        let resp = Encoding.UTF8.GetBytes("OK")
        stream.Write(resp, 0, resp.Length)

        handleClient stream
    else
        printfn "Client disconnected."

while true do
    let client = listener.AcceptTcpClient()
    printfn "Client connected."
    use stream = client.GetStream()
    handleClient stream
