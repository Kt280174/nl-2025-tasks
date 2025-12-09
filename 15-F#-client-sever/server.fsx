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

let handleClient (stream: NetworkStream) =
    let buffer = Array.zeroCreate<byte> 1024
    let mutable connected = true

    while connected do
        try
            let read = stream.Read(buffer, 0, buffer.Length)

            if read = 0 then
                // Client đóng kết nối đúng cách
                connected <- false
                printfn "Client disconnected."
            else
                let msg = Encoding.UTF8.GetString(buffer, 0, read)
                printfn "Logged: %s" msg

                File.AppendAllText(logFile, msg + Environment.NewLine)

                // gửi phản hồi
                let resp = Encoding.UTF8.GetBytes("OK")
                stream.Write(resp, 0, resp.Length)

        with
        | :? IOException ->
            // client đóng kết nối bất ngờ → không crash nữa
            connected <- false
            printfn "Client disconnected unexpectedly."
        | :? SocketException as ex ->
            connected <- false
            printfn "Socket error: %s" ex.Message

while true do
    let client = listener.AcceptTcpClient()
    printfn "Client connected."
    use stream = client.GetStream()
    handleClient stream
