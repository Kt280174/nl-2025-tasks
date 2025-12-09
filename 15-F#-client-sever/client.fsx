open System
open System.IO
open System.Net.Sockets
open System.Text

let inputFile = "client_input.txt"

if not (File.Exists inputFile) then
    printfn "ERROR: %s not found!" inputFile
    Environment.Exit(1)

let lines = File.ReadAllLines(inputFile)

let client = new TcpClient()
client.Connect("127.0.0.1", 5000)
printfn "Connected to server!"

use stream = client.GetStream()
let buffer = Array.zeroCreate<byte> 1024

for line in lines do
    // gửi thêm ký tự xuống dòng để server biết kết thúc message
    let bytes = Encoding.UTF8.GetBytes(line + "\n")
    stream.Write(bytes, 0, bytes.Length)
    printfn "Sent: %s" line

    // đọc phản hồi từ server
    let read = stream.Read(buffer, 0, buffer.Length)
    let resp = Encoding.UTF8.GetString(buffer, 0, read)
    printfn "Server: %s" resp

printfn "Client finished sending all messages."
client.Close()
