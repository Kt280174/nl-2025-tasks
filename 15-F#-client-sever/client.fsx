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
    let bytes = Encoding.UTF8.GetBytes(line)
    stream.Write(bytes, 0, bytes.Length)
    printfn "Sent: %s" line

    let read = stream.Read(buffer, 0, buffer.Length)
    let resp = Encoding.UTF8.GetString(buffer, 0, read)
    printfn "Server: %s" resp
