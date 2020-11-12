// Learn more about F# at http://fsharp.org
open System
open System.Text.Json
open System.IO
open System.Text.Encodings
open System.Security.Cryptography

type participantDetails = {Name: string;}
type pair = {Sender:participantDetails; Receiver:participantDetails;}
type webpublishPair = {Pair:pair; FileName:string}

let participantsFile = @"C:\tmp\participants.txt"

let getAllValidPairs participantDetails = Seq.allPairs participantDetails participantDetails |> Seq.filter (fun item -> (fst item) <> (snd item)) |> Seq.map (fun item -> {Sender=(fst item);Receiver=(snd item);})

let getParticipants (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield {Name=sr.ReadLine ()}
}

let getRandomElement list = 
    let random = System.Random()
    list |> Seq.sortBy (fun _ -> random.Next()) |> Seq.take 1 |> Seq.find (fun _ -> true)

let withoutThisPair list pair = list |> Seq.filter (fun item -> (item.Sender <> pair.Sender && item.Receiver <> pair.Receiver))

let rec randomPairings list = 
    let randomPair = getRandomElement list
    let remainingPairs = withoutThisPair list randomPair

    if (remainingPairs |> Seq.length) = 0 then  [randomPair]
    else randomPair :: (randomPairings remainingPairs)

let rec getValidRandomPairings allPossiblePairs participants =
    let random = randomPairings allPossiblePairs;
    if ((random |> Seq.length) = (participants |> Seq.length)) then random
    else getValidRandomPairings allPossiblePairs participants

let generateFileName (input : string) = 
    let hashing = SHA256.Create()
    let hashed = hashing.ComputeHash (System.Text.Encoding.Default.GetBytes input) |> Seq.map (fun c -> c.ToString("X2"))
    String.concat "" hashed

let addFileName pair =
    {Pair=pair;FileName=(generateFileName pair.Sender.Name)}

let generateWebsiteContent pair = 
    let title = sprintf "Skandal!!! %A beschenkt ..." pair.Sender.Name
    let stylesheetPath = @"style.css"
    let header = sprintf "<h1>Hallo %A,</h1>" pair.Sender.Name
    let text = sprintf "Du darfst <i>%A</i> eine schöne Kleinigkeit schenken" pair.Receiver.Name
    let body = sprintf "%A%A" header text
    sprintf "<!DOCTYPE html><html lang='de'><head><title>%A</title><link rel='stylesheet' href='%A'></head><body>%A</body></html>" title stylesheetPath body

let buildWebsite webpublishPair = 
    let basePath = @"C:\tmp\"
    let filePath = sprintf "%A.%A.html" basePath webpublishPair.FileName

    File.WriteAllText (filePath, (generateWebsiteContent webpublishPair.Pair)) |> ignore


[<EntryPoint>]
let main argv =
    let participants = getParticipants participantsFile
    let pairings = getValidRandomPairings (getAllValidPairs participants ) participants |> Seq.map addFileName 
    let json = pairings |> JsonSerializer.Serialize

    File.WriteAllText (@"C:\tmp\combined.json", json) |> ignore

    pairings |> Seq.map buildWebsite



    0
