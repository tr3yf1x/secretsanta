// Learn more about F# at http://fsharp.org
open System
open System.Text.Json
open System.IO
open System.Text.Encodings
open System.Security.Cryptography

type participantDetails = {Name: string;}
type pair = {Sender:participantDetails; Receiver:participantDetails;}
type webpublishPair = {Pair:pair; FileName:string}
type anonymisedPair = {Who:participantDetails;Link:string}

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

let getStyleContent = 
    let path = @"C:\tmp\style.css"

    File.ReadAllLines path |> String.concat ""

let generateWebsiteContent pair = 
    let title = sprintf "Skandal!!! %s beschenkt ..." pair.Sender.Name
    let style = getStyleContent

    let body = sprintf "<div class='cracker' id='cracker'>\
        <div class='cracker-message'>\
            <div class='cracker-message__inner'>\
            %s\
            <br>\
            beschenke doch\
            <br>\
            %s\
            </div>\
        </div>\
        <div class='cracker-left'>\
            <div class='cracker-left-inner'>\
            <div class='cracker-left__mask-top'></div>\
            <div class='cracker-left__mask-bottom'></div>\
            <div class='cracker-left__tail'></div>\
            <div class='cracker-left__end'></div>\
            <div class='cracker-left__body'></div>\
            <div class='cracker-left-zigzag'>\
                <div class='cracker-left-zigzag__item'></div>\
                <div class='cracker-left-zigzag__item'></div>\
                <div class='cracker-left-zigzag__item'></div>\
                <div class='cracker-left-zigzag__item'></div>\
                <div class='cracker-left-zigzag__item'></div>\
            </div>\
            </div>\
        </div>\
        <div class='cracker-right'>\
            <div class='cracker-right-inner'>\
            <div class='cracker-right__mask-top'></div>\
            <div class='cracker-right__mask-bottom'></div>\
            <div class='cracker-right__tail'></div>\
            <div class='cracker-right__end'></div>\
            <div class='cracker-right__body'></div>\
            <div class='cracker-right-zigzag'>\
                <div class='cracker-right-zigzag__item'></div>\
                <div class='cracker-right-zigzag__item'></div>\
                <div class='cracker-right-zigzag__item'></div>\
                <div class='cracker-right-zigzag__item'></div>\
                <div class='cracker-right-zigzag__item'></div>\
            </div>\
            </div>\
        </div>\
        </div>\
        <p class='hover-me-text'>Neugierig? Cracker antippen</p>" pair.Sender.Name pair.Receiver.Name

    (sprintf "<!DOCTYPE html><html lang='de'><head><title>%s</title><style>%s</style></head><body>%s</body></html>" title style body)

let buildWebsite webpublishPair = 
    let basePath = @"C:\tmp\"
    let filePath = sprintf "%s%s.html" basePath webpublishPair.FileName

    File.WriteAllText (filePath, (generateWebsiteContent webpublishPair.Pair))

let buildLinkList pair = 
    let basePath = @"http://damnyouareawesome.com/wichteln/"
    let link = sprintf "%s%s.html" basePath pair.FileName
    {Who=pair.Pair.Sender;Link=link}

[<EntryPoint>]
let main argv =
    let participants = getParticipants participantsFile
    let pairings = getValidRandomPairings (getAllValidPairs participants ) participants |> Seq.map addFileName 

    pairings |> Seq.iter buildWebsite
    let json = pairings |>  Seq.map buildLinkList |> JsonSerializer.Serialize

    File.WriteAllText (@"C:\tmp\links.json", json) |> ignore

    0
