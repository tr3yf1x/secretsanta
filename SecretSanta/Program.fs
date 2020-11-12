// Learn more about F# at http://fsharp.org
open System
open System.Text.Json
open System.IO

type participantDetails = {Name: string;}
type pair = {Sender:participantDetails; Receiver:participantDetails;}

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

[<EntryPoint>]
let main argv =
    // let random = getValidRandomPairings validPairs participantDetails

    let participants = getParticipants participantsFile
    let random = getValidRandomPairings (getAllValidPairs participants ) participants
    let json = JsonSerializer.Serialize random

    File.WriteAllText (@"C:\tmp\combined.json", json) |> ignore

    printfn "%A" random


    0
