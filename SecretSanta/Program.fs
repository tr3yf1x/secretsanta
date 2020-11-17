// Learn more about F# at http://fsharp.org
open System
open System.Text.Json
open System.IO
open System.Text.Encodings
open System.Security.Cryptography
open FSharp.Collections
open FSharp.Collections.ParallelSeq
open System.Numerics
open System.Numerics
open Microsoft.FSharp.Core.Operators

type participantDetails = {Name: string;}
type pair = {Sender:participantDetails; Receiver:participantDetails;}
type webpublishPair = {Pair:pair; FileName:string}
type anonymisedPair = {Who:participantDetails;Link:string}

let participantsFile = @"C:\tmp\participants_huge.txt"

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

let calcIndex numberOfParticpants participant offset  =
    let baseAddress = (numberOfParticpants * (participant - 1)) // Set Pointer to participant
    let baseOffset = (participant - 1) // Set Offset to participant gifts participant
    let combinedOffset = baseOffset + offset // Add run-specific Offset
    let fittingOffset = if (combinedOffset >= numberOfParticpants) then combinedOffset - numberOfParticpants else combinedOffset
    baseAddress + fittingOffset
     
let getIndex numberOfParticipants =
    let random = System.Random()
    let offset = random.Next(1,(numberOfParticipants - 1))

    printfn "Offset: %i ##########" offset
    [1 .. numberOfParticipants] |> PSeq.map (fun item -> calcIndex numberOfParticipants item offset)

let classicMode participants =
    let start = DateTime.Now
    getValidRandomPairings (getAllValidPairs participants ) participants 
    let duration = (DateTime.Now - start)
    printfn "Classic: %A for %i Elements"  duration (participants |> Seq.length)
    duration

let calcedMode participants = 
    let start = DateTime.Now
    let listOfIndexes = getIndex (participants |> PSeq.length)
    let allPairs = (participants |> Seq.allPairs participants)  |> PSeq.map (fun item -> {Sender=(fst item);Receiver=(snd item)}) 
    listOfIndexes |> PSeq.map (fun index -> (allPairs |> Seq.item index))
    let duration = (DateTime.Now - start)
    printfn "Calced: %A for %i Elements" duration   (participants |> Seq.length)
    duration

[<EntryPoint>]
let main argv =
    // let participants = getParticipants participantsFile
    // let participants = [1 .. 83166711] |> PSeq.map (fun item -> {Name=(sprintf "%A" item)})
    // let participants = [1 .. 83166711] |> PSeq.map (fun item -> {Name=(sprintf "%A" item)})
    let participants = [1 .. 13124737] |> PSeq.map (fun item -> {Name=(sprintf "%A" item)})


    let calced = calcedMode participants
    // let calced = calcedMode participants
    // let factorCalced = classic / calced

    // printfn "%A" factorCalced
    0
