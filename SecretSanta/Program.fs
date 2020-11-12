// Learn more about F# at http://fsharp.org
open System
open System.Text.Json
open System.Net.Mail
open System.IO

type participantDetails = {Name: string; Email:string}
type pair = {Sender:participantDetails; Receiver:participantDetails;}
type mail = {Email:string;Subject:string;Text:string}

let participantDetails = [ {Name="Georg";Email="test@testmail.com"}; {Name="Birgit";Email="test@testmail.com"}; {Name="Philipp";Email="test@testmail.com"}; {Name="Janni";Email="test@testmail.com"}; {Name="Simon";Email="test@testmail.com"}; {Name="Miri";Email="test@testmail.com"}; {Name="Theresa";Email="test@testmail.com"} ]

let validPairs = Seq.allPairs participantDetails participantDetails |> Seq.filter (fun item -> (fst item) <> (snd item)) |> Seq.map (fun item -> {Sender=(fst item);Receiver=(snd item);})

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
    let random = getValidRandomPairings validPairs participantDetails

    0
