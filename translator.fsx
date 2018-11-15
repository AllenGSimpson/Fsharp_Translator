open System
#I __SOURCE_DIRECTORY__
#r @".\packages\Newtonsoft.Json\lib\net45\Newtonsoft.Json.dll"
#r @".\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsecCS.dll"
#r @".\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsec.dll"

open FParsec
let fromJson (tr:System.IO.TextReader) : 'T =
    let js = Newtonsoft.Json.JsonSerializer()
    js.Deserialize(tr,typeof<'T>) |> unbox<'T>

let toJson (tw:System.IO.TextWriter) (o:obj) =
    let js = Newtonsoft.Json.JsonSerializer()
    js.Serialize(tw,o)
let consonants = "kzlthrřdgxfvƙƭƌƍʫɬƚʑɠ" |> Seq.toArray  //ƙ=kr ƭ= tr ƌ= dr ƍ= gr ʫ = lz ɬ= kl ƚ= tl ʑ =zl ɠ= gl
let vowels = "aouû" |> Seq.toArray
let stops = "'-" |> Seq.toArray

// let C = "kzlthrřdgxfvƙƭƌƍʫɬƚʑɠ".Length
// let V = "aouû".Length
// let S = "'-".Length
// C*V*(C*V)*C*(S*C*V*(C*V)*C)+C*V*(C*V)*C*(S*C*V*C)+C*V*(C*V)*C+C*V*C*(S*C*V*(C*V)*C)+C*V*C*(S*C*V*C)+C*V*C

//let rand = System.Random ()


let getRandNumber =
    let rand = System.Random ()
    fun max -> rand.Next (max)
    
let pConsonant = 
    pstring "C" |>> fun _ -> consonants.[getRandNumber consonants.Length] |> string

let pVowel = 
    pstring "V" |>> fun _ -> vowels.[getRandNumber vowels.Length] |> string

let pStop = 
    pstring "S" |>> fun _ -> stops.[getRandNumber stops.Length] |> string


let mutable pWord'= (pstring "" |>> fun x -> printf "Hello!"; x)

let rec pOptional = 
    pstring "(" >>= (fun _ -> pWord' .>> pstring ")")
    |>> fun value -> if getRandNumber 2 = 0 then "" else value

and pWord =
    many (pConsonant <|> pVowel <|> pStop <|> pOptional)
    |>> (String.concat "")

pWord' <- pWord

let getRawWord wordDesign = 
    match run pWord wordDesign with 
    | Success (result,_,_) -> Some result
    | Failure (errMessage,_,_) -> printfn "Error: %s" errMessage; None


// let createWordKazar =
//     getRawWord "CV(CV)C(SCV(CV)C)" 
//     |> Seq.iteri ( fun x i ->
//         match i with
//         | i when i < 1 ->
//         | i when i > word.Length-1 ->
//         | _ -> //apply word transform rules
//     )
// let createNewWordKazar =     //work in progress
//     let dictionary : System.Collections.Generic.Dictionary<string,int> = use sr = new System.IO.StreamReader(@".\dict.json") in fromJson sr 
//     let rec loop = 
//         match createWordKazar with 
//         | None -> printf "ERROR: Bad Raw Word Structure!"; "ERROR"
//         | w when dictionary.ContainsValue w -> loop
//         | w -> w.Value
//     loop

let dictionaryKazar : System.Collections.Generic.Dictionary<string,string> = use sr = new System.IO.StreamReader(@".\dict.json") in fromJson sr

let rec getNewWord = 
    (getRawWord "CV(CV)C(SCV(CV)C)" ).Value
    |>  (fun x -> match dictionaryKazar.ContainsValue x, x with 
        | true,_ -> getNewWord
        | false,x -> x )


dictionaryKazar.Add("apple",getNewWord)

use sw = new System.IO.StreamWriter(@".\dict.json") in toJson sw dictionaryKazar