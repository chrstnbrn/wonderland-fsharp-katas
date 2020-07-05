// See the file alphabet-cipher.md for detailed information.

open System

type Message = string
type Keyword = string

type Letter = private Letter of char

module Letter =
        let create ch = 
            match ch with
            | ch when ch >= 'a' && ch <= 'z' -> Some (Letter ch) 
            | _ -> None

        let value (Letter ch) = ch
        let diff (letterA) (letterB): int = int (value letterB) - int (value letterA)
        let offset (letter) = letter |> diff (Letter 'a')
        let shift (number) (letter) = (offset letter + number + 26) % 26 + int 'a' |> char |> Letter
        let createFromOffset (i) = Letter 'a' |> shift i
        let concat (letters) = letters |> Seq.map value |> String.Concat

let (<*>) fOpt xOpt = 
    match fOpt,xOpt with
    | Some f, Some x -> Some (f x)
    | _ -> None
let retn x = Some x

let sequence listOfOptions =
    let folder x xs = retn (fun x xs -> x :: xs) <*> x <*> xs
    List.foldBack folder listOfOptions (retn [])

let toLetters (str: string) = str |> Seq.toList |> List.map Letter.create |> sequence

let rec cycle xs = seq { yield! xs; yield! cycle xs }

let validateKeyWord (key: Keyword) =
    match key with
        | null -> Error "Keyword cannot be null. "
        | "" -> Error "Keyword must contain at least one letter. "
        | _ -> match toLetters key with
                | Some x -> Ok x
                | None -> Error "Keyword contains invalid characters. "

let validateMessage (message: Message) =
    match message with
        | null -> Error "Message cannot be null. "
        | _ -> match toLetters message with
                | Some x -> Ok x
                | None -> Error "Message contains invalid characters. "

let rec findPrefixRec len (str: string) =
    let candidate = [0 .. str.Length] |> Seq.map (fun i -> str.[i % len])
    if str |> Seq.forall2 (=) candidate then
        str.Substring(0, len)
    else
        findPrefixRec (len + 1) str

let findPrefix = findPrefixRec 1

let combine resultA resultB =
    resultA |> Result.bind(fun a -> resultB |> Result.map (fun b -> (a,b)))

let encode (key: Keyword) (message: Message) : Message =
    let encodeLetter (keywordLetter, messageLetter) = messageLetter |> Letter.shift (Letter.offset keywordLetter)

    match combine (validateKeyWord key) (validateMessage message) with
        | Ok (key, message) -> Seq.zip (cycle key) message |> Seq.map encodeLetter |> Letter.concat
        | Error error -> error
 
let decode (key: Keyword) (message: Message) : Message =
    let decodeLetter (keywordLetter, messageLetter) = messageLetter |> Letter.shift (-Letter.offset keywordLetter)

    match combine (validateKeyWord key) (validateMessage message) with
        | Ok (key, message) -> Seq.zip (cycle key) message |> Seq.map decodeLetter |> Letter.concat
        | Error error -> error

let decipher (cipher: Message) (message: Message) : Keyword =
   let decipherLetter (cipherLetter, messageLetter) = cipherLetter |> Letter.diff messageLetter |> Letter.createFromOffset

   match combine (validateMessage cipher) (validateMessage message) with
       | Ok (cipher, message) when cipher.Length = message.Length -> Seq.zip cipher message |> Seq.map decipherLetter |> Letter.concat |> findPrefix
       | Ok -> "Cipher and Message must have the same length. "
       | Error error -> error

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>
    test <@ encode "scones" "" = "" @>
    test <@ encode null "meetmebythetree" = "Keyword cannot be null. " @>
    test <@ encode "" "meetmebythetree" = "Keyword must contain at least one letter. " @>
    test <@ encode "IContainInvalidCharacters" "meetmebythetree" = "Keyword contains invalid characters. " @>
    test <@ encode "scones" null = "Message cannot be null. " @>
    test <@ encode "scones" "IContainInvalidCharacters" = "Message contains invalid characters. " @>
    test <@ encode "IContainInvalidCharacters" "IContainInvalidCharacters" = "Keyword contains invalid characters. " @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>
    test <@ decode null "egsgqwtahuiljgs" = "Keyword cannot be null. " @>
    test <@ decode "" "egsgqwtahuiljgs" = "Keyword must contain at least one letter. " @>
    test <@ decode "IContainInvalidCharacters" "meetmebythetree" = "Keyword contains invalid characters. " @>
    test <@ decode "scones" null = "Message cannot be null. " @>
    test <@ decode "scones" "IContainInvalidCharacters" = "Message contains invalid characters. " @>
    test <@ decode "IContainInvalidCharacters" "IContainInvalidCharacters" = "Keyword contains invalid characters. " @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>
    test <@ decipher null "packmyboxwithfivedozenliquorjugs" = "Message cannot be null. " @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" null = "Message cannot be null. " @>
    test <@ decipher "" "packmyboxwithfivedozenliquorjugs" = "Cipher and Message must have the same length. " @>
    test <@ decipher "IContainInvalidCharacters" "packmyboxwithfivedozenliquorjugs" = "Message contains invalid characters. " @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "IContainInvalidCharacters" = "Message contains invalid characters. " @>

// run the tests
tests ()
