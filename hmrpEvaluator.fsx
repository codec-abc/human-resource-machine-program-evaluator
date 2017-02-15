open System
open System.IO
open System.Text.RegularExpressions

let iBlankLineRegex = new Regex @"^(\s)*$"
let isLabelRegex = new Regex @"^(\w)*:$"

type Label = {
    Name : string;
    Line : int 
}

type Register = {
    Index : int;
    Value : Option<int>
}

type Instruction = 
    | Inbox
    | Outbox
    | CopyTo of Register
    | CopyFrom of Register
    | JumpIfNegative of Label
    | JumpIfZero of Label
    | Increment of Register
    | Decrement of Register
    | Add of Register
    | Substract  of Register

let removeUselessLines inputLines =
    let noBlankLines = 
        inputLines |>
            Array.filter (not << iBlankLineRegex.IsMatch)

    let noCommentLines = 
        noBlankLines |>
            Array.filter (fun line -> not <| line.StartsWith "--")

    noCommentLines

let lines = File.ReadAllLines "program.hrmp"
let noCommentLines = removeUselessLines lines

for line in noCommentLines do
    printfn "%s" line

