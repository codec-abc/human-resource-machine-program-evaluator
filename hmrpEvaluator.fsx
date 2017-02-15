open System
open System.IO
open System.Text.RegularExpressions

let isBlankLineRegex = new Regex @"^(\s)*$"
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

    | JumpIfNegative of Label
    | JumpIfZero of Label
    | Jump of Label

    | CopyTo of Register
    | CopyFrom of Register

    | Increment of Register
    | Decrement of Register

    | Add of Register
    | Substract  of Register

type MachineState = {
    Input : List<int>;
    Output : List<int>;
    Registers : List<Register>;
    HumanValue : Option<int>;
    //TODO program
}

let removeUselessLines inputLines =
    let noBlankLines = 
        inputLines |>
            Array.filter (not << isBlankLineRegex.IsMatch)

    let noCommentLines = 
        noBlankLines |>
            Array.filter (fun line -> not <| line.StartsWith "--")

    noCommentLines

let parseLine (line : string) =
    Inbox

// test
let lines = File.ReadAllLines "program.hrmp"
let noCommentLines = removeUselessLines lines

for line in noCommentLines do
    printfn "%s" line

