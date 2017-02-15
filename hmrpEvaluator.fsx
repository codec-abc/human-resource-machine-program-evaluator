open System
open System.IO
open System.Text.RegularExpressions

let LabelRegex = new Regex @"^(\w+):$"
let InstructionRegex = new Regex @"^(\s+)(\w+)(\s*)(\w*)$"

type Register = 
    {
        Index : int;
        Value : Option<int>
    }
    override x.ToString() = "Register at " + string x.Index

type Label = 
    {
        Name : string;
        Line : int 
    }
    override x.ToString() = "Label named " + x.Name + " at line " + string x.Line

type Instruction = 
    | Inbox
    | Outbox

    | JumpIfNegative of string
    | JumpIfZero of string
    | Jump of string

    | CopyTo of int
    | CopyFrom of int

    | Increment of int
    | Decrement of int

    | Add of int
    | Substract of int

    override x.ToString() = 
        match x with
            | Inbox -> "Inbox"
            | Outbox -> "Outbox"

            | JumpIfNegative label -> "Jump if negative to : " + label.ToString()
            | JumpIfZero label -> "Jump if zero to : " + label.ToString()
            | Jump label -> "Jump to : " + label.ToString()

            | CopyTo register -> "Copy to : " + register.ToString()
            | CopyFrom register -> "Copy from : " + register.ToString()

            | Increment register -> "Increment : " + register.ToString()
            | Decrement register -> "Decrement : " + register.ToString()

            | Add register -> "Add with : " + register.ToString()
            | Substract register -> "Substract with : " + register.ToString()

type MachineState = {
    Input : List<int>;
    Output : List<int>;
    Registers : List<Register>;
    HumanValue : Option<int>;
    //TODO program
}

type LineParseResult =
    | NoneResult
    | InstructionResult of Instruction
    | LabelResult of Label
    override x.ToString() = 
        match x with
            | NoneResult -> "Line is not a valid line"
            | InstructionResult instruction -> "Line is an instruction : "  + instruction.ToString() //sprintf "%A" x
            | LabelResult label -> "Line is a label : " + label.ToString()
let strCompare (str1 : string) (str2 : string)  = 
    String.Compare (str1, str2, true)

let parseLine (line : string) (lineNumber : int) =
    let isLabel = LabelRegex.IsMatch line
    let isInstruction = InstructionRegex.IsMatch line
    if isLabel then
        let regexMatch = LabelRegex.Match line
        let label =  {
            Name = regexMatch.Groups.[0].Captures.[0].Value;
            Line = lineNumber;
        }
        LabelResult label
    elif isInstruction then
        let regexMatch = InstructionRegex.Match line
        let instructionName = regexMatch.Groups.[2].Captures.[0].Value
        let hasArgument = regexMatch.Groups.Count >= 4
        let argument = 
            if hasArgument then
                Some regexMatch.Groups.[4].Captures.[0].Value
            else
                None
        NoneResult //  TODO
    else
        NoneResult


let line = "    BUMPDN   a"
let lineNumber = 0

let result = parseLine line lineNumber
printfn "%s" <| result.ToString()

// test
// let lines = File.ReadAllLines "program.hrmp"
// let noCommentLines = removeUselessLines lines

// for line in noCommentLines do
//     printfn "%s" line

