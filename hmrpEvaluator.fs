open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

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
    | Subtract of int

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
            | Subtract register -> "Subtract with : " + register.ToString()

type ProgramLine =
    | MeaningLessLine
    | InstructionLine of Instruction
    | LabelLine of Label
    override x.ToString() = 
        match x with
            | MeaningLessLine -> "Line is meaningless and can be skipped"
            | InstructionLine instruction -> "Line is an instruction : "  + instruction.ToString()
            | LabelLine label -> "Line is a label : " + label.ToString()

type MachineState = {
    Input : List<int>;
    Output : List<int>;
    Registers : List<Register>;
    HumanValue : Option<int>;
    Program : List<ProgramLine>;
    CurrentInstructionLine : int;
}

let toInstruction (instructionName : string) (argument : Option<string>) (lineNumber : int) =
    let instructionUpperCase = instructionName.ToUpper()
    try
        match instructionUpperCase with
            | "INBOX" -> InstructionLine Inbox
            | "OUTBOX" -> InstructionLine Outbox

            | "JUMPZ" -> InstructionLine <| JumpIfZero argument.Value
            | "JUMPN" -> InstructionLine <| JumpIfNegative argument.Value
            | "JUMP" -> InstructionLine <| JumpIfNegative argument.Value

            | "COPYTO" -> let value = int argument.Value in InstructionLine <| CopyTo value
            | "COPYFROM" -> let value = int argument.Value in InstructionLine <| CopyFrom value

            | "BUMPUP" -> let value = int argument.Value in InstructionLine <| Increment value
            | "BUMPDN" -> let value = int argument.Value in InstructionLine <| Decrement value

            | "ADD" -> let value = int argument.Value in InstructionLine <| Add value
            | "SUB" -> let value = int argument.Value in InstructionLine <| Subtract value

            | _ -> MeaningLessLine
    with 
        | _ -> 
            let argumentToString = 
                match argument with
                | None -> "None"
                | Some x -> x.ToString()
            printfn "Cannot parse line %i which instruction is %s and argument %s" lineNumber instructionName argumentToString
            printfn "Line will be interpreted as a comment"
            MeaningLessLine

let parseLine (line : string) (lineNumber : int) =
    let isLabel = LabelRegex.IsMatch line
    let isInstruction = InstructionRegex.IsMatch line
    if isLabel then
        let regexMatch = LabelRegex.Match line
        let label =  {
            Name = regexMatch.Groups.[0].Captures.[0].Value;
            Line = lineNumber;
        }
        LabelLine label
    elif isInstruction then
        let regexMatch = InstructionRegex.Match line
        let instructionName = regexMatch.Groups.[2].Captures.[0].Value
        let hasArgument = regexMatch.Groups.Count >= 4
        let argument = 
            if hasArgument then
                Some regexMatch.Groups.[4].Captures.[0].Value
            else
                None
        toInstruction instructionName argument lineNumber
    else
        MeaningLessLine

[<EntryPoint>]
let main argv = 
    let lines = File.ReadAllLines "program.hrmp"
    let results = new List<ProgramLine>()

    for i in 0 .. (lines.Length - 1) do
        let line = lines.[i]
        let result = parseLine line i
        results.Add(result)

    for result in results do
        printfn "%s" <| result.ToString()

    let returnCode = 0 in returnCode