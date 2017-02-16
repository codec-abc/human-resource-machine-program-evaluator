﻿open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let LabelRegex = new Regex @"^(\w+):$"
let InstructionRegex = new Regex @"^(\s+)(\w+)(\s*)(\w*)$"

type Register = 
    {
        Index : int;
        RegisterValue : int option
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
    Inputs : int list;
    Outputs : int list;
    Registers : Register list;
    HumanValue : int option;
    Program : ProgramLine list;
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

let skipLine machineState =
    {machineState with CurrentInstructionLine = machineState.CurrentInstructionLine + 1}

let getLineIndexByLabelName (program : ProgramLine list) (labelToFind : string) =
    let filterFunc = fun (programLine : ProgramLine) -> 
        match programLine with
            | MeaningLessLine -> false
            | InstructionLine instruction -> false
            | LabelLine label -> 
                label.Name = labelToFind
    List.findIndex filterFunc program

let getRegisterByIndex (registers : Register list) (registerIndex : int) =
    let filterFunc = fun (register : Register) -> 
        register.Index = registerIndex
    List.find filterFunc registers

let runInboxInstruction machineState =
    let firstElemOfInput = machineState.Inputs.[0]
    let restOfInput = List.tail machineState.Inputs

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            HumanValue = Some firstElemOfInput;
            Inputs = restOfInput;
    }

let runOutBoxInstruction machineState =
    let newOutputs = List.append machineState.Outputs [machineState.HumanValue.Value]

    {
        machineState with
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            HumanValue = None;
            Outputs = newOutputs
    }

let runJumpIfNegativeInstruction machineState labelToJumpTo =
    let shouldJump = machineState.HumanValue.Value < 0;
    let nextLineIndex = 
        if shouldJump then
            getLineIndexByLabelName machineState.Program labelToJumpTo
        else
            machineState.CurrentInstructionLine + 1

    {
        machineState with
            CurrentInstructionLine = nextLineIndex;
    }

let runJumpIfZeroInstruction machineState labelToJumpTo =
    let shouldJump = machineState.HumanValue.Value = 0;
    let nextLineIndex = 
        if shouldJump then
            getLineIndexByLabelName machineState.Program labelToJumpTo
        else
            machineState.CurrentInstructionLine + 1

    {
        machineState with
            CurrentInstructionLine = nextLineIndex;
    }

let runJumpInstruction machineState labelToJumpTo =
    let nextLineIndex = getLineIndexByLabelName machineState.Program labelToJumpTo

    {
        machineState with
            CurrentInstructionLine = nextLineIndex;
    }

let runCopyToInstruction machineState registerIndex =
    let oldRegister = getRegisterByIndex machineState.Registers registerIndex
    let newRegister = 
        {
            oldRegister with
                RegisterValue = Some machineState.HumanValue.Value
        }
    
    let allRegisterExceptOne = List.filter (fun register -> register = oldRegister) machineState.Registers
    let allRegistersUpdate = List.append allRegisterExceptOne [newRegister]

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            Registers = allRegistersUpdate
    }

let runCopyFromInstruction machineState registerIndex =
    let register = getRegisterByIndex machineState.Registers registerIndex

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            HumanValue = Some register.RegisterValue.Value
    }

let runAddInstruction machineState registerIndex =
    let register = getRegisterByIndex machineState.Registers registerIndex

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            HumanValue = Some <| register.RegisterValue.Value + machineState.HumanValue.Value
    }

let runSubtractInstruction machineState registerIndex =
    let register = getRegisterByIndex machineState.Registers registerIndex

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            HumanValue = Some <| machineState.HumanValue.Value - register.RegisterValue.Value
    }

let runIncrementInstruction machineState registerIndex =
    let oldRegister = getRegisterByIndex machineState.Registers registerIndex
    let newValue = oldRegister.RegisterValue.Value + 1

    let newRegister = 
        {
            oldRegister with
                RegisterValue = Some newValue
        }
    
    let allRegisterExceptOne = List.filter (fun register -> register = oldRegister) machineState.Registers
    let allRegistersUpdate = List.append allRegisterExceptOne [newRegister]

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            Registers = allRegistersUpdate
    }

let runDecrementInstruction machineState registerIndex =
    let oldRegister = getRegisterByIndex machineState.Registers registerIndex
    let newValue = oldRegister.RegisterValue.Value - 1

    let newRegister = 
        {
            oldRegister with
                RegisterValue = Some newValue
        }
    
    let allRegisterExceptOne = List.filter (fun register -> register = oldRegister) machineState.Registers
    let allRegistersUpdate = List.append allRegisterExceptOne [newRegister]

    {
        machineState with 
            CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
            Registers = allRegistersUpdate
    }

let runInstruction (machineState : MachineState) (instruction : Instruction) =
    match instruction with
        | Inbox -> runInboxInstruction machineState
        | Outbox -> runOutBoxInstruction machineState
        | JumpIfNegative labelToJumpTo -> runJumpIfNegativeInstruction machineState labelToJumpTo
        | JumpIfZero labelToJumpTo -> runJumpIfZeroInstruction machineState labelToJumpTo
        | Jump labelToJumpTo -> runJumpInstruction machineState labelToJumpTo
        | CopyTo registerIndex -> runCopyToInstruction machineState registerIndex
        | CopyFrom registerIndex -> runCopyFromInstruction machineState registerIndex
        | Add registerIndex -> runAddInstruction machineState registerIndex
        | Subtract registerIndex -> runSubtractInstruction machineState registerIndex 
        | Increment registerIndex -> runIncrementInstruction machineState registerIndex
        | Decrement registerIndex -> runDecrementInstruction machineState registerIndex

let runStep (machineState : MachineState) =
    let currentLineNumber = machineState.CurrentInstructionLine;
    let currentInstruction = machineState.Program.[currentLineNumber];
    match currentInstruction with
        | MeaningLessLine -> skipLine machineState
        | LabelLine label -> skipLine machineState
        | InstructionLine instruction -> runInstruction machineState instruction

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