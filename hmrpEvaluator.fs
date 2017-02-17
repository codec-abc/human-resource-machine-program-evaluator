open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

//TODO : replace use of IsSome, IsNone with pattern matching
let LabelRegex = new Regex @"^(\w+):$"
let InstructionRegex = new Regex @"^(\s+)(\w+)(\s*)(\w*)$"

let listToString (listOfElem : 't list) =
    let nbElem = listOfElem.Length
    if nbElem = 0 then
        "[]"
    else
        let elemsAsString = List.fold (fun accum elem -> accum + elem.ToString() + ", ") "" listOfElem
        let fixedString = elemsAsString.Substring(0, elemsAsString.Length - 2)
        "[" + fixedString + "]"

let maybeToString (maybeAThing : 't option) =
    match maybeAThing with
        | None -> "None"
        | Some aValue -> string <| aValue.ToString()

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

type MachineState = 
    {
        Inputs : int list;
        Outputs : int list;
        Registers : Register list;
        HumanValue : int option;
        ProgramLines : ProgramLine list;
        CurrentInstructionLine : int;
    }
    override x.ToString() =
        let inputsAsString = listToString x.Inputs
        let outputsAsString = listToString x.Outputs
        let registersAsStringList = List.map (fun r -> sprintf "{Index : %i, Value : %s}" r.Index (maybeToString r.RegisterValue)) x.Registers
        let registersAsString = listToString registersAsStringList
        let humanValueAsString = 
            match x.HumanValue with
            | None -> "None"
            | Some aValue -> string aValue
        let result =
             "State" + "\n" + 
             "Inputs: " + inputsAsString + "\n" + 
             "Outputs: " + outputsAsString + "\n" + 
             "Registers: " + registersAsString + "\n" + 
             "Human Value: " + humanValueAsString + "\n" + 
             "Current Line: " + x.CurrentInstructionLine.ToString()
        result

type InstructionEvaluationResult =
    | End of string
    | NewState of  MachineState

let toInstruction (instructionName : string) (argument : string option) (lineNumber : int) =
    let instructionUpperCase = instructionName.ToUpper()
    try
        match instructionUpperCase with
            | "INBOX" -> InstructionLine Inbox
            | "OUTBOX" -> InstructionLine Outbox
            | "JUMPZ" -> InstructionLine <| JumpIfZero argument.Value
            | "JUMPN" -> InstructionLine <| JumpIfNegative argument.Value
            | "JUMP" -> InstructionLine <| Jump argument.Value
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
            printfn "Cannot parse line %i which instruction is %s and argument %s." lineNumber instructionName argumentToString
            printfn "Line will be interpreted as a comment."
            MeaningLessLine

let parseLine (line : string) (lineNumber : int) =
    let isLabel = LabelRegex.IsMatch line
    let isInstruction = InstructionRegex.IsMatch line
    if isLabel then
        let regexMatch = LabelRegex.Match line

        let label =  {
            Name = regexMatch.Groups.[1].Captures.[0].Value;
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
    let result = 
        {
            machineState with 
                CurrentInstructionLine = machineState.CurrentInstructionLine + 1
        }
        in result

let getLineIndexByLabelName (program : ProgramLine list) (labelToFind : string) =
    let filterFunc = fun (programLine : ProgramLine) -> 
        match programLine with
            | MeaningLessLine -> false
            | InstructionLine instruction -> false
            | LabelLine label ->
                label.Name = labelToFind
    try
        Ok <| List.findIndex filterFunc program
    with
        | _ -> Error <| sprintf "Cannot find line with the given label %s." labelToFind

let getRegisterByIndex (registers : Register list) (registerIndex : int) =
    let filterFunc = fun (register : Register) -> 
        register.Index = registerIndex
    try
        Ok <| List.find filterFunc registers
    with
        | _ -> Error <| sprintf "Cannot find register with the given index %i." registerIndex

let runInboxInstruction machineState =
    if machineState.Inputs.Length > 0 then
        let firstElemOfInput = machineState.Inputs.[0]
        let restOfInput = List.tail machineState.Inputs
        let result = 
            {
                machineState with 
                    CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                    HumanValue = Some firstElemOfInput;
                    Inputs = restOfInput;
            }
            in Ok result
    else
        Error "Cannot get an input because the input list is empty."


let runOutBoxInstruction machineState =
    if machineState.HumanValue.IsSome then
        let newOutputs = List.append machineState.Outputs [machineState.HumanValue.Value]
        let result = 
            {
                machineState with
                    CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                    HumanValue = None;
                    Outputs = newOutputs
            }
            in Ok result
    else
        Error "Cannot set output since there is no value in the human register."

let runJumpIfNegativeInstruction machineState labelToJumpTo =
    if machineState.HumanValue.IsSome then
        let shouldJump = machineState.HumanValue.Value < 0;
        let nextLineIndexOrError = 
            if shouldJump then
                getLineIndexByLabelName machineState.ProgramLines labelToJumpTo
            else
                Ok <| machineState.CurrentInstructionLine + 1
        let f = 
            fun nextLineIndex -> 
                Ok {
                    machineState with
                        CurrentInstructionLine = nextLineIndex;
                }
        Result.bind f nextLineIndexOrError
    else
        Error "Cannot test to jump since there is no value in the human register."

let runJumpIfZeroInstruction machineState labelToJumpTo =
    if machineState.HumanValue.IsSome then
        let shouldJump = machineState.HumanValue.Value = 0;
        let nextLineIndexOrError = 
            if shouldJump then
                getLineIndexByLabelName machineState.ProgramLines labelToJumpTo
            else
                Ok <| machineState.CurrentInstructionLine + 1
        let f =
            fun nextLineIndex -> 
                Ok { 
                    machineState with
                        CurrentInstructionLine = nextLineIndex;
                } 
        Result.bind f nextLineIndexOrError
    else
        Error "Cannot test to jump since there is no value in the human register."

let runJumpInstruction machineState labelToJumpTo =
    let nextLineIndexOrError = getLineIndexByLabelName machineState.ProgramLines labelToJumpTo
    let f =
        fun nextLineIndex ->
            Ok {
                machineState with
                    CurrentInstructionLine = nextLineIndex;
            }
    Result.bind f nextLineIndexOrError

let runCopyToInstruction machineState registerIndex =
    let oldRegisterOrError = getRegisterByIndex machineState.Registers registerIndex
    let f = 
        fun oldRegister ->
            let newRegister = 
                {
                    oldRegister with
                        RegisterValue = Some machineState.HumanValue.Value
                }
            
            let allRegisterExceptOne = List.filter (fun register -> register <> oldRegister) machineState.Registers
            let allRegistersUpdate = List.append allRegisterExceptOne [newRegister]
            Ok {
                machineState with 
                    CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                    Registers = allRegistersUpdate
            }
    Result.bind f oldRegisterOrError

let runCopyFromInstruction machineState registerIndex =
    let registerOrError = getRegisterByIndex machineState.Registers registerIndex
    let f =
        fun register ->
            if register.RegisterValue.IsSome then
                Ok {
                    machineState with 
                        CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                        HumanValue = Some register.RegisterValue.Value
                }
            else
                Error <| sprintf "Cannot copy from register %i because register has no value." register.Index
    Result.bind f registerOrError

let runAddInstruction machineState registerIndex =
    let registerOrError = getRegisterByIndex machineState.Registers registerIndex
    let f = 
        fun register ->
            if register.RegisterValue.IsSome then
                Ok {
                    machineState with 
                        CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                        HumanValue = Some <| register.RegisterValue.Value + machineState.HumanValue.Value
                }
            else
                Error <| sprintf "Cannot add from register %i because register has no value." register.Index
    Result.bind f registerOrError

let runSubtractInstruction machineState registerIndex =
    let registerOrError = getRegisterByIndex machineState.Registers registerIndex
    let f = 
        fun register ->
            if register.RegisterValue.IsSome then
                Ok {
                    machineState with 
                        CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                        HumanValue = Some <| machineState.HumanValue.Value - register.RegisterValue.Value
                }
            else
                Error <| sprintf "Cannot subtract from register %i because register has no value." register.Index
    Result.bind f registerOrError

let runIncrementInstruction machineState registerIndex =
    let oldRegisterOrError = getRegisterByIndex machineState.Registers registerIndex
    let f = 
        fun oldRegister ->
            if oldRegister.RegisterValue.IsSome then
                let newValue = oldRegister.RegisterValue.Value + 1
                let newRegister = 
                    {
                        oldRegister with
                            RegisterValue = Some newValue
                    }
                let allRegisterExceptOne = List.filter (fun register -> register <> oldRegister) machineState.Registers
                let allRegistersUpdate = List.append allRegisterExceptOne [newRegister]
                Ok {
                    machineState with 
                        CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                        Registers = allRegistersUpdate
                }
            else
                Error <| sprintf "Cannot increment from register %i because register has no value." oldRegister.Index
    Result.bind f oldRegisterOrError

let runDecrementInstruction machineState registerIndex =
    let oldRegisterOrError = getRegisterByIndex machineState.Registers registerIndex
    let f = 
        fun oldRegister ->
            if oldRegister.RegisterValue.IsSome then
                let newValue = oldRegister.RegisterValue.Value - 1
                let newRegister = 
                    {
                        oldRegister with
                            RegisterValue = Some newValue
                    }
                let allRegisterExceptOne = List.filter (fun register -> register <> oldRegister) machineState.Registers
                let allRegistersUpdate = List.append allRegisterExceptOne [newRegister]
                Ok  {
                    machineState with 
                        CurrentInstructionLine = machineState.CurrentInstructionLine + 1;
                        Registers = allRegistersUpdate
                }
            else
                Error <| sprintf "Cannot increment from register %i because register has no value." oldRegister.Index
    Result.bind f oldRegisterOrError

let runInstruction (machineState : MachineState) (instruction : Instruction) =
    //printfn "Running line %i with instruction %s " (machineState.CurrentInstructionLine + 1) (instruction.ToString())
    let nextStep = 
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
    match nextStep with
        | Error str -> End str
        | Ok newState -> NewState newState

let runStep (machineState : MachineState) =
    let currentLineNumber = machineState.CurrentInstructionLine;
    if currentLineNumber < machineState.ProgramLines.Length then
        let currentInstruction = machineState.ProgramLines.[currentLineNumber];
        match currentInstruction with
            | MeaningLessLine -> NewState <| skipLine machineState
            | LabelLine label -> NewState <| skipLine machineState
            | InstructionLine instruction -> runInstruction machineState instruction
    else
        End "Program has ended. There is no more line to run."

let run initialMachineState =
    let mutable keepRunning = true
    let mutable allStates = []
    let mutable currentState = initialMachineState
    let mutable programStoppedReason = ""
    while keepRunning do
        let nextStepResult = runStep currentState
        match nextStepResult with
         | End errorMsg -> 
            //printfn "%s\n" errorMsg
            programStoppedReason <- errorMsg
            keepRunning <- false
         | NewState state ->
            //printfn "%s\n" <| state.ToString()
            allStates <- List.append allStates [state]
            currentState <- state
    (allStates, programStoppedReason)

let printState state =
    let stateToString = state.ToString()
    printfn "%s\n" stateToString

let printStates states =
    for state in states do
        printState state

let stringArrayToProgramList (lines : string array) =
    let results = new List<ProgramLine>()
    for i in 0 .. (lines.Length - 1) do
        let line = lines.[i]
        let result = parseLine line i
        results.Add(result)
    let returnedValue = Seq.toList results in returnedValue

let defaultMachineState = 
    {
        Inputs = [];
        Outputs =[];
        Registers = [];
        HumanValue = None;
        ProgramLines = [];
        CurrentInstructionLine = 0;
    }

let buildEmptyRegisters nbOfRegisters =
    let mutable registers = []
    for i in 0 .. nbOfRegisters do
        let newRegister = 
            {
                Index = i;
                RegisterValue = None
            }
        registers <- List.append registers [newRegister]
    registers

let printProgramLines programLines =
    let mutable i = 1
    for result in programLines do
        printfn "Line %i %s" i (result.ToString())
        i <- i + 1

[<EntryPoint>]
let main argv = 
    let lines = File.ReadAllLines "program.hrmp"
    let programLines = stringArrayToProgramList lines
    //printProgramLines programLines
    let initialMachineState = 
        {
            defaultMachineState with
                ProgramLines = programLines
                Inputs = [1;3;4;-2]
                Registers = buildEmptyRegisters 6
        }
    
    let (states, stopReason) = run initialMachineState
    try 
        let lastState = List.last states
        printfn "last state is:"
        printState lastState
    with 
        | _ -> ()

    printfn "Program has stopped to run because:\n%s" stopReason
    //printStates states
    let returnCode = 0 in returnCode