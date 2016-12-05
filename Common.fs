
[<AutoOpen>]
module Common

let splitLines (s:string) = s.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)

let split (c:char) (s:string) = s.Split([|c|], System.StringSplitOptions.RemoveEmptyEntries)