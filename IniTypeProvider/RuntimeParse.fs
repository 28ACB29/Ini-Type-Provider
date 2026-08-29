namespace IniTypedProvider

open System

// ------------------------------------------------------------
// NEW: Safe parsing helpers callable from inside quotations
// ------------------------------------------------------------
module RuntimeParse =

    let parseBool (raw:string):bool option =
        match Boolean.TryParse raw with
        | true, v -> Some v
        | _ -> None

    let parseNumeric (t:Type) (raw:string):obj option =
        Parsing.parseRuntime t raw