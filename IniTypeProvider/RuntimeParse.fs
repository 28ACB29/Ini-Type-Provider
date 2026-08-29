namespace IniTypedProvider

open System

open IniDesign
open Parsing

type IniRuntimeSection = IniRuntimeSection of Map<string,string>

type IniRuntimeDocument = IniRuntimeDocument of Map<string, IniRuntimeSection> with
    member this.TryGetSection (name:string):IniRuntimeSection option =
        let (IniRuntimeDocument sections) = this
        Map.tryFind name sections

    member this.TryGet(section, key) =
        match this.TryGetSection section with
        | None -> None
        | Some (IniRuntimeSection m) -> Map.tryFind key m

module RuntimeLoad =
    let loadRuntime (path:string):IniRuntimeDocument =
        let ini:Map<string, Map<string, string>> = parseIniFile path
        let sections:Map<string, IniRuntimeSection> =
            ini
            |> Map.map (fun _ (keys:Map<string, string>) -> IniRuntimeSection keys)
        IniRuntimeDocument sections

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