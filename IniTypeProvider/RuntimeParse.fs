namespace IniTypedProvider

open IniDesign
open Parsing

type IniRuntimeSection = IniRuntimeSection of Map<string,string>

type IniRuntimeDocument = IniRuntimeDocument of Map<string, IniRuntimeSection> with
    member this.TryGetSection name =
        let (IniRuntimeDocument sections) = this
        Map.tryFind name sections

    member this.TryGet(section, key) =
        match this.TryGetSection section with
        | None -> None
        | Some (IniRuntimeSection m) -> Map.tryFind key m

module RuntimeLoad =
    let loadRuntime path : IniRuntimeDocument =
        let ini = parseIniFile path
        let sections =
            ini |> Map.map (fun _ keys -> IniRuntimeSection keys)
        IniRuntimeDocument sections

// ------------------------------------------------------------
// NEW: Safe parsing helpers callable from inside quotations
// ------------------------------------------------------------
module RuntimeParse =

    let parseBool (raw : string) =
        match System.Boolean.TryParse raw with
        | true, v -> Some v
        | _ -> None

    let parseNumeric (t : System.Type) (raw : string) =
        Parsing.parseRuntime t raw