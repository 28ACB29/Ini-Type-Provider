namespace IniTypedProvider

open IniDesign

type IniRuntimeSection = IniRuntimeSection of Map<string,string>

type IniRuntimeDocument = IniRuntimeDocument of Map<string, IniRuntimeSection> with
    member this.TryGetSection (name:string):IniRuntimeSection option =
        let (IniRuntimeDocument (sections:Map<string, IniRuntimeSection>)) = this
        Map.tryFind name sections

    member this.TryGet(section:string, key:string):string option =
        match this.TryGetSection section with
        | None -> None
        | Some (IniRuntimeSection m) -> Map.tryFind key m

module RuntimeLoad =

    let loadRuntime (path:string):IniRuntimeDocument =
        let (ini:Map<string, Map<string, string>>) = parseIniFile path
        let sections:Map<string, IniRuntimeSection> =
            ini
            |> Map.map (fun _ (keys:Map<string, string>) -> IniRuntimeSection keys)
        IniRuntimeDocument sections