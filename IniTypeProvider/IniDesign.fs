namespace IniTypedProvider

open System
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Parsing

type IniParseState =
    { Current  : string option
      Sections : Map<string, Map<string,string>> }

module IniDesign =

    let parseIniText (sourceName : string) (lines : string[]) =
        let folder state (i, raw) =
            match raw with
            | CommentOrEmpty -> state
            | SectionHeader name ->
                if name = "" then
                    raise (InvalidDataException(sprintf "Empty section name at line %d in %s. (line=%d, column=%d)" (i+1) sourceName (i+1) 0))
                { state with
                    Current  = Some name
                    Sections = state.Sections |> Map.add name Map.empty }
            | KeyValue(key, value) ->
                match state.Current with
                | None ->
                    raise (InvalidDataException(sprintf "Key/value before any section at line %d in %s. (line=%d, column=%d)" (i+1) sourceName (i+1) 0))
                | Some sec ->
                    if key = "" then
                        raise (InvalidDataException(sprintf "Empty key name at line %d in %s. (line=%d, column=%d)" (i+1) sourceName (i+1) 0))
                    let sectionMap = state.Sections |> Map.tryFind sec |> Option.defaultValue Map.empty
                    if sectionMap |> Map.containsKey key then
                        raise (InvalidDataException(sprintf "Duplicate key '%s' in section [%s] at line %d in %s. (line=%d, column=%d)" key sec (i+1) sourceName (i+1) 0))
                    { state with
                        Sections =
                            state.Sections
                            |> Map.add sec (sectionMap |> Map.add key value) }
            | InvalidLine text ->
                raise (InvalidDataException(sprintf "Unrecognized line %d in %s: '%s' (line=%d, column=%d)" (i+1) sourceName text (i+1) 0))

        { Current = None; Sections = Map.empty }
        |> fun init -> lines |> Array.indexed |> Array.fold folder init
        |> fun s -> s.Sections

    let parseIniFile path =
        if not (File.Exists path) then
            raise (InvalidDataException(sprintf "INI file '%s' not found. (line=%d, column=%d)" path 0 0))
        File.ReadAllLines path |> parseIniText path

    let parseDefaultsText (text : string) =
        text.Split('\n')
        |> Array.map (fun s -> s.TrimEnd())
        |> parseIniText "Defaults"