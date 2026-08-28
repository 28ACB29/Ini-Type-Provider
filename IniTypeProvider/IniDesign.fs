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
                    raise (TypeProviderError($"Empty section name at line {i+1} in {sourceName}.", i+1, 0))
                { state with
                    Current  = Some name
                    Sections = state.Sections |> Map.add name Map.empty }
            | KeyValue(key, value) ->
                match state.Current with
                | None ->
                    raise (TypeProviderError($"Key/value before any section at line {i+1} in {sourceName}.", i+1, 0))
                | Some sec ->
                    if key = "" then
                        raise (TypeProviderError($"Empty key name at line {i+1} in {sourceName}.", i+1, 0))
                    let sectionMap = state.Sections |> Map.tryFind sec |> Option.defaultValue Map.empty
                    if sectionMap |> Map.containsKey key then
                        raise (TypeProviderError($"Duplicate key '{key}' in section [{sec}] at line {i+1} in {sourceName}.", i+1, 0))
                    { state with
                        Sections =
                            state.Sections
                            |> Map.add sec (sectionMap |> Map.add key value) }
            | InvalidLine text ->
                raise (TypeProviderError($"Unrecognized line {i+1} in {sourceName}: '{text}'", i+1, 0))

        { Current = None; Sections = Map.empty }
        |> fun init -> lines |> Array.indexed |> Array.fold folder init
        |> fun s -> s.Sections

    let parseIniFile path =
        if not (File.Exists path) then
            raise (TypeProviderError($"INI file '{path}' not found.", 0, 0))
        File.ReadAllLines path |> parseIniText path

    let parseDefaultsText (text : string) =
        text.Split('\n')
        |> Array.map (fun s -> s.TrimEnd())
        |> parseIniText "Defaults"