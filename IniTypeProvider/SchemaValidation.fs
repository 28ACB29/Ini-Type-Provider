namespace IniTypedProvider

open System
open System.IO

open Defaults
open Parsing
open DesignMerge

module SchemaValidation =

    let private fail (msg:string) =
        raise (InvalidDataException msg)
        

    let failIfEmpty (sec: string, keys: Map<string, string>):unit =
        if keys.IsEmpty then
                fail $"Section [{sec}] has no keys."

    let checkKeyType (info: KeyDesignInfo): unit =
        let inferred:Type = info.InferredType

        match info.TypedDefault with
        | Some (dv:DefaultValue) ->
            let t:Type = Defaults.typeOfDefault dv
            if t <> inferred then
                fail $"Key {info.Section}.{info.Key} has inconsistent types: inferred {inferred} but typed default {t}."

        | None -> ()

        match info.BlockDefault with
        | Some (raw:string) ->
            let t2:Type = Parsing.inferFromString raw
            if t2 <> inferred then
                fail $"Key {info.Section}.{info.Key} has inconsistent types: inferred {inferred} but block default {t2}."
        | None -> ()

    let checkRequiredKeyExists (info: KeyDesignInfo): unit =
        match info.TypedDefault, info.BlockDefault, info.IniValue with
        | None, None, None ->
            // optional is allowed
            ()
        | (typed:DefaultValue option, block:string option, ini:string option) ->
            // required keys must have at least one source
            match typed, block, ini with
            | None, None, None ->
                fail $"Required key {info.Section}.{info.Key} has no value in INI or defaults."
            | _ -> ()

    let checkOptionalKey (info: KeyDesignInfo): unit =
        match info.TypedDefault, info.BlockDefault, info.IniValue with
            | None, None, None ->
                let pt:Type = DesignMerge.propertyType info
                if not (pt.IsGenericType && pt.GetGenericTypeDefinition() = typedefof<option<_>>) then
                    fail $"Key {info.Section}.{info.Key} is missing but not optional."
            | _ -> ()

    let validateSchema (keyInfos : KeyDesignInfo list)
                       (ini : Map<string, Map<string,string>>)
                       (defaultsBlock : Map<string, Map<string,string>>) =

        // 1. Ensure every section has keys
        ini
        |> Map.toSeq
        |> Seq.iter failIfEmpty

        // 2. Ensure type consistency
        keyInfos
        |> List.iter checkKeyType

        // 3. Required keys must exist
        keyInfos
        |> List.iter checkRequiredKeyExists

        // 4. Optional keys must be option<T>
        keyInfos
        |> List.iter checkOptionalKey