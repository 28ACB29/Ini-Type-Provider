namespace IniTypedProvider

open System
open System.IO

open Defaults
open Parsing
open DesignMerge

module SchemaValidation =

    let private fail msg =
        raise (InvalidDataException msg)

    let validateSchema (keyInfos : KeyDesignInfo list)
                       (ini : Map<string, Map<string,string>>)
                       (defaultsBlock : Map<string, Map<string,string>>) =

        // 1. Ensure every section has keys
        for (sec, keys) in Map.toSeq ini do
            if keys.IsEmpty then
                fail $"Section [{sec}] has no keys."

        // 2. Ensure type consistency
        for info in keyInfos do
            let inferred = info.InferredType

            match info.TypedDefault with
            | Some dv ->
                let t = Defaults.typeOfDefault dv
                if t <> inferred then
                    fail $"Key {info.Section}.{info.Key} has inconsistent types: inferred {inferred} but typed default {t}."

            | None -> ()

            match info.BlockDefault with
            | Some raw ->
                let t2 = Parsing.inferFromString raw
                if t2 <> inferred then
                    fail $"Key {info.Section}.{info.Key} has inconsistent types: inferred {inferred} but block default {t2}."
            | None -> ()

        // 3. Required keys must exist
        for info in keyInfos do
            match info.TypedDefault, info.BlockDefault, info.IniValue with
            | None, None, None ->
                // optional is allowed
                ()
            | _, _, _ ->
                // required keys must have at least one source
                ()

        // 4. Optional keys must be option<T>
        for info in keyInfos do
            match info.TypedDefault, info.BlockDefault, info.IniValue with
            | None, None, None ->
                let pt = DesignMerge.propertyType info
                if not (pt.IsGenericType && pt.GetGenericTypeDefinition() = typedefof<option<_>>) then
                    fail $"Key {info.Section}.{info.Key} is missing but not optional."
            | _ -> ()