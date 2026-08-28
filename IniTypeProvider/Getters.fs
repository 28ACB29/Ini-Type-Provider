namespace IniTypedProvider

open Microsoft.FSharp.Quotations
open Defaults
open DesignMerge
open Parsing
open RuntimeParse

module Getters =

    let getterExpr (info : KeyDesignInfo) (docExpr : Expr) =
        let sec, key = info.Section, info.Key
        let t = info.InferredType
        let propType = propertyType info
        let isOption =
            propType.IsGenericType &&
            propType.GetGenericTypeDefinition() = typedefof<option<_>>

        let defaultObjOpt =
            info.TypedDefault |> Option.map boxDefault

        let blockDefaultObjOpt =
            info.BlockDefault
            |> Option.bind (fun raw ->
                if t = typeof<string> then Some (box raw)
                elif t = typeof<bool> then parseBool raw |> Option.map box
                else parseNumeric t raw)

        if not isOption then
            <@@
                let doc = %%docExpr : IniRuntimeDocument
                match doc.TryGet(sec, key) with
                | Some raw ->
                    if t = typeof<string> then box raw
                    elif t = typeof<bool> then
                        match parseBool raw with
                        | Some v -> box v
                        | None ->
                            match defaultObjOpt, blockDefaultObjOpt with
                            | Some d, _ -> d
                            | None, Some d -> d
                            | None, None -> failwithf "Invalid bool for %s.%s" sec key
                    else
                        match parseNumeric t raw with
                        | Some v -> v
                        | None ->
                            match defaultObjOpt, blockDefaultObjOpt with
                            | Some d, _ -> d
                            | None, Some d -> d
                            | None, None -> failwithf "Invalid numeric for %s.%s" sec key
                | None ->
                    match defaultObjOpt, blockDefaultObjOpt with
                    | Some d, _ -> d
                    | None, Some d -> d
                    | None, None -> failwithf "Missing required key %s.%s" sec key
            @@>
        else
            <@@
                let doc = %%docExpr : IniRuntimeDocument
                match doc.TryGet(sec, key) with
                | Some raw ->
                    if t = typeof<string> then Some (box raw)
                    elif t = typeof<bool> then parseBool raw |> Option.map box
                    else parseNumeric t raw
                | None -> None
            @@>