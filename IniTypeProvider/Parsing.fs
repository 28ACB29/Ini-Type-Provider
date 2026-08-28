namespace IniTypedProvider

open System

module Parsing =

    // -----------------------------
    // Line classification
    // -----------------------------
    let (|CommentOrEmpty|SectionHeader|KeyValue|InvalidLine|) (line : string) =
        let trimmed = line.Trim()
        if trimmed = "" || trimmed.StartsWith(";") then CommentOrEmpty
        elif trimmed.StartsWith("[") && trimmed.EndsWith("]") then
            SectionHeader (trimmed.Trim('[',']').Trim())
        elif trimmed.Contains("=") then
            let parts = trimmed.Split([|'='|], 2)
            KeyValue(parts.[0].Trim(), parts.[1].Trim())
        else
            InvalidLine trimmed

    // -----------------------------
    // Individual numeric patterns
    // -----------------------------
    let (|Int32Val|_|) (raw:string) =
        match Int32.TryParse raw with true, v -> Some v | _ -> None

    let (|Int64Val|_|) (raw:string) =
        match Int64.TryParse raw with true, v -> Some v | _ -> None

    let (|UInt32Val|_|) (raw:string) =
        match UInt32.TryParse raw with true, v -> Some v | _ -> None

    let (|UInt64Val|_|) (raw:string) =
        match UInt64.TryParse raw with true, v -> Some v | _ -> None

    let (|Int16Val|_|) (raw:string) =
        match Int16.TryParse raw with true, v -> Some v | _ -> None

    let (|UInt16Val|_|) (raw:string) =
        match UInt16.TryParse raw with true, v -> Some v | _ -> None

    let (|ByteVal|_|) (raw:string) =
        match Byte.TryParse raw with true, v -> Some v | _ -> None

    let (|SByteVal|_|) (raw:string) =
        match SByte.TryParse raw with true, v -> Some v | _ -> None

    let (|DoubleVal|_|) (raw:string) =
        match Double.TryParse raw with true, v -> Some v | _ -> None

    let (|SingleVal|_|) (raw:string) =
        match Single.TryParse raw with true, v -> Some v | _ -> None

    let (|DecimalVal|_|) (raw:string) =
        match Decimal.TryParse raw with true, v -> Some v | _ -> None

    let (|BigintVal|_|) (raw:string) =
        try Some (bigint.Parse raw) with _ -> None

    let (|BoolVal|_|) (raw:string) =
        match Boolean.TryParse raw with true, v -> Some v | _ -> None

    // -----------------------------
    // Type inference
    // -----------------------------
    let inferFromString (raw:string) =
        match raw with
        | Int32Val _   -> typeof<int>
        | Int64Val _   -> typeof<int64>
        | UInt32Val _  -> typeof<uint32>
        | UInt64Val _  -> typeof<uint64>
        | Int16Val _   -> typeof<int16>
        | UInt16Val _  -> typeof<uint16>
        | ByteVal _    -> typeof<byte>
        | SByteVal _   -> typeof<sbyte>
        | DoubleVal _  -> typeof<double>
        | SingleVal _  -> typeof<single>
        | DecimalVal _ -> typeof<decimal>
        | BigintVal _  -> typeof<bigint>
        | BoolVal _    -> typeof<bool>
        | _            -> typeof<string>

    // -----------------------------
    // Runtime parsing
    // -----------------------------
    let parseRuntime (t : Type) (raw : string) : obj option =
        match raw with
        | Int32Val v   when t = typeof<int>      -> Some (box v)
        | Int64Val v   when t = typeof<int64>    -> Some (box v)
        | UInt32Val v  when t = typeof<uint32>   -> Some (box v)
        | UInt64Val v  when t = typeof<uint64>   -> Some (box v)
        | Int16Val v   when t = typeof<int16>    -> Some (box v)
        | UInt16Val v  when t = typeof<uint16>   -> Some (box v)
        | ByteVal v    when t = typeof<byte>     -> Some (box v)
        | SByteVal v   when t = typeof<sbyte>    -> Some (box v)
        | DoubleVal v  when t = typeof<double>   -> Some (box v)
        | SingleVal v  when t = typeof<single>   -> Some (box v)
        | DecimalVal v when t = typeof<decimal>  -> Some (box v)
        | BigintVal v  when t = typeof<bigint>   -> Some (box v)
        | BoolVal v    when t = typeof<bool>     -> Some (box v)
        | _ -> None