namespace IniTypedProvider

open System

type DefaultValue =
    | DefaultInt32 of int
    | DefaultInt64 of int64
    | DefaultUInt32 of uint32
    | DefaultUInt64 of uint64
    | DefaultInt16 of int16
    | DefaultUInt16 of uint16
    | DefaultByte of byte
    | DefaultSByte of sbyte
    | DefaultFloat of float
    | DefaultDouble of double
    | DefaultSingle of single
    | DefaultDecimal of decimal
    | DefaultBigint of bigint
    | DefaultString of string
    | DefaultBool of bool

module Defaults =
    let typeOfDefault:(DefaultValue -> Type) =
        function
        | DefaultInt32 _   -> typeof<int>
        | DefaultInt64 _   -> typeof<int64>
        | DefaultUInt32 _  -> typeof<uint32>
        | DefaultUInt64 _  -> typeof<uint64>
        | DefaultInt16 _   -> typeof<int16>
        | DefaultUInt16 _  -> typeof<uint16>
        | DefaultByte _    -> typeof<byte>
        | DefaultSByte _   -> typeof<sbyte>
        | DefaultFloat _   -> typeof<float>
        | DefaultDouble _  -> typeof<double>
        | DefaultSingle _  -> typeof<single>
        | DefaultDecimal _ -> typeof<decimal>
        | DefaultBigint _  -> typeof<bigint>
        | DefaultString _  -> typeof<string>
        | DefaultBool _    -> typeof<bool>

    let boxDefault:(DefaultValue -> objnull) =
        function
        | DefaultInt32 v   -> box v
        | DefaultInt64 v   -> box v
        | DefaultUInt32 v  -> box v
        | DefaultUInt64 v  -> box v
        | DefaultInt16 v   -> box v
        | DefaultUInt16 v  -> box v
        | DefaultByte v    -> box v
        | DefaultSByte v   -> box v
        | DefaultFloat v   -> box v
        | DefaultDouble v  -> box v
        | DefaultSingle v  -> box v
        | DefaultDecimal v -> box v
        | DefaultBigint v  -> box v
        | DefaultString v  -> box v
        | DefaultBool v    -> box v