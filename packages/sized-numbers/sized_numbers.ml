(** Fixed sized numbers for OCaml *)

module Unsigned = Unsigned
module Signed = Signed
module Floating = Floating

type uint8 = Unsigned.uint8
type uint16 = Unsigned.uint16
type uint32 = Unsigned.uint32
type uint64 = Unsigned.uint64
type int8 = Signed.int8
type int16 = Signed.int16
type int32 = Signed.int32
type int64 = Signed.int64
type float32 = Floating.float32
type float64 = Floating.float64

module UInt8 = Unsigned.UInt8
module UInt16 = Unsigned.UInt16
module UInt32 = Unsigned.UInt32
module UInt64 = Unsigned.UInt64
module Int8 = Signed.Int8
module Int16 = Signed.Int16
module Int32 = Signed.Int32
module Int64 = Signed.Int64
module Float32 = Floating.Float32
module Float64 = Floating.Float64
