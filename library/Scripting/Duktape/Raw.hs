{-# LANGUAGE OverloadedStrings, UnicodeSyntax, CPP #-}
{-# LANGUAGE ForeignFunctionInterface, CApiFFI, EmptyDataDecls #-}

module Scripting.Duktape.Raw where

import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Control.Concurrent.MVar (newMVar, MVar)

foreign import capi "duktape.h value DUK_TYPE_NONE"      c_DUK_TYPE_NONE ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_UNDEFINED" c_DUK_TYPE_UNDEFINED ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_NULL"      c_DUK_TYPE_NULL ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_BOOLEAN"   c_DUK_TYPE_BOOLEAN ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_NUMBER"    c_DUK_TYPE_NUMBER ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_STRING"    c_DUK_TYPE_STRING ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_OBJECT"    c_DUK_TYPE_OBJECT ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_BUFFER"    c_DUK_TYPE_BUFFER ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_POINTER"   c_DUK_TYPE_POINTER ∷ CInt
foreign import capi "duktape.h value DUK_TYPE_LIGHTFUNC" c_DUK_TYPE_LIGHTFUNC ∷ CInt

-- Duktape return vals
foreign import capi "duktape.h value DUK_RET_TYPE_ERROR" c_DUK_RET_TYPE_ERROR ∷ CInt

data DuktapeHeap

type DuktapeCtx = MVar (ForeignPtr DuktapeHeap)

type DukAllocFunction = Ptr () → CSize → IO (Ptr ())
type DukReallocFunction = Ptr () → Ptr () → CSize → IO (Ptr ())
type DukFreeFunction = Ptr () → Ptr () → IO ()
type DukFatalFunction = Ptr DuktapeHeap → CInt → CString → IO ()

foreign import ccall safe "wrapper"
  c_wrapper ∷ (Ptr DuktapeHeap → IO CInt) → IO (FunPtr (Ptr DuktapeHeap → IO CInt))

-- Heap lifecycle

foreign import capi safe "duktape.h duk_create_heap"
  c_duk_create_heap ∷ FunPtr DukAllocFunction → FunPtr DukReallocFunction → FunPtr DukFreeFunction → Ptr () → FunPtr DukFatalFunction → IO (Ptr DuktapeHeap)

foreign import capi safe "duktape.h &duk_destroy_heap"
  c_duk_destroy_heap ∷ FunPtr(Ptr DuktapeHeap → IO ())

-- Evaluation

foreign import capi safe "duktape.h duk_eval_raw"
  c_duk_eval_raw ∷ Ptr DuktapeHeap → CString → CSize → CUInt → IO CInt

foreign import capi safe "duktape.h duk_peval_lstring"
  c_duk_peval_lstring ∷ Ptr DuktapeHeap → CString → CSize → IO CInt

foreign import capi safe "duktape.h duk_pcall_prop"
  c_duk_pcall_prop ∷ Ptr DuktapeHeap → CInt → CInt → IO CInt

-- Managing the stack

foreign import capi safe "duktape.h duk_pop"
  c_duk_pop ∷ Ptr DuktapeHeap → IO ()

-- Properties

foreign import capi safe "duktape.h duk_put_prop_index"
  c_duk_put_prop_index ∷ Ptr DuktapeHeap → CInt → CInt → IO CInt

foreign import capi safe "duktape.h duk_put_prop_string"
  c_duk_put_prop_string ∷ Ptr DuktapeHeap → CInt → CString → IO CInt

foreign import capi safe "duktape.h duk_get_prop_string"
  c_duk_get_prop_string ∷ Ptr DuktapeHeap → CInt → CString → IO CInt

-- Pushing to the stack

foreign import capi safe "duktape.h duk_push_string"
  c_duk_push_string ∷ Ptr DuktapeHeap → CString → IO CString

foreign import capi safe "duktape.h duk_push_lstring"
  c_duk_push_lstring ∷ Ptr DuktapeHeap → CString → CSize → IO CString

foreign import capi safe "duktape.h duk_push_number"
  c_duk_push_number ∷ Ptr DuktapeHeap → CDouble → IO ()

foreign import capi safe "duktape.h duk_push_boolean"
  c_duk_push_boolean ∷ Ptr DuktapeHeap → CInt → IO ()

foreign import capi safe "duktape.h duk_push_null"
  c_duk_push_null ∷ Ptr DuktapeHeap → IO ()

foreign import capi safe "duktape.h duk_push_array"
  c_duk_push_array ∷ Ptr DuktapeHeap → IO CInt

foreign import capi safe "duktape.h duk_push_object"
  c_duk_push_object ∷ Ptr DuktapeHeap → IO CInt

foreign import capi safe "duktape.h duk_push_c_function"
  c_duk_push_c_function ∷ Ptr DuktapeHeap → FunPtr (Ptr DuktapeHeap → IO CInt) → CInt → IO CInt

foreign import capi safe "duktape.h duk_push_global_object"
  c_duk_push_global_object ∷ Ptr DuktapeHeap → IO ()

foreign import capi safe "duktape.h duk_get_global_string"
  c_duk_get_global_string ∷ Ptr DuktapeHeap → CString → IO CInt

-- Fetching from the stack

foreign import capi safe "duktape.h duk_safe_to_lstring"
  c_duk_safe_to_lstring ∷ Ptr DuktapeHeap → CInt → Ptr CSize → IO CString

foreign import capi safe "duktape.h duk_safe_to_string"
  c_duk_safe_to_string ∷ Ptr DuktapeHeap → CInt → IO CString

foreign import capi safe "duktape.h duk_get_string"
  c_duk_get_string ∷ Ptr DuktapeHeap → CInt → IO CString

foreign import capi safe "duktape.h duk_get_lstring"
  c_duk_get_lstring ∷ Ptr DuktapeHeap → CInt → Ptr CSize → IO CString

foreign import capi safe "duktape.h duk_get_int"
  c_duk_get_int ∷ Ptr DuktapeHeap → CInt → IO CInt

foreign import capi safe "duktape.h duk_get_uint"
  c_duk_get_uint ∷ Ptr DuktapeHeap → CInt → IO CUInt

foreign import capi safe "duktape.h duk_get_number"
  c_duk_get_number ∷ Ptr DuktapeHeap → CInt → IO CDouble

foreign import capi safe "duktape.h duk_get_boolean"
  c_duk_get_boolean ∷ Ptr DuktapeHeap → CInt → IO CInt

foreign import capi safe "duktape.h duk_get_type"
  c_duk_get_type ∷ Ptr DuktapeHeap → CInt → IO CInt

-- Encoding/decoding

foreign import capi safe "duktape.h duk_json_encode"
  c_duk_json_encode ∷ Ptr DuktapeHeap → CInt → IO CString

foreign import capi safe "duktape.h duk_json_decode"
  c_duk_json_decode ∷ Ptr DuktapeHeap → CInt → IO ()

-- Debugging

foreign import capi safe "duktape.h duk_push_context_dump"
  c_duk_push_context_dump ∷ Ptr DuktapeHeap → IO ()

-------------------------------------------------------------------------------------------------------

createHeap ∷ FunPtr DukAllocFunction → FunPtr DukReallocFunction → FunPtr DukFreeFunction → Ptr () → FunPtr DukFatalFunction → IO (Maybe DuktapeCtx)
createHeap allocf reallocf freef udata fatalf = do
  ptr ← c_duk_create_heap allocf reallocf freef udata fatalf
  if ptr /= nullPtr
     then newForeignPtr c_duk_destroy_heap ptr >>= newMVar >>= return . Just
     else return Nothing

createHeapF ∷ FunPtr DukFatalFunction → IO (Maybe DuktapeCtx)
createHeapF = createHeap nullFunPtr nullFunPtr nullFunPtr nullPtr
