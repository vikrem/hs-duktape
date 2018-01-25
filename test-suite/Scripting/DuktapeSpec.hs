{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Scripting.DuktapeSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Data.Maybe
import           Data.Aeson hiding (json)
import           Scripting.Duktape

spec ∷ Spec
spec = do
  describe "evalDuktape" $ do
    it "evaluates ECMAScript expressions" $ do
      ctx ← createDuktapeCtx
      r0 ← evalDuktape (fromJust ctx) ""
      r0 `shouldBe` Right Nothing
      rE ← evalDuktape (fromJust ctx) "print('hello"
      rE `shouldBe` Left "SyntaxError: unterminated string (line 1)"
      rT ← evalDuktape (fromJust ctx) "1 == 1"
      rT `shouldBe` (Right $ Just $ Bool True)
      rF ← evalDuktape (fromJust ctx) "1 == 2"
      rF `shouldBe` (Right $ Just $ Bool False)
      rN ← evalDuktape (fromJust ctx) "12345"
      rN `shouldBe` (Right $ Just $ Number 12345)
      rS ← evalDuktape (fromJust ctx) "'string'"
      rS `shouldBe` (Right $ Just $ String "string")
      rA ← evalDuktape (fromJust ctx) "[1,2,'string',{what:'ever'}]"
      rA `shouldBe` (Right $ Just [json|[1,2,"string",{"what":"ever"}]|])
      rO ← evalDuktape (fromJust ctx) "({what: ['ever', 1, 2, 3]})"
      rO `shouldBe` (Right $ Just [json|{"what":["ever", 1, 2, 3]}|])

  describe "callDuktape" $ do
    it "calls ECMAScript functions" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
function returnsObj () { return {hello: {from: [1, 2, 3]}} }
function throwsErr () { throw new Error('hi') }
|]
      rE ← callDuktape (fromJust ctx) [] "n0thing!" []
      rE `shouldBe` (Left "TypeError: undefined not callable (property 'n0thing!' of [object global])")
      rO ← callDuktape (fromJust ctx) [] "returnsObj" []
      rO `shouldBe` (Right $ Just [json|{"hello": {"from": [1, 2, 3]}}|])
      rT ← callDuktape (fromJust ctx) [] "throwsErr" []
      rT `shouldBe` (Left "Error: hi")

    it "calls ECMAScript functions on objects" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
var Stuff = { x: 'testFromThis' }
Stuff.fun = function () { return this.x }
|]
      rS ← callDuktape (fromJust ctx) ["Stuff"] "fun" []
      rS `shouldBe` (Right $ Just $ String "testFromThis")
      rE ← callDuktape (fromJust ctx) ["failNonExistent"] "fun" []
      rE `shouldBe` (Left "Nonexistent property of global object: [\"failNonExistent\"]")

    it "calls ECMAScript functions with args" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
function double (x, y) { return x*2 + y*2 }
function awesomeString (x) { return '-= ' + x + ' =-' }
function boolTest (x, y, z) { return typeof(x) === 'boolean' && x && (y && !z) }
function arrTest (a) { return a.map(function (el) { return typeof(el) }).join('') }
function objTest (obj) { return obj.name + obj.stuff.filter(function (x) { return x != null && (typeof(x) == 'string' || typeof(x) == 'object') })
                                                    .map(function(x) { return typeof(x) == 'object' ? x.value : x } ).join('') }
|]
      rN ← callDuktape (fromJust ctx) [] "double" [Number 7, Number 8]
      rN `shouldBe` (Right $ Just $ Number 30)
      rS ← callDuktape (fromJust ctx) [] "awesomeString" [String "hello"]
      rS `shouldBe` (Right $ Just $ String "-= hello =-")
      rB ← callDuktape (fromJust ctx) [] "boolTest" [Bool True, Bool True, Bool False]
      rB `shouldBe` (Right $ Just $ Bool True)
      rA ← callDuktape (fromJust ctx) [] "arrTest" [[json|[1, 2, "test", null]|]]
      rA `shouldBe` (Right $ Just $ String "numbernumberstringobject")
      rO ← callDuktape (fromJust ctx) [] "objTest" [[json|{"stuff": [1, 2, "hello", null, {"value": "world"}], "name": "test"}|]]
      rO `shouldBe` (Right $ Just $ String "testhelloworld")

  describe "exposeFnDuktape" $ do
    it "exposes Haskell functions to ECMAScript" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) "var X = {}"
      let dbl x = return $ Number $ x * 2 ∷ IO Value
          add x y = return $ x + y ∷ IO Integer
          cnst = return $ 123 ∷ IO Integer
          noop = return () ∷ IO ()
      _ ← exposeFnDuktape (fromJust ctx) [] "double" dbl
      _ ← exposeFnDuktape (fromJust ctx) [] "noop" noop
      _ ← exposeFnDuktape (fromJust ctx) ["X"] "cnst" cnst
      _ ← exposeFnDuktape (fromJust ctx) ["X"] "add" add
      rd ← evalDuktape (fromJust ctx) "double(7) + X.cnst() + X.add(3,5)"
      rd `shouldBe` (Right $ Just $ Number 145)
      rD ← callDuktape (fromJust ctx) [] "double" [Number 7]
      rD `shouldBe` (Right $ Just $ Number 14)
      rE ← callDuktape (fromJust ctx) [] "double" []
      rE `shouldBe` (Left "TypeError: error (rc -6)")
      rF ← evalDuktape (fromJust ctx) "try { X.add(undefined, \"wtf\") } catch (e) { 0; }"
      rF `shouldBe` (Right $ Just $ Number 0)
      rG ← evalDuktape (fromJust ctx) "noop()"
      rG `shouldBe` (Right $ Nothing)
