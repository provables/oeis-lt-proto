-- This module serves as the root of the `OeisLtProto` library.
-- Import modules here that should be built as part of the library.
import Lean

open Lean

structure OEISContext where
  env : Environment
  ctx : Core.Context
  state : Core.State

inductive OEISError where
  | JSONDecodeError (e : String)
  | UserError (e : String)

abbrev OEISM := ReaderT OEISContext IO

-- Implement monad lift from IO -> EIO Myerror so the user can use IO inside OEISM (and
-- make OEISM in EIO MyError)

abbrev PluginFunction := Σ input : Type, Σ _ : FromJson input, Σ output : Type, Σ _ : ToJson output,
  input → OEISM output

structure Plugin : Type where
  cmd : String
  function : Json → OEISM Json

    -- let .ok w := FromJson.fromJson? inp |>.map (fun x => v.function x) |>.map (fun y => do
    --   let z ← y
    --   return ToJson.toJson z
def mkPlugin {a b : Type} [FromJson a] [ToJson b] (cmd : String) (f : a → OEISM b)
    : Plugin :=
  let g (x : Json) := do
    let .ok v := FromJson.fromJson? x |>.map f |>.map (fun y => do return ToJson.toJson (← y))
      | throw <| IO.Error.userError "json failed"
    v
  ⟨cmd, g⟩

-- Client provides a value `plugin : Plugin`.
