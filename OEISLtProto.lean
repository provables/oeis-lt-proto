-- This module serves as the root of the `OeisLtProto` library.
-- Import modules here that should be built as part of the library.
import Lean

open Lean Elab

structure OEISContext where
  env : Environment
  ctx : Core.Context
  state : Core.State

inductive OEISError where
  | JSONDecodeError (e : String)
  | UserError (e : String)
  | IOError (e : IO.Error)

instance : Repr OEISError where
  reprPrec
    | .JSONDecodeError e, _ => f!"JSONDecodeError: {e}"
    | .UserError e, _ => f!"UserError: {e}"
    | .IOError e, _ => f!"IOError: {e}"

abbrev OEISM := ReaderT OEISContext (EIO OEISError)

instance : MonadLift IO OEISM where
  monadLift o := IO.toEIO OEISError.IOError o

def f : OEISM Nat := do
  IO.println ""
  throw <| OEISError.UserError ""
  return 1
  --throw <| IO.Error.userError ""

-- Implement monad lift from IO -> EIO Myerror so the user can use IO inside OEISM (and
-- make OEISM in EIO MyError)

abbrev PluginFunction := Σ input : Type, Σ _ : FromJson input, Σ output : Type, Σ _ : ToJson output,
  input → OEISM output

structure Plugin : Type where
  cmd : String
  function : Json → OEISM Json

def mkPlugin {a b : Type} [FromJson a] [ToJson b] (cmd : String) (f : a → OEISM b)
    : Plugin :=
  let g (x : Json) := do
    let .ok (obj : a) := FromJson.fromJson? x
      | throw <| .JSONDecodeError s!"JSON input cannot be converted to type of plugin function"
    return ToJson.toJson (← f obj)
  ⟨cmd, g⟩

-- Client provides a value `plugin : Plugin`.
