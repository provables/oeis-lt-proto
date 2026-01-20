-- This module serves as the root of the `OeisLtProto` library.
-- Import modules here that should be built as part of the library.
import Lean

open Lean

structure OEISContext where
  env : Environment
  ctx : Core.Context
  state : Core.State

abbrev OEISM := ReaderT OEISContext IO

-- Implement monad lift from IO -> EIO Myerror so the user can use IO inside OEISM (and
-- make OEISM in EIO MyError)

abbrev PluginFunction := Σ input : Type, Σ _ : FromJson input, Σ output : Type, Σ _ : ToJson output,
  input → OEISM output

structure Plugin where
  cmd : String
  function : PluginFunction

def mkPlugin {a b : Type} [FromJson a] [ToJson b] (cmd : String) (f : a → OEISM b) : Plugin := {
  cmd,
  function := ⟨ _, inferInstance, _, inferInstance, f ⟩
}

-- Client provides a value `plugin : Plugin`.
