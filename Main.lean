import Aoc2024
import Cli

open Cli

def runMainCmd (p : Parsed) : IO UInt32 :=
  return 0

def mainCmd : Cmd := `[Cli|
  mainCmd VIA runMainCmd; ["0.0.1"]
  "Run the main command"
]

def main (args : List String) : IO UInt32 :=
  mainCmd.validate args
