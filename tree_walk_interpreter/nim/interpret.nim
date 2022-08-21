import error
import os
import parser
import scanner

proc run(contents: string) =
  let tokens = scanTokens contents
  for tok in tokens:
    echo $tok
  let expr = parse tokens
  if hadError: return
  echo $expr

proc runFile(filename: string) =
  run filename.readFile
  if hadError: quit 65

proc runPrompt() =
  while true:
    try:
      stdout.write "> "
      run stdin.readLine
      hadError = false
    except EOFError:
      break

proc main() =
  let args = commandLineParams()
  case len args:
    of 1: runFile args[0]
    of 0: runPrompt()
    else: quit "Error: Expected at most 1 argument, i.e., the file to run.", 64

when isMainModule:
  main()
