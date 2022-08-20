import error
import os
import parser
import scanner

var hadError = false

proc report(line: int, where: string, message: string) =
  stderr.write "[line ", line, "] Error", where, ": ", message
  hadError = true

proc error(line: int, message: string) =
  report line, "", message

proc run(contents: string) =
  for tok in scanTokens contents:
    echo $tok

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
