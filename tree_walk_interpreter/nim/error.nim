var hadError* = false

proc report*(line: int, where: string, message: string) =
  stderr.write "[line ", line, "] Error", where, ": ", message, '\n'
  hadError = true

proc error*(line: int, message: string) =
  report line, "", message
