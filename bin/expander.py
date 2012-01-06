#!/usr/bin/python

import sys

import argparse

import prekappaParser
import prekappaLexer
import printing

def usage():
  print("Usage: expander.py [options] prekappa_file\n\nFor more info, please type expander.py -h", file=sys.stderr)
  sys.exit(0)

def main():
  commandLineParser = argparse.ArgumentParser(description="expands prekappa syntax onto formal kappa syntax")
  commandLineParser.add_argument("prekappaFile")
  commandLineParser.add_argument("-f","--full-matrix",dest="fullMatrix",action="store_true",
                    default=False,help="expand whole matrices, no matter the values in their cells")
  commandLineParser.add_argument("-o","--out-file",dest="outFile",default=sys.stdout,type=argparse.FileType('w'),
                    help="Output file")
  options = commandLineParser.parse_args()
  
  filename = options.prekappaFile

  try:
    PKA = open(filename, "r")
  except IOError:
    print("Couldn't open " + filename, file=sys.stderr)
    usage()

  lexer = prekappaLexer.PkaLexer()
  parser = prekappaParser.PkaParser(options)

  if (0):
    lexer.input(PKA.read())

    while True:
      tok = lexer.token()
      if not tok: break
      print(tok.type, tok.value, tok.lineno, tok.lexpos)
    PKA.seek(0)

  if (1):
    printing.printStr("# Created by expander.py\n")
    parser.parse(PKA.read(), lexer=lexer)
    #print(result)

main()
