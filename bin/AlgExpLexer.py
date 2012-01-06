# Algebraic Expressions Lexer

import ply.lex as lex
import expanderTokens

def AlgExpLexer():
  # A string containing ignored characters (spaces and tabs)
  t_ignore  = ' \t'

  # Error handling rule
  def t_error(t):
    print("Illegal character " + t.value[0])
    t.lexer.skip(1)
  
  tokens = expanderTokens.algExpTokens

  # Regular expression rules for simple tokens
  t_AEPLUS     = r'\+'
  t_AEMINUS    = r'-'
  t_AETIMES    = r'\*'
  t_AEDIVIDE   = r'/'
  t_AELPAREN   = r'\('
  t_AERPAREN   = r'\)'
  t_AEPI       = r'\[pi\]'
  t_AEFUNCTION = r'\[(exp|sin|cos|tan|int|sqrt|log)\]'

  # A regular expression rule with some action code
  # Note addition of self parameter since we're in a class
  def t_AENUMBER(t):
    r'(\d+\.\d*|\d*\.\d+|\d+)([Ee][\+-]?\d+)|(\d+\.\d*|\d*\.\d+|\d+)'
    t.value = float(t.value)
    return t

  # Define a rule so we can track line numbers
  def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    return t

  return lex.lex()
