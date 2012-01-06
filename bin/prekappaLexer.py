# Expander Lexers module

import ply.lex as lex
import expanderTokens

def PkaLexer():
  # A string containing ignored characters (spaces and tabs)
  t_ignore  = ' \t'

  # Error handling rule
  def t_error(t):
    print("Illegal character " + t.value[0])
    t.lexer.skip(1)

  #List of token names required by ply.lex
  tokens = expanderTokens.pkaTokens

  t_ID = r'[A-Za-z0-9][A-Za-z0-9_\-]*'
  t_LABEL = r'\'[^\']+\''
  t_ISTATE = r'~[A-Za-z0-9_]+'
  t_LSTATE = r'!\d+|\?|!_'
  t_COMMA = r','
  t_ELLIPSIS = r'\.\.\.'
  t_AT = r'@'
  t_SET = r':='
  t_AND = r'&&'
  t_OR = r'\|\|'
  t_DELETE = r'\$DEL'
  t_INTRO = r'\$ADD'
  t_SNAPSHOT = r'\$SNAPSHOT'
  t_STOP = r'\$STOP'
  t_EVENT = r'\[E\]'
  t_TIME = r'\[T\]'
  t_INFINITY = r'\[inf\]'
  t_PI = r'\[pi\]'
  t_NOT = r'\[NOT\]'
  t_EMAX = r'\[emax\]'
  t_TMAX = r'\[tmax\]'
  t_LPAREN = r'\('
  t_RPAREN = r'\)'
  t_PKVARIABLE = r'%((loc|org|dst)\[\d+\]|cell)'
  t_DIFFTARGET = r'%'
  t_RELATOR = r'>|<|='
  t_OPERATOR = r'\*|/|\+|-|\^|\[mod\]'
  t_FUNCTION = r'\[(exp|sin|cos|tan|int|sqrt|log)\]'
  t_BOOL = r'[true]|[false]'
  t_COMMENT = r'\#.+'
  t_EXCLAMATION = r'!'
  t_TILDE = r'~'

  def t_DO(t):
    r'do'
    return t

  def t_UNTIL(t):
    r'until'
    return t

  def t_FLOAT(t):
    r'(\d+\.\d*|\d*\.\d+)([Ee][\+-]?\d+)|(\d+\.\d*|\d*\.\d+)'
    return t

  def t_INTEGER(t):
    r'(\d+)([Ee][\+-]?\d+)|\d+'
    return t
    
  def t_RARROW(t):
    r'->'
    return t

  def t_LOCL(t):
    r'%locl:'
    return t

  def t_LOCM(t):
    r'%locm:'
    return t

  def t_LOC(t):
    r'%loc:'
    return t

  def t_ESIGNATURE(t):
    r'%expand-agent:'
    return t

  def t_SIGNATURE(t):
    r'%agent:'
    return t

  def t_ERULE(t):
    r'%expand-rule:'
    return t

  def t_EINIT(t):
    r'%expand-init:'
    return t

  def t_INIT(t):
     r'%init:'
     return t

  def t_EOBS(t):
    r'%expand-obs:'
    return t

  def t_OBS(t):
    r'%obs:'
    return t
  
  def t_EPLOT(t):
    r'%expand-plot:'
    return t

  def t_PLOT(t):
    r'%plot:'
    return t

  def t_EVAR(t):
    r'%expand-var:'
    return t

  def t_VAR(t):
    r'%var:'
    return t

  def t_EMOD(t):
    r'%expand-mod:'
    return t

  def t_MOD(t):
    r'%mod:'
    return t

  def t_BACKSLASHNEWLINE(t):
    r'\\[ \t]*\n'
    pass

  def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    t.value = len(t.value)
    return t
  
  return lex.lex()

