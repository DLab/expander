import ply.yacc as yacc
import math

import expanderTokens

def AlgExpParser():
  # Error rule for syntax errors
  def p_error(p):
    print("Syntax error in input! " + str(p), file=sys.stderr)
    sys.exit(1)
  
  tokens = expanderTokens.algExpTokens

  def p_expression_plus(p):
    'expression : expression AEPLUS term'
    p[0] = p[1] + p[3]

  def p_expression_minus(p):
    'expression : expression AEMINUS term'
    p[0] = p[1] - p[3]

  def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

  def p_term_times(p):
    'term : term AETIMES factor'
    p[0] = p[1] * p[3]

  def p_term_div(p):
    'term : term AEDIVIDE factor'
    p[0] = p[1] / p[3]

  def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

  def p_factor_num(p):
    'factor : AENUMBER'
    p[0] = p[1]

  def p_factor_pi(p):
    'factor : AEPI'
    p[0] = math.pi

  def p_factor_function(p):
    'factor : AEFUNCTION factor'
    if p[1] == "[exp]":
      p[0] = math.exp(p[2])
    elif p[1] == "[sin]":
      p[0] = math.sin(p[2])
    elif p[1] == "[cos]":
      p[0] = math.cos(p[2])
    elif p[1] == "[tan]":
      p[0] = math.tan(p[2])
    elif p[1] == "[int]":
      p[0] = int(p[2])
    elif p[1] == "[sqrt]":
      p[0] = math.sqrt(p[2])
    elif p[1] == "[log]":
      p[0] = math.log(p[2])
    else:
      print("Error, function " + p[1] + " is not supported.", file=sys.stderr)
      sys.exit(1)

  def p_factor_expr(p):
    'factor : AELPAREN expression AERPAREN'
    p[0] = p[2]

  # Build the parser
  return yacc.yacc()
