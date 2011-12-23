#!/usr/bin/python

import ply.lex as lex
import ply.yacc as yacc
import sys
import collections
import functools
import copy
import math
import re

locations = collections.OrderedDict()
locationLists = {}
locationMatrices = {}
signatures = {}
nullInstruction = {}

pkaTokens = ['LOC',
             'LOCL',
             'LOCM',
             'ESIGNATURE',
             'SIGNATURE',
             'ERULE',
             'EINIT',
             'INIT',
             'EOBS',
             'OBS',
             'EPLOT',
             'PLOT',
             'EVAR',
             'VAR',
             'EMOD',
             'MOD',
             'ID',
             'LABEL',
             'NUMBER',
             'ISTATE',
             'LSTATE',
             'COMMA',
             'ELLIPSIS',
             'AT',
             'RARROW',
             'OPERATOR',
             'FUNCTION',
             'LPAREN',
             'RPAREN',
             'DIFFTARGET',
             'PKVARIABLE',
             'DO',
             'UNTIL',
             'SET',
             'AND',
             'OR',
             'DELETE',
             'INTRO',
             'SNAPSHOT',
             'STOP',
             'EVENT',
             'TIME',
             'INFINITY',
             'PI',
             'EMAX',
             'TMAX',
             'RELATOR',
             'BOOL',
             'NOT',
             'COMMENT',
             'EXCLAMATION',
             'NEWLINE',
             'BACKSLASHNEWLINE',
             ]
  
# Error rule for syntax errors
def p_error(p):
  print("Syntax error in input! " + str(p), file=sys.stderr)
  sys.exit(1)
  
def t_error(t):
  print("Illegal character \'", t.value[0], "\'", file=sys.stderr)
  t.lexer.skip(1)
  
t_ignore = ' \t'

def PkaLexer():
  #List of token names required by ply.lex
  tokens = pkaTokens

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

  def t_DO(t):
    r'do'
    return t

  def t_UNTIL(t):
    r'until'
    return t

  def t_NUMBER(t):
    r'(\d+\.\d*|\d*\.\d+|\d+)([Ee][\+-]?\d+)|(\d+\.\d*|\d*\.\d+|\d+)'
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

def PkaParser():
  tokens = pkaTokens
  tokens.remove('BACKSLASHNEWLINE')

  precedence = (
    ('left', 'COMMA'),
    ('right', 'ID'),
  )

  def p_prekappa(p):
    '''prekappa : prekappa instruction
                | prekappa info
                | instruction
                | info'''
    pass;

  def p_instruction_newline(p): #get rid of the newline between every pair of instructions
    'instruction : instruction NEWLINE'
    if(p[2] > 1):
      print()
    pass;

  def p_instruction(p):
    '''instruction : esignature
                   | signature
                   | erule
                   | rule
                   | einit
                   | init
                   | eobs
                   | obs
                   | eplot
                   | plot
                   | evar
                   | var
                   | emod
                   | mod'''
    pass;

  def p_info_newline(p):
    'info : info NEWLINE'
    if(p[2] > 1):
      print()
    pass;

  def p_info(p):
    '''info : comment
            | loc
            | locl
            | locm'''
    pass;

  def p_comment(p):
    'comment : COMMENT'
    print(p[1])

  def p_loc(p):
    'loc : LOC ID numberlist'
    global locations
    if locations.__contains__(p[2]):
      sys.stderr.write("Warning!, location " + p[2] + " redefined\n")
    locations[p[2]] = []
    for i in map(float,p[3]):
      locations[p[2]].append(i)
    printLocation(p[2], p[3])

  def p_locl(p):
    'locl : LOCL idlist'
    global locationLists
    locationLists[p[2][0]] = p[2][1:]
    printLocationList(p[2][0], p[2][1:])

  def p_idlist(p):
    'idlist : idlist ID'
    p[0] = p[1] + [p[2]]

  def p_idlist_begin(p):
    'idlist : ID'
    p[0] = [p[1]]

  def p_locm(p):
    'locm : initmatrix idlist NEWLINE mrows'
    global locationMatrices
    mr = p[4]
    matrixId = p[2][0]
    destList = p[2][1:]
    tupleList = []
    newMR = []
    for i in range(0,len(mr)):
      newMR.append(mr[i]["mrow"])
      org = newMR[i][0]
      j = 1
      for dst in destList:
        tupleList.append((org,dst,newMR[i][j]))
        j += 1
    locationMatrices[matrixId] = tupleList
    printLocationMatrix(matrixId, [p[2]] + newMR)
    if (mr[i]["nlines"] > 1):
      print()

  def p_initmatrix(p):
    '''initmatrix : LOCM
                  | LOCM NEWLINE'''
    p[0] = p[1]

  def p_mrows(p):
    'mrows : mrows mrow'
    p[0] = p[1] + [p[2]]

  def p_mrows_begin(p):
    'mrows : mrow'
    p[0] = [p[1]]

  def p_mrow(p):
    'mrow : ID numberlist NEWLINE'
    p[0] = {}
    p[0]["nlines"] = p[3]
    p[0]["mrow"] = [p[1]] + p[2]

  def p_numberlist(p):
    'numberlist : numberlist NUMBER'
    p[0] = p[1] + [float(p[2])]

  def p_numberlist_begin(p):
    'numberlist : NUMBER'
    p[0] = [float(p[1])]

  def p_esignature(p):
    'esignature : ESIGNATURE ID agentsignature'
    global signatures
    signatures[p[3]["name"]] = p[3]["signature"]
    print("# expanding in " + p[2], end=" ")
    printSignature(p[3])
    expand(p[2], p[3], esignature)

  def p_agentsignature(p):
    'agentsignature : ID LPAREN sitesignaturelist RPAREN'
    agentsignature = {}
    agentsignature["name"] = p[1]
    agentsignature["signature"] = p[3]
    p[0] = agentsignature
  
  def p_agentemptysignature(p):
    'agentsignature : ID LPAREN RPAREN'
    agentsignature = {}
    agentsignature["name"] = p[1]
    agentsignature["signature"] = []
    p[0] = agentsignature

  def p_sitesignaturelist(p):
    'sitesignaturelist : sitesignaturelist COMMA sitesignature'
    p[0] = p[1] + [p[3]]

  def p_sitesignaturelist_begin(p):
    'sitesignaturelist : sitesignature'
    p[0] = [p[1]]

  def p_sitesignature_istate(p):
    'sitesignature : ID istatelist'
    sitesignature = {}
    sitesignature["name"] = p[1]
    sitesignature["istatelist"] = p[2]
    p[0] = sitesignature

  def p_sitesignature(p):
    'sitesignature : ID'
    sitesignature = {}
    sitesignature["name"] = p[1]
    sitesignature["istatelist"] = []
    p[0] = sitesignature

  def p_istatelist(p):
    'istatelist : istatelist ISTATE'
    p[0] = p[1] + [p[2]]
    
  def p_istatelist_begin(p):
    'istatelist : ISTATE'
    p[0] = [p[1]]

  def p_signature(p):
    'signature : SIGNATURE agentsignature'
    global signatures
    signatures[p[2]["name"]] = p[2]["signature"]
    printSignature(p[2])

  def p_erule(p):
    'erule : ERULE ID LABEL reaction'
    p[4]["label"] = p[3]
    print("# expanding in " + p[2], end=" ")
    printRule(p[4])
    expand(p[2], p[4], erule)
  
  def p_erule_no_label(p):
    'erule : ERULE ID reaction'
    p[4]["label"] = ""
    print("# expanding in " + p[2], end=" ")
    printRule(p[3])
    expand(p[2], p[3], erule)

  def p_reaction(p):
    'reaction : expression RARROW expression AT algexp'
    reaction = {}
    reaction["lhs"] = p[1]
    reaction["rhs"] = p[3]
    reaction["rate"] = p[5]
    p[0] = reaction

  def p_introduction(p):
    'reaction : RARROW expression AT algexp'
    reaction = {}
    reaction["lhs"] = []
    reaction["rhs"] = p[2]
    reaction["rate"] = p[4]
    p[0] = reaction
  
  def p_deletion(p):
    'reaction : expression RARROW AT algexp'
    reaction = {}
    reaction["lhs"] = p[1]
    reaction["rhs"] = []
    reaction["rate"] = p[4]
    p[0] = reaction
  
  def p_algexp_op(p):
    'algexp : algexp OPERATOR algexp'
    p[0] = p[1] + " " + p[2] + " " + p[3]
  
  def p_algexp_paren(p):
    'algexp : LPAREN algexp RPAREN'
    p[0] = p[1] + " " + p[2] + " " + p[3]

  def p_algexp_function(p):
    'algexp : FUNCTION algexp'
    p[0] = p[1] + " " + p[2]

  def p_algexp_init(p):
    '''algexp : NUMBER
              | PI
              | INFINITY
              | EMAX
              | TMAX
              | TIME
              | EVENT
              | LABEL
              | PKVARIABLE'''
    p[0] = p[1]

  def p_chain(p):
    'expression : expression COMMA ELLIPSIS COMMA agent'
    p[1][-2]["firstOnChain"] = 1
    p[1][-1]["secondOnChain"] = 1
    p[5]["lastOnChain"] = 1
    p[0] = p[1] + [p[5]]

  def p_expression(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

  def p_expression_expression(p):
    'expression : expression COMMA expression'
    p[0] = p[1] + p[3]

  def p_expression_begin(p):
    'expression : agent'
    p[0] = [p[1]]

  def p_agent(p):
    'agent : ID LPAREN sitelist RPAREN'
    agent = {}
    agent["name"] = p[1]
    agent["interface"] = p[3]
    p[0] = agent

  def p_agent_emptyInferface(p): #TODO add support for alternative syntaxis of agents and bonds
    'agent : ID LPAREN RPAREN'
    agent = {}
    agent["name"] = p[1]
    agent["interface"] = []
    p[0] = agent

  def p_sitelist(p):
    'sitelist : sitelist COMMA site'
    p[0] = p[1] + [p[3]]

  def p_sitelist_begin(p):
    'sitelist : site'
    p[0] = [p[1]]

  def p_site(p):
    'site : ID'
    site = {}
    site["name"] = p[1]
    site["istate"] = ""
    site["lstate"] = ""
    p[0] = site

  def p_site_istate(p):
    'site : ID ISTATE'
    site = {}
    site["name"] = p[1]
    site["istate"] = p[2]
    site["lstate"] = ""
    p[0] = site
  
  def p_site_locfield(p):
    'site : DIFFTARGET'
    site = {}
    site["name"] = p[1]
    site["istate"] = ""
    site["lstate"] = ""
    p[0] = site

  def p_lstate(p):
    'lstate : LSTATE'
    p[0] = p[1]

  def p_lstate_algexp(p):
    'lstate : EXCLAMATION algexp'
    p[0] = p[1] + p[2]

  def p_site_lstate(p):
    'site : ID lstate'
    site = {}
    site["name"] = p[1]
    site["istate"] = ""
    site["lstate"] = p[2]
    p[0] = site

  def p_site_istate_lstate(p):
    'site : ID ISTATE lstate'
    site = {}
    site["name"] = p[1]
    site["istate"] = p[2]
    site["lstate"] = p[3]
    p[0] = site
  
  def p_site_lstate_istate(p):
    'site : ID lstate ISTATE'
    site = {}
    site["name"] = p[1]
    site["lstate"] = p[2]
    site["istate"] = p[3]
    p[0] = site
  
  def p_rule(p):
    'rule : LABEL reaction'
    p[2]["label"] = p[1]
    printRule(p[2])

  def p_rule_no_label(p):
    'rule : reaction'
    p[1]["label"] = ""
    printRule(p[1])
  
  def p_einit(p):
    'einit : EINIT ID algexp expression'
    init = {}
    init["quantity"] = p[3]
    init["expression"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printInit(init)
    expand(p[2], init, einit)

  def p_init(p):
    'init : INIT NUMBER expression'
    init = {}
    init["quantity"] = p[2]
    init["expression"] = p[3]
    printInit(init)

  def p_eobs(p):
    'eobs : EOBS ID LABEL expression'
    obs = {}
    obs["label"] = p[3]
    obs["expression"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printObs(obs)
    expand(p[2], obs, eobs)

  def p_eobs_algexp(p):
    'eobs : EOBS ID LABEL algexp'
    obs = {}
    obs["label"] = p[3]
    obs["algexp"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printObs2(obs)
    expand(p[2], obs, eobs2)

  def p_obs(p):
    'obs : OBS LABEL expression'
    obs = {}
    obs["label"] = p[2]
    obs["expression"] = p[3]
    printObs(obs)

  def p_obs_algexp(p):
    'obs : OBS LABEL algexp'
    obs = {}
    obs["label"] = p[2]
    obs["algexp"] = p[3]
    printObs2(obs)
  
  def p_eplot(p):
    'eplot : EPLOT ID LABEL expression'
    plot = {}
    plot["label"] = p[3]
    plot["expression"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printPlot(plot)
    expand(p[2], plot, eplot)

  def p_eplot_algexp(p):
    'eplot : EPLOT ID LABEL algexp'
    plot = {}
    plot["label"] = p[3]
    plot["algexp"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printPlot2(plot)
    expand(p[2], plot, eplot2)

  def p_plot(p):
    'plot : PLOT LABEL expression'
    plot = {}
    plot["label"] = p[2]
    plot["expression"] = p[3]
    printPlot(plot)

  def p_plot_algexp(p):
    'plot : PLOT LABEL algexp'
    plot = {}
    plot["label"] = p[2]
    plot["algexp"] = p[3]
    printPlot2(plot)
  
  def p_evar(p):
    'evar : EVAR ID LABEL expression'
    var = {}
    var["label"] = p[3]
    var["expression"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printVar(var)
    expand(p[2], var, evar)

  def p_evar_algexp(p):
    'evar : EVAR ID LABEL algexp'
    var = {}
    var["label"] = p[3]
    var["algexp"] = p[4]
    print("# expanding in " + p[2], end=" ")
    printVar2(var)
    expand(p[2], var, evar2)

  def p_var(p):
    'var : VAR LABEL expression'
    var = {}
    var["label"] = p[2]
    var["expression"] = p[3]
    printVar(var)

  def p_var_algexp(p):
    'var : VAR LABEL algexp'
    var = {}
    var["label"] = p[2]
    var["algexp"] = p[3]
    printObs2(var)

  def p_emod(p):
    'emod : EMOD ID boolexp DO effect UNTIL boolexp'
    mod = {}
    mod["boolexp"] = p[3]
    mod["effect"] = p[5]
    mod["endexp"] = p[7]
    print("# expanding in " + p[2], end=" ")
    printMod(mod)
    expand(p[2], mod, emod)
  
  def p_emod_noend(p):
    'emod : EMOD ID boolexp DO effect'
    mod = {}
    mod["boolexp"] = p[3]
    mod["effect"] = p[5]
    mod["endexp"] = ""
    print("# expanding in " + p[2], end=" ")
    printMod(mod)
    expand(p[2], mod, emod)

  def p_boolexp_rel(p):
    'boolexp : algexp RELATOR algexp'
    p[0] = p[1] + " " + p[2] + " " + p[3]

  def p_boolexp_logic(p):
    '''boolexp : boolexp AND boolexp
               | boolexp OR boolexp'''
    p[0] = p[1] + " " + p[2] + " " + p[3]

  def p_boolexp_not(p):
    'boolexp : NOT boolexp'
    p[0] = p[1] + " " + p[2]

  def p_boolexp_bool(p):
    'boolexp : BOOL'
    p[0] = p[1]

  def p_boolexp_paren(p):
    'boolexp : LPAREN boolexp RPAREN'
    p[0] = p[1] + " " + p[2] + " " + p[3]

  def p_effect_pert_mixture(p):
    '''effect : INTRO algexp expression
              | DELETE algexp expression'''
    effect = {}
    effect["keyword"] = p[1]
    effect["algexp"] = p[2]
    effect["expression"] = p[3]
    p[0] = effect

  def p_effect_nopar(p):
    '''effect : SNAPSHOT
              | STOP'''
    effect = {}
    effect["keyword"] = p[1]
    p[0] = effect

  def p_effect_chcons(p):
    'effect : LABEL SET algexp'
    effect = {}
    effect["keyword"] = p[2]
    effect["rule"] = p[1]
    effect["rate"] = p[3]
    p[0] = effect

  def p_mod(p):
    'mod : MOD boolexp DO effect UNTIL boolexp'
    mod = {}
    mod["boolexp"] = p[3]
    mod["effect"] = p[5]
    mod["endexp"] = p[7]
    printMod(mod)
  
  def p_mod_noend(p):
    'mod : MOD ID boolexp DO effect'
    mod = {}
    mod["boolexp"] = p[3]
    mod["effect"] = p[5]
    mod["endexp"] = ""
    printMod(mod)

  return yacc.yacc()

def printLocation(name, dataArray):
  print("# defined location " + name + " with data array = " + " ".join(map(str,dataArray)))

def printLocationList(name, llist):
  print("# defined location list " + name + " containing: " + ",".join(llist))

def printLocationMatrix(name, lmatrix):
  print("# defined location matrix " + name + ":")
  for mrow in lmatrix:
    print("#\t" + "\t".join(map(str, mrow)))

def printSignature(agentsignature):
  print("%agent: " + agentsignature["name"] + "(", end='')
  sites = []
  for sitesignature in agentsignature["signature"]:
    sites.append(sitesignature["name"] + "".join(sitesignature["istatelist"]))
  print(",".join(sites) + ")")

def siteToString(site):
  return site["name"] + site["istate"] + site["lstate"]

def agentToString(agent):
  s = agent["name"] + "("
  s += ",".join(map(siteToString, agent["interface"]))
  if "lastOnChain" in agent:
    s = "...," + s
  return s + ")"

def expressionToString(expression):
  return ",".join(map(agentToString, expression))

def effectToString(effect):
  if (effect["keyword"] == "$ADD" or effect["keyword"] == "$DEL"):
    return " ".join([effect["keyword"],effect["algexp"],expressionToString(effect["expression"])])
  elif (effect["keyword"] == "$SNAPSHOT" or effect["keyword"] == "$STOP"):
    return effect["keyword"]
  else:
    return " ".join([effect["rule"],effect["keyword"],effect["rate"]])
  
def printRule(rule):
  if (rule != nullInstruction):
    print(rule["label"] + " " + expressionToString(rule["lhs"]) + " -> " + expressionToString(rule["rhs"]) + " @ " + rule["rate"])

def printInit(init):
  if (init != nullInstruction):
    e = expressionToString(init["expression"])
    if (len(init["expression"]) > 1):
      e = "(" + e + ")"
    print("%init: " + init["quantity"] + " " + e)

def printObs(obs):
  if (obs != nullInstruction):
    print("%obs: " + obs["label"] + " " + expressionToString(obs["expression"]))

def printObs2(obs):
  if (obs != nullInstruction):
    print("%obs: " + obs["label"] + " " + obs["algexp"])

def printVar(var):
  if (var != nullInstruction):
    print("%var: " + var["label"] + " " + expressionToString(var["expression"]))

def printVar2(var):
  if (var != nullInstruction):
    print("%var: " + var["label"] + " " + var["algexp"])

def printPlot(plot):
  if (plot != nullInstruction):
    print("%plot: " + plot["label"] + " " + expressionToString(plot["expression"]))

def printPlot2(plot):
  if (plot != nullInstruction):
    print("%plot: " + plot["label"] + " " + plot["algexp"])

def printMod(mod):
  if (mod != nullInstruction):
    print(" ".join(["%mod:",mod["boolexp"],"do",effectToString(mod["effect"])]), end="")
    if (mod["endexp"] != ""):
      print(" " + "until" + " " + mod["endexp"], end="")
    print()

def esignature(sign, domain):
  if (domain[0].__class__ == "".__class__): #if the first element is a string, domain is a list of locs
    locsignature = {}
    locsignature["name"] = "loc"
    locsignature["istatelist"] = []
    for loc in domain:
      locsignature["istatelist"].append("~" + loc)
    sign["signature"].append(locsignature)
  else: #otherwise the first element is a tuple, so the domain is a matrix of locs
    orgsignature = {}
    orgsignature["name"] = "org"
    orgsignature["istatelist"] = []
    dstsignature = {}
    dstsignature["name"] = "dst"
    dstsignature["istatelist"] = []
    for (org,dst,n) in domain:
      if ("~" + org not in orgsignature["istatelist"]):#This ensures that the input order is maintained
        orgsignature["istatelist"].append("~" + org)
      if ("~" + dst not in dstsignature["istatelist"]):
        dstsignature["istatelist"].append("~" + dst)
    sign["signature"].append(orgsignature)
    sign["signature"].append(dstsignature)
  printSignature(sign)

def isBimol(expression):
  def searchLink(linkList, link, i):
    for agent in range(0,len(linkList)):
      if (agent != i):
        for l in linkList[agent]:
          if (link == l):
            return agent
    print("Error in links of a rule", file=sys.stderr)
    sys.exit(1)

  def isConnected(graph):
    visited = []
    for i in range(0,len(graph)):
      visited.append(False)
    def isConnectedIter(i):
      visited[i] = True
      if (all(n == True for n in visited)):
        return True
      for n in graph[i]:
        if (not visited[n]):
          if (isConnectedIter(n)):
            return True
      return False
    return isConnectedIter(0)

  linkList = []
  i = 0
  for agent in expression:
    linkList.append([])
    for site in agent["interface"]:
      link = site["lstate"]
      if (link != "?" and link != "!_" and link != ""):
        linkList[i].append(link)
    i += 1
  connections = []
  i = 0
  for agent in linkList:
    connections.append([])
    for link in agent:
      connections[i].append(searchLink(linkList, link, i))
  return not isConnected(connections)

def agentHasSite(agentName, siteName):
  signature = signatures[agentName]
  for site in signature:
    if (site["name"] == siteName):
      return True
  return False

def insertSite(siteName, agent, istate):
  if (agentHasSite(agent["name"], siteName)):
    site = {}
    site["name"] = siteName
    site["istate"] = "~" + istate
    site["lstate"] = ""
    agent["interface"].append(site)

def insertLocInAgent(agent, loc):
  insertSite("loc", agent, loc)

def insertOrgInAgent(agent, org):
  insertSite("org", agent, org)
  insertSite("loc", agent, org)

def insertDstInAgent(agent, dst):
  insertSite("dst", agent, dst)

def insertLocInExpression(expression, loc):
  for agent in expression:
    insertLocInAgent(agent, loc)

def insertOrgInExpression(expression, org):
  for agent in expression:
    insertOrgInAgent(agent, org)

def insertDstInExpression(expression, dst):
  for agent in expression:
    insertDstInAgent(agent, dst)

def insertSiteInEffect(siteName, effect, istate):
  if (effect["keyword"] == "$ADD" or effect["keyword"] == "$DEL"):
    effect["algexp"] = effect["algexp"].replace("%" + siteName, istate)
    for agent in effect["expression"]:
      insertSite(siteName, agent, istate)
  else:
    for key in effect.keys():
      effect[key] = effect[key].replace("%" + siteName, istate)

def insertLocInEffect(effect, loc):
  insertSiteInEffect("loc", effect, loc)

def insertOrgInEffect(effect, org):
  insertSiteInEffect("org", effect, org)

def insertDstInEffect(agent, dst):
  insertSiteInEffect("dst", agent, dst)

def interfaceHasSite(interface, siteName):
  for site in interface:
    if (site["name"] == siteName):
      return True
  return False

def diffuseAgent(interface, dst):
  i = 0
  for site in interface:
    if (site["name"] == "loc"):
      site["istate"] = "~" + dst
    elif (site["name"] == "%"):
      p = i
    i += 1
  interface.pop(p)

def replaceLocVariable(algexp, vname, dataArray):
  splittedStr = algexp.split(" ")
  for i in range(len(splittedStr)):
    if re.match(vname + "\[\d+\]", splittedStr[i]):
      index = int(re.findall("\d+", splittedStr[i])[-1])
      if index < len(dataArray):
        splittedStr[i] = str(dataArray[index])
      else:
        print("Error replacing Loc variable", file=sys.stderr)
        sys.exit(1)
  return " ".join(splittedStr)

def joinInAList(e1, e2):
  return (e1,e2)

def solveAlgebraicExpressions(expression):
  for agent in expression:
    for site in agent["interface"]:
      if site["lstate"] != "" and site["lstate"] != "!_" and site["lstate"] != "?":
        site["lstate"] = "!" + str(int(aeParser.parse(site["lstate"][1:],lexer=aeLexer)))

def checkForAlgebraicExpressions(expression, vnames, dataArrays, cellValue):
  for agent in expression:
    for site in agent["interface"]:
      for (vname,dataArray) in map(joinInAList, vnames, dataArrays):
        for variable in re.findall(vname + "\[\d+\]", site["lstate"]):
          index = int(re.findall("\d+", variable)[0])
          if index < len(dataArray):
            site["lstate"] = site["lstate"].replace(variable, str(dataArray[index]))
          else:
            print("Error replacing " + vname + "[" + str(index) + "] variable in a expression", file=sys.stderr)
            sys.exit(2)
      for variable in re.findall("%cell", site["lstate"]):
        site["lstate"] = site["lstate"].replace(variable, str(cellValue))

def checkForChains(expression):
  newExpression = []
  for i in range(len(expression)):
    if "firstOnChain" in expression[i]:
      expression[i].pop("firstOnChain")
      continue
    if "secondOnChain" in expression[i]:
      expression[i].pop("secondOnChain")
      continue
    if "lastOnChain" in expression[i]:
      expression[i].pop("lastOnChain")
      newExpression = newExpression + buildChain(expression[i-2],expression[i-1],expression[i])
      continue
    newExpression = newExpression + [expression[i]]
  return newExpression

def erule(rule, domain):
  def makeDiffusions(expression, dst):
    for agent in expression:
      if(interfaceHasSite(agent["interface"], "%")):
        diffuseAgent(agent["interface"], dst)

  def encloseAndSuffix(prefix, suffix):
    return "(" + prefix + ") " + suffix

  def mixRuleWithLoc(loc):
    newRule = copy.deepcopy(rule)
    newRule["label"] = newRule["label"].replace("%loc",loc)
    checkForAlgebraicExpressions(newRule["lhs"],["%loc"],[locations[loc]],str(0))
    checkForAlgebraicExpressions(newRule["rhs"],["%loc"],[locations[loc]],str(0))
    solveAlgebraicExpressions(newRule["lhs"])
    solveAlgebraicExpressions(newRule["rhs"])
    newRule["lhs"] = checkForChains(newRule["lhs"])
    newRule["rhs"] = checkForChains(newRule["rhs"])
    insertLocInExpression(newRule["lhs"],loc)
    insertLocInExpression(newRule["rhs"],loc)
    newRule["rate"] = replaceLocVariable(newRule["rate"], "%loc", locations[loc])
    newRule["rate"] = newRule["rate"].replace("%loc", loc)
    #if (isBimol(rule["lhs"])):
    #  newRule["rate"] = encloseAndSuffix(newRule["rate"], "/ " + str(locations[loc]))
    return newRule
  
  def mixRuleWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newRule = copy.deepcopy(rule)
      newRule["label"] = newRule["label"].replace("%org",org)
      newRule["label"] = newRule["label"].replace("%dst",dst)
      checkForAlgebraicExpressions(newRule["lhs"],[org,dst],[locations[org],locations[dst]], str(r))
      checkForAlgebraicExpressions(newRule["rhs"],[org,dst],[locations[org],locations[dst]], str(r))
      solveAlgebraicExpressions(newRule["lhs"])
      solveAlgebraicExpressions(newRule["rhs"])
      newRule["lhs"] = checkForChains(newRule["lhs"])
      newRule["rhs"] = checkForChains(newRule["rhs"])
      insertOrgInExpression(newRule["lhs"],org)
      insertOrgInExpression(newRule["rhs"],org)
      insertDstInExpression(newRule["lhs"],dst)
      insertDstInExpression(newRule["rhs"],dst)
      newRule["rate"] = replaceLocVariable(newRule["rate"], "%org", locations[org])
      newRule["rate"] = replaceLocVariable(newRule["rate"], "%dst", locations[dst])
      newRule["rate"] = newRule["rate"].replace("%cell", str(r))
      newRule["rate"] = newRule["rate"].replace("%org", org)
      newRule["rate"] = newRule["rate"].replace("%dst", dst)
      #if (isBimol(rule["lhs"])):
      #  newRule["rate"] = encloseAndSuffix(newRule["rate"], "/ " + str(locations[org]))
      makeDiffusions(newRule["rhs"], dst)
      return newRule
    else:
      return nullInstruction

  ruleBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    ruleBuilder = mixRuleWithLoc
  else: #the type is tuple, it is a matrix
    ruleBuilder = mixRuleWithOrgDst
  for r in map(ruleBuilder, domain):
    printRule(r)

def einit(init, domain):
  def mixInitWithLoc(loc):
    newInit = copy.deepcopy(init)
    insertLocInExpression(newInit["expression"], loc)
    newInit["quantity"] = replaceLocVariable(newInit["quantity"], "%loc", locations[loc])
    newInit["quantity"] = str(int(aeParser.parse(newInit["quantity"],lexer=aeLexer)))
    return newInit
  
  def mixInitWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newInit = copy.deepcopy(init)
      insertOrgInExpression(newInit["expression"],org)
      insertDstInExpression(newInit["expression"],dst)
      newInit["quantity"] = replaceLocVariable(newInit["quantity"],"%org",locations[org])
      newInit["quantity"] = replaceLocVariable(newInit["quantity"],"%dst",locations[dst])
      newInit["quantity"] = newInit["quantity"].replace("%cell", str(r))
      newInit["quantity"] = str(int(aeParser.parse(newInit["quantity"],lexer=aeLexer)))
      return newInit
    else:
      return nullInstruction

  initBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    initBuilder = mixInitWithLoc
  else: #the type is tuple, it is a matrix
    initBuilder = mixInitWithOrgDst
  for i in map(initBuilder, domain):
    printInit(i)

def eobs(obs, domain):
  def mixObsWithLoc(loc):
    newObs = copy.deepcopy(obs)
    newObs["label"] = newObs["label"].replace("%loc",loc)
    insertLocInExpression(newObs["expression"],loc)
    return newObs
  
  def mixObsWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newObs = copy.deepcopy(obs)
      newObs["label"] = newObs["label"].replace("%org",org)
      newObs["label"] = newObs["label"].replace("%dst",dst)
      insertOrgInExpression(newObs["expression"],org)
      insertDstInExpression(newObs["expression"],dst)
      return newObs
    else:
      return nullInstruction

  obsBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    obsBuilder = mixObsWithLoc
  else: #the type is tuple, it is a matrix
    obsBuilder = mixObsWithOrgDst
  for r in map(obsBuilder, domain):
    printObs(r)

def eobs2(obs, domain):
  def mixObsWithLoc(loc):
    newObs = copy.deepcopy(obs)
    newObs["label"] = newObs["label"].replace("%loc",loc)
    newObs["algexp"] = newObs["algexp"].replace("%loc",loc)
    return newObs
  
  def mixObsWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newObs = copy.deepcopy(obs)
      newObs["label"] = newObs["label"].replace("%org",org)
      newObs["label"] = newObs["label"].replace("%dst",dst)
      newObs["algexp"] = newObs["algexp"].replace("%org",org)
      newObs["algexp"] = newObs["algexp"].replace("%dst",dst)
      return newObs
    else:
      return nullInstruction

  obsBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    obsBuilder = mixObsWithLoc
  else: #the type is tuple, it is a matrix
    obsBuilder = mixObsWithOrgDst
  for r in map(obsBuilder, domain):
    printObs2(r)

def eplot(plot, domain):
  def mixPlotWithLoc(loc):
    newPlot = copy.deepcopy(plot)
    newPlot["label"] = newPlot["label"].replace("%loc",loc)
    insertLocInExpression(newPlot["expression"],loc)
    return newPlot
  
  def mixPlotWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newPlot = copy.deepcopy(plot)
      newPlot["label"] = newPlot["label"].replace("%org",org)
      newPlot["label"] = newPlot["label"].replace("%dst",dst)
      insertOrgInExpression(newPlot["expression"],org)
      insertDstInExpression(newPlot["expression"],dst)
      return newPlot
    else:
      return nullInstruction

  plotBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    plotBuilder = mixPlotWithLoc
  else: #the type is tuple, it is a matrix
    plotBuilder = mixPlotWithOrgDst
  for r in map(plotBuilder, domain):
    printPlot(r)

def eplot2(plot, domain):
  def mixPlotWithLoc(loc):
    newPlot = copy.deepcopy(plot)
    newPlot["label"] = newPlot["label"].replace("%loc",loc)
    newPlot["algexp"] = newPlot["algexp"].replace("%loc",loc)
    return newPlot
  
  def mixPlotWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newPlot = copy.deepcopy(plot)
      newPlot["label"] = newPlot["label"].replace("%org",org)
      newPlot["label"] = newPlot["label"].replace("%dst",dst)
      newPlot["algexp"] = newPlot["algexp"].replace("%org",org)
      newPlot["algexp"] = newPlot["algexp"].replace("%dst",dst)
      return newPlot
    else:
      return nullInstruction

  plotBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    plotBuilder = mixPlotWithLoc
  else: #the type is tuple, it is a matrix
    plotBuilder = mixPlotWithOrgDst
  for r in map(plotBuilder, domain):
    printPlot2(r)

def evar(var, domain):
  def mixVarWithLoc(loc):
    newVar = copy.deepcopy(var)
    newVar["label"] = newVar["label"].replace("%loc",loc)
    insertLocInExpression(newVar["expression"],loc)
    return newVar
  
  def mixVarWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newVar = copy.deepcopy(var)
      newVar["label"] = newVar["label"].replace("%org",org)
      newVar["label"] = newVar["label"].replace("%dst",dst)
      insertOrgInExpression(newVar["expression"],org)
      insertDstInExpression(newVar["expression"],dst)
      return newVar
    else:
      return nullInstruction

  varBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    varBuilder = mixVarWithLoc
  else: #the type is tuple, it is a matrix
    varBuilder = mixVarWithOrgDst
  for r in map(varBuilder, domain):
    printVar(r)

def evar2(var, domain):
  def mixVarWithLoc(loc):
    newVar = copy.deepcopy(var)
    newVar["label"] = newVar["label"].replace("%loc",loc)
    newVar["algexp"] = newVar["algexp"].replace("%loc",loc)
    return newVar
  
  def mixVarWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newVar = copy.deepcopy(var)
      newVar["label"] = newVar["label"].replace("%org",org)
      newVar["label"] = newVar["label"].replace("%dst",dst)
      newVar["algexp"] = newVar["algexp"].replace("%org",org)
      newVar["algexp"] = newVar["algexp"].replace("%dst",dst)
      return newVar
    else:
      return nullInstruction

  varBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    varBuilder = mixVarWithLoc
  else: #the type is tuple, it is a matrix
    varBuilder = mixVarWithOrgDst
  for r in map(varBuilder, domain):
    printVar2(r)

def emod(mod, domain):
  def mixModWithLoc(loc):
    newMod = copy.deepcopy(mod)
    #newMod["boolexp"] = newMod["boolexp"].replace("%loc",loc)
    #newMod["endexp"] = newMod["endexp"].replace("%loc",loc)
    newMod["boolexp"] = replaceLocVariable(newMod["boolexp"],"%loc",locations[loc])
    newMod["endexp"] = replaceLocVariable(newMod["endexp"],"%loc",locations[loc])
    insertLocInEffect(newMod["effect"],loc)
    return newMod
  
  def mixModWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newMod = copy.deepcopy(mod)
      #newMod["boolexp"] = newMod["boolexp"].replace("%org",org)
      #newMod["boolexp"] = newMod["boolexp"].replace("%dst",dst)
      #newMod["endexp"] = newMod["endexp"].replace("%org",org)
      #newMod["endexp"] = newMod["endexp"].replace("%dst",dst)
      newMod["boolexp"] = replaceLocVariable(newMod["boolexp"],"%org",locations[org])
      newMod["boolexp"] = replaceLocVariable(newMod["boolexp"],"%dst",locations[dst])
      newMod["boolexp"] = newMod["boolexp"].replace("%cell",str(r))
      newMod["endexp"] = replaceLocVariable(newMod["endexp"],"%org",locations[org])
      newMod["endexp"] = replaceLocVariable(newMod["endexp"],"%dst",locations[dst])
      newMod["endexp"] = newMod["endexp"].replace("%cell",str(r))
      insertOrgInEffect(newMod["effect"],org)
      insertDstInEffect(newMod["effect"],dst)
      return newMod
    else:
      return nullInstruction

  modBuilder = None
  if (domain[0].__class__ == "".__class__): #if it is string, it is just loc
    modBuilder = mixModWithLoc
  else: #the type is tuple, it is a matrix
    modBuilder = mixModWithOrgDst
  for r in map(modBuilder, domain):
    printMod(r)

def expand(domainId, instruction, writer):
  if (domainId in locationLists):
    writer(instruction, locationLists[domainId])
  elif (domainId in locationMatrices):
    writer(instruction, locationMatrices[domainId])
  elif (domainId == "all"):
    writer(instruction, list(locations.keys()))
  else:
    print("Error, " + domainId + " doesn't exist", file=sys.stderr)
    sys.exit(2)

def setLinkValue(interface, siteName, lValue):
  for site in interface:
    if (site["name"] == siteName):
      if lValue == "" or lValue == "!_" or lValue == "?":
        site["lstate"] = lValue
      else:
        site["lstate"] = "!" + str(lValue)
      break

def findBond(left, right):
  for l in left["interface"]:
    for r in right["interface"]:
      if (l["lstate"] == r["lstate"]):
        return (r["name"], l["name"])
  print("Error in chain definition!, can't find prev and next sites", file=sys.stderr)
  sys.exit(3)

def getLinkValue(agent, siteName):
  for site in agent["interface"]:
    if (site["name"] == siteName):
      return int(site["lstate"][1:])
  print("Error, can't find siteName in getLinkValue function", file=sys.stderr)
  sys.exit(4)

def getLinkString(agent, siteName):
  for site in agent["interface"]:
    if (site["name"] == siteName):
      return site["lstate"]
  return ""

def excludeSites(agent, sitesToExclude):
  sites = []
  for site in agent["interface"]:
    toBeExcluded = False
    for siteName in sitesToExclude:
      if site["name"] == siteName:
        toBeExcluded = True
        sitesToExclude.remove(siteName)
        break
    if not toBeExcluded:
      sites.append(site["name"])
  return sites

def getBondSteps(agent1, agent2, sites):
  bondSteps = []
  for s in sites:
    lValue = getLinkString(agent2, s)
    if lValue == "":
      bondSteps.append((s,0))
      continue
    if lValue == "!_" or lValue == "?":
      bondSteps.append((s,lValue))
      continue
    prevLValue = getLinkString(agent1, s)
    if prevLValue == "" or prevLValue == "!_" or prevLValue == "?":
      bondSteps.append((s,0))
      continue
    step = int(lValue[1:]) - int(prevLValue[1:])
    bondSteps.append((s,step))
  return bondSteps

def buildChain(first, second, last):
  (prv, nxt) = findBond(first, second)
  restOfSites = excludeSites(second, [prv, nxt])
  bondSteps = getBondSteps(first, second, restOfSites)
  start = getLinkValue(second, nxt)
  step = start - getLinkValue(first, nxt)
  stop = getLinkValue(last, prv)
  chain = [first, second]
  prevLinkValues = {}
  for siteName in restOfSites:
    lString = getLinkString(second,siteName)
    if lString != "" and lString != "!_" and lString != "?":
      prevLinkValues[siteName] = int(lString[1:])
  for i in range(start, stop, step):
    if (i < stop):
      newAgent = copy.deepcopy(second)
      setLinkValue(newAgent["interface"], prv, i)
      setLinkValue(newAgent["interface"], nxt, i + step)
      for (siteName,bondStep) in bondSteps:
        if bondStep == 0:
          setLinkValue(newAgent["interface"], siteName, "")
          continue
        if bondStep == "!_" or bondStep == "?":
          setLinkValue(newAgent["interface"], siteName, bondStep)
          continue
        newLinkValue = int(prevLinkValues[siteName]) + bondStep
        prevLinkValues[siteName] = newLinkValue
        setLinkValue(newAgent["interface"], siteName, newLinkValue)
      chain += [newAgent]
  chain += [last]
  return chain

algExpTokens = ('AENUMBER',
                'AEPI',
                'AEPLUS',
                'AEMINUS',
                'AETIMES',
                'AEDIVIDE',
                'AELPAREN',
                'AERPAREN',
                'AEFUNCTION'
                )

# List of token names.   This is always required
def AlgExpLexer():
  tokens = algExpTokens

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

  # A string containing ignored characters (spaces and tabs)
  #t_ignore  = ' \t'

  # Error handling rule
  #def t_error(t):
  #    print("Illegal character " + t.value[0])
  #    t.lexer.skip(1)

  return lex.lex()
  
def AlgExpParser():
  tokens = algExpTokens

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

def usage():
  print("Usage: prekappa.py prekappa_file", file=sys.stderr)
  sys.exit(0)

def main():
  if (len(sys.argv) == 2):
    filename = sys.argv[1]
  else:
    usage()

  try:
    PKA = open(filename, "r")
  except IOError:
    print("Couldn't open " + filename, file=sys.stderr)
    usage()

  lexer = PkaLexer()
  parser = PkaParser()

  if (0):
    lexer.input(PKA.read())

    while True:
      tok = lexer.token()
      if not tok: break
      print(tok.type, tok.value, tok.lineno, tok.lexpos)
    PKA.seek(0)

  if (1):
    print("# Created by expander.py\n")
    parser.parse(PKA.read(), lexer=lexer)
    #print(result)

aeLexer = AlgExpLexer()
aeParser = AlgExpParser()
main()
