import ply.lex as lex
import ply.yacc as yacc
import sys
import collections
import functools
import copy

locations = collections.OrderedDict()
locationLists = {}
locationMatrices = {}
signatures = {}
nullInstruction = {}

#List of token names required by ply.lex
tokens = ( 'LOC',
           'LOCL',
           'LOCM',
           'ENDMATRIX',
           'ESIGNATURE',
           'SIGNATURE',
           'ERULE',
           'EINIT',
           'INIT',
           'EOBS',
           'OBS',
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
           'LPAREN',
           'RPAREN',
           'QUOTE',
           'LOCFIELD',
           'RELATOR',
           'LOGICOPERATOR',
           'NOT',
           'BOOLEAN',
           'EFFECTKEYWORD',
           'UNTIL',
           'COMMENT',
           'NEWLINE'
           )

def PkaLexer():
  t_ID = r'[A-Za-z0-9][A-Za-z0-9_\-]*'
  t_LABEL = r'\'.+\''
  t_ISTATE = r'~[A-Za-z0-9_]+'
  t_LSTATE = r'!\d+|\?|!_'
  t_COMMA = r','
  t_ELLIPSIS = r'\.\.\.'
  t_AT = r'@'
  t_OPERATOR = r'\[(E|T|log|sin|cos|tan|sqrt|pi|inf|mod|exp|int|\+|-|\*|/|\^)\]'
  t_LPAREN = r'\('
  t_RPAREN = r'\)'
  t_QUOTE = r'\''
  t_LOCFIELD = r'%org|%dst|%loc|%'
  t_RELATOR = r'>|<|='
  t_LOGICOPERATOR = r'&&|\|\|'
  t_COMMENT = r'\#.+'

  def t_NOT(t):
    r'not'
    return t

  def t_BOOLEAN(t):
    r'true|false'
    return t

  def t_EFFECTKEYWORD(t):
    r'\$ADD|\$DEL|\$SNAPSHOT|\$STOP|:='
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

  def t_ENDMATRIX(t):
    r'%endMatrix'
    return t

  def t_ESIGNATURE(t):
    r'%expand-signature:'
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

  def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += 1
    return t

  t_ignore = ' \t'

  def t_error(t):
    print("Illegal character \'", t.value[0], "\'")
    t.lexer.skip(1)

  return lex.lex()

def printSignature(agentsignature):
  print("agent: " + agentsignature["name"] + "(", end='')
  sites = []
  for sitesignature in agentsignature["signature"]:
    sites.append(sitesignature["name"] + "".join(sitesignature["istatelist"]))
  print(",".join(sites) + ")")

def siteToString(site):
  return site["name"] + site["lstate"] + site["istate"]

def agentToString(agent):
  s = agent["name"] + "("
  s += ",".join(map(siteToString, agent["interface"]))
  return s + ")"

def complexToString(complex):
  return ",".join(map(agentToString, complex))

def printRule(rule):
  if (rule != nullInstruction):
    print(rule["label"] + " " + complexToString(rule["lhs"]) + " -> " + complexToString(rule["rhs"]) + " @ " + str(rule["rate"]))

def printInit(init):
  if (init != nullInstruction):
    print("%init: " + str(init["n"]) + " " + agentToString(init["agent"]))

def printObs(obs):
  if (obs != nullInstruction):
    print("%obs: " + obs["label"] + " " + complexToString(obs["complex"]))

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

def isBimol(complex):
  def searchLink(linkList, link, i):
    for agent in range(0,len(linkList)):
      if (agent != i):
        for l in linkList[agent]:
          if (link == l):
            return agent
    print("Error in links of a rule")
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
  for agent in complex:
    linkList.append([])
    for site in agent["interface"]:
      link = site["lstate"]
      if (link != "!?" and link != "!_" and link != ""):
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

def insertLocInComplex(complex, loc):
  for agent in complex:
    insertLocInAgent(agent, loc)

def insertOrgInComplex(complex, org):
  for agent in complex:
    insertOrgInAgent(agent, org)

def insertDstInComplex(complex, dst):
  for agent in complex:
    insertDstInAgent(agent, dst)

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

def erule(rule, domain):
  def makeDiffusions(complex, dst):
    for agent in complex:
      if(interfaceHasSite(agent["interface"], "%")):
        diffuseAgent(agent["interface"], dst)

  def mixRuleWithLoc(loc):
    newRule = copy.deepcopy(rule)
    newRule["label"] = newRule["label"].replace("%loc",loc)
    insertLocInComplex(newRule["lhs"],loc)
    insertLocInComplex(newRule["rhs"],loc)
    if (isBimol(rule["lhs"])):
      newRule["rate"] /= locations[loc]
    return newRule
  
  def mixRuleWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newRule = copy.deepcopy(rule)
      newRule["label"] = newRule["label"].replace("%org",org)
      newRule["label"] = newRule["label"].replace("%dst",dst)
      insertOrgInComplex(newRule["lhs"],org)
      insertOrgInComplex(newRule["rhs"],org)
      insertDstInComplex(newRule["lhs"],dst)
      insertDstInComplex(newRule["rhs"],dst)
      newRule["rate"] *= r
#Is the following necessary?
      if (isBimol(rule["lhs"])):
        newRule["rate"] /= locations[org]
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
    insertLocInAgent(newInit["agent"], loc)
    newInit["n"] *= locations[loc]
    return newInit
  
  def mixInitWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newInit = copy.deepcopy(init)
      insertOrgInAgent(newInit["agent"],org)
      insertDstInAgent(newInit["agent"],dst)
      newInit["n"] *= r
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
    insertLocInComplex(newObs["complex"],loc)
    return newObs
  
  def mixObsWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (org != dst):
      newObs = copy.deepcopy(obs)
      newObs["label"] = newObs["label"].replace("%org",org)
      newObs["label"] = newObs["label"].replace("%dst",dst)
      insertOrgInComplex(newObs["complex"],org)
      insertDstInComplex(newObs["complex"],dst)
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

def expand(domainId, expression, writer):
  if (domainId in locationLists):
    writer(expression, locationLists[domainId])
  elif (domainId in locationMatrices):
    writer(expression, locationMatrices[domainId])
  elif (domainId == "all"):
    writer(expression, list(locations.keys()))
  else:
    print("Error, " + domainId + " doesn't exist")
    sys.exit(2)

def setLinkValue(interface, siteName, intValue):
  for site in interface:
    if (site["name"] == siteName):
      site["lstate"] = "!" + str(intValue)
      break

def findBond(left, right):
  for l in left["interface"]:
    for r in right["interface"]:
      if (l["lstate"] == r["lstate"]):
        return (r["name"], l["name"])
  print("Error in chain definition!, can't find prev and next sites")
  sys.exit(3)

def getLinkValue(agent, siteName):
  for site in agent["interface"]:
    if (site["name"] == siteName):
      return int(site["lstate"][1:])
  print("Error, can't find siteName in getLinkValue function")
  sys.exit(4)

def buildChain(first, second, last):
  (prv, nxt) = findBond(first, second)
  start = getLinkValue(second, nxt)
  step = start - getLinkValue(first, nxt)
  stop = getLinkValue(last, prv)
  chain = [first, second]
  for i in range(start, stop, step):
    if (i < stop):
      newAgent = copy.deepcopy(second)
      setLinkValue(newAgent["interface"], prv, i)
      setLinkValue(newAgent["interface"], nxt, i + step)
      chain += [newAgent]
  chain += [last]
  return chain

def PkaParser():  
  def p_prekappa(p):
    '''prekappa : prekappa instruction
                | prekappa info
                | instruction
                | info'''
    pass;

  def p_instruction_newline(p): #get rid of the newline between every pair of instructions
    'instruction : instruction newlines'
    if(p[2] > 1):
      print()
    pass;

  def p_newlines(p):
    'newlines : newlines NEWLINE'
    p[0] = p[1] + 1

  def p_newlines_begin(p):
    'newlines : NEWLINE'
    p[0] = 1

  def p_instruction(p):
    '''instruction : esignature
                   | signature
                   | erule
                   | rule
                   | einit
                   | init
                   | eobs'''
    pass;

  def p_info_newline(p):
    'info : info NEWLINE'
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
    'loc : LOC ID NUMBER'
    global locations
    locations[p[2]] = float(p[3])

  def p_locl(p):
    'locl : LOCL idlist'
    global locationLists
    locationLists[p[2][0]] = p[2][1:]

  def p_idlist(p):
    'idlist : idlist ID'
    p[0] = p[1] + [p[2]]

  def p_idlist_begin(p):
    'idlist : ID'
    p[0] = [p[1]]

  def p_locm(p):
    'locm : initmatrix idlist NEWLINE mrows ENDMATRIX'
    global locationMatrices
    mr = p[4]
    matrixId = p[2][0]
    destList = p[2][1:]
    tupleList = []
    for i in range(0,len(mr)):
      org = mr[i][0]
      j = 1
      for dst in destList:
        tupleList.append((org,dst,mr[i][j]))
        j += 1
    locationMatrices[matrixId] = tupleList

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
    p[0] = [p[1]] + p[2]

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
    print("#expanding in " + p[2], end=" ")
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
    print("#expanding in " + p[2], end=" ")
    printRule(p[4])
    expand(p[2], p[4], erule)
  
  def p_erule_no_label(p):
    'erule : ERULE ID reaction'
    p[4]["label"] = ""
    expand(p[2], p[3], erule)

  def p_reaction(p):
    'reaction : complex RARROW complex AT NUMBER'
    reaction = {}
    reaction["lhs"] = p[1]
    reaction["rhs"] = p[3]
    reaction["rate"] = float(p[5])
    p[0] = reaction

  def p_introduction(p):
    'reaction : RARROW complex AT NUMBER'
    reaction = {}
    reaction["lhs"] = []
    reaction["rhs"] = p[2]
    reaction["rate"] = p[4]
    p[0] = reaction
  
  def p_deletion(p):
    'reaction : complex RARROW AT NUMBER'
    reaction = {}
    reaction["lhs"] = p[1]
    reaction["rhs"] = []
    reaction["rate"] = p[4]
    p[0] = reaction

  def p_chain(p):
    'chain : agent COMMA agent COMMA ELLIPSIS COMMA agent'
    p[0] = buildChain(p[1], p[3], p[7])

  def p_complex_chain(p):
    'complex : complex COMMA chain'
    p[0] = p[1] + p[3]
  
  def p_complex_complex(p):
    'complex : complex COMMA complex'
    p[0] = p[1] + p[3]

  def p_complex_begin(p):
    'complex : agent'
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
    'site : LOCFIELD'
    site = {}
    site["name"] = p[1]
    site["istate"] = ""
    site["lstate"] = ""
    p[0] = site

  def p_site_lstate(p):
    'site : ID LSTATE'
    site = {}
    site["name"] = p[1]
    site["istate"] = ""
    site["lstate"] = p[2]
    p[0] = site

  def p_site_istate_lstate(p):
    'site : ID ISTATE LSTATE'
    site = {}
    site["name"] = p[1]
    site["istate"] = p[2][1:]
    site["lstate"] = p[3][1:]
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
    'einit : EINIT ID NUMBER agent'
    init = {}
    init["n"] = float(p[3])
    init["agent"] = p[4]
    print("#expanding in " + p[2], end=" ")
    printInit(init)
    expand(p[2], init, einit)

  def p_init(p):
    'init : INIT NUMBER agent'
    init = {}
    init["n"] = float(p[2])
    init["agent"] = p[3]
    printInit(init)

  def p_eobs(p):
    'eobs : EOBS ID LABEL complex'
    obs = {}
    obs["label"] = p[3]
    obs["complex"] = p[4]
    print("#expanding in " + p[2], end=" ")
    printObs(obs)
    expand(p[2], obs, eobs)
  
# Error rule for syntax errors
  def p_error(p):
    print("Syntax error in input! " + str(p))
    
  return yacc.yacc()

def usage():
  print("Usage: prekappa.py prekappa_file")
  sys.exit(0)

def main():
  if (len(sys.argv) == 2):
    filename = sys.argv[1]
  else:
    usage()

  try:
    PKA = open(filename, "r")
  except IOError:
    print("Couldn't open " + filename)
    usage()

  lexer = PkaLexer()
  parser = PkaParser()

  if (0):
    lexer.input(PKA.read())

    while True:
      tok = lexer.token()
      if not tok: break
      print(tok.type, tok.value, tok.lineno, tok.lexpos)

  if (1):
    parser.parse(PKA.read())
    #print(result)

main()
