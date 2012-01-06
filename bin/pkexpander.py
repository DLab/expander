import sys
import collections
import functools
import copy
import re

import AlgExpLexer
import AlgExpParser
import printing

aeLexer = AlgExpLexer.AlgExpLexer()
aeParser = AlgExpParser.AlgExpParser()

locations = collections.OrderedDict()
locationLists = {}
locationMatrices = {}
signatures = {}
nullInstruction = printing.nullInstruction
options = {}
  
def setOptions(opts):
  global options
  options = opts
  printing.setOutput(options.outFile)

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
  printing.printSignature(sign)

def isBimol(expression):
  def searchLink(linkList, link, i):
    for agent in range(0,len(linkList)):
      if (agent != i):
        for l in linkList[agent]:
          if (link == l):
            return agent
    printing.printError("Error in links of a rule")
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

def joinInAList(e1, e2):
  return (e1,e2)

def solveAlgebraicExpressions(expression):
  for agent in expression:
    for site in agent["interface"]:
      if site["lstate"] != "" and site["lstate"] != "!_" and site["lstate"] != "?":
        site["lstate"] = "!" + str(int(aeParser.parse(site["lstate"][1:],lexer=aeLexer)))

def substituteDataArrayElementInString(vname, s, dataArray):
  for variable in re.findall(vname + "\[\d+\]", s):
    index = int(re.findall("\d+", variable)[0])
    if index < len(dataArray):
      s = s.replace(variable, str(dataArray[index]))
    else:
      printing.printError("Error replacing " + vname + "[" + str(index) + "] variable in a expression", file=sys.stderr)
      sys.exit(2)
  return s

def checkForAlgebraicExpressions(expression, vnames, dataArrays, cellValue):
  for agent in expression:
    for site in agent["interface"]:
      for (vname,dataArray) in map(joinInAList, vnames, dataArrays):
        site["lstate"] = substituteDataArrayElementInString(vname, site["lstate"], dataArray)
        site["istate"] = substituteDataArrayElementInString(vname, site["istate"], dataArray)
      site["lstate"] = site["lstate"].replace("%cell", str(cellValue))
      site["istate"] = site["istate"].replace("%cell", str(cellValue))

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
    newRule["rate"] = substituteDataArrayElementInString("%loc", newRule["rate"], locations[loc])
    newRule["rate"] = newRule["rate"].replace("%loc", loc)
    #if (isBimol(rule["lhs"])):
    #  newRule["rate"] = encloseAndSuffix(newRule["rate"], "/ " + str(locations[loc]))
    return newRule
  
  def mixRuleWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
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
      newRule["rate"] = substituteDataArrayElementInString("%org", newRule["rate"], locations[org])
      newRule["rate"] = substituteDataArrayElementInString("%dst", newRule["rate"], locations[dst])
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
    printing.printRule(r)

def einit(init, domain):
  def mixInitWithLoc(loc):
    newInit = copy.deepcopy(init)
    checkForAlgebraicExpressions(newInit["expression"],["%loc"],[locations[loc]],str(0))
    solveAlgebraicExpressions(newInit["expression"])
    newInit["expression"] = checkForChains(newInit["expression"])
    insertLocInExpression(newInit["expression"], loc)
    newInit["quantity"] = substituteDataArrayElementInString("%loc", newInit["quantity"], locations[loc])
    newInit["quantity"] = str(int(aeParser.parse(newInit["quantity"],lexer=aeLexer)))
    return newInit
  
  def mixInitWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newInit = copy.deepcopy(init)
      checkForAlgebraicExpressions(newInit["expression"],["%org","%dst"],[locations[org],locations[dst]],str(r))
      solveAlgebraicExpressions(newInit["expression"])
      newInit["expression"] = checkForChains(newInit["expression"])
      insertOrgInExpression(newInit["expression"],org)
      insertDstInExpression(newInit["expression"],dst)
      newInit["quantity"] = substituteDataArrayElementInString("%org", newInit["quantity"], locations[org])
      newInit["quantity"] = substituteDataArrayElementInString("%dst", newInit["quantity"], locations[dst])
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
    printing.printInit(i)

def eobs(obs, domain):
  def mixObsWithLoc(loc):
    newObs = copy.deepcopy(obs)
    newObs["label"] = newObs["label"].replace("%loc",loc)
    checkForAlgebraicExpressions(newObs["expression"],["%loc"],[locations[loc]],str(0))
    solveAlgebraicExpressions(newObs["expression"])
    newObs["expression"] = checkForChains(newObs["expression"])
    insertLocInExpression(newObs["expression"],loc)
    return newObs
  
  def mixObsWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newObs = copy.deepcopy(obs)
      newObs["label"] = newObs["label"].replace("%org",org)
      newObs["label"] = newObs["label"].replace("%dst",dst)
      checkForAlgebraicExpressions(newObs["expression"],["%org","%dst"],[locations[org],locations[dst]],str(r))
      solveAlgebraicExpressions(newObs["expression"])
      newObs["expression"] = checkForChains(newObs["expression"])
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
    printing.printObs(r)

def eobs2(obs, domain):
  def mixObsWithLoc(loc):
    newObs = copy.deepcopy(obs)
    newObs["label"] = newObs["label"].replace("%loc",loc)
    newObs["algexp"] = substituteDataArrayElementInString("%loc", newObs["algexp"], locations[loc])
    newObs["algexp"] = newObs["algexp"].replace("%loc",loc)
    return newObs
  
  def mixObsWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newObs = copy.deepcopy(obs)
      newObs["label"] = newObs["label"].replace("%org",org)
      newObs["label"] = newObs["label"].replace("%dst",dst)
      newObs["algexp"] = substituteDataArrayElementInString("%org", newObs["algexp"], locations[org])
      newObs["algexp"] = substituteDataArrayElementInString("%dst", newObs["algexp"], locations[dst])
      newObs["algexp"] = newObs["algexp"].replace("%cell", str(r))
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
    printing.printObs2(r)

def eplot(plot, domain):
  def mixPlotWithLoc(loc):
    newPlot = copy.deepcopy(plot)
    newPlot["label"] = newPlot["label"].replace("%loc",loc)
    checkForAlgebraicExpressions(newPlot["expression"],["%loc"],[locations[loc]],str(0))
    solveAlgebraicExpressions(newPlot["expression"])
    newPlot["expression"] = checkForChains(newPlot["expression"])
    insertLocInExpression(newPlot["expression"],loc)
    return newPlot
  
  def mixPlotWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newPlot = copy.deepcopy(plot)
      newPlot["label"] = newPlot["label"].replace("%org",org)
      newPlot["label"] = newPlot["label"].replace("%dst",dst)
      checkForAlgebraicExpressions(newPlot["expression"],["%org","%dst"],[locations[org],locations[dst]],str(r))
      solveAlgebraicExpressions(newPlot["expression"])
      newPlot["expression"] = checkForChains(newPlot["expression"])
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
    printing.printPlot(r)

def eplot2(plot, domain):
  def mixPlotWithLoc(loc):
    newPlot = copy.deepcopy(plot)
    newPlot["label"] = newPlot["label"].replace("%loc",loc)
    newPlot["algexp"] = substituteDataArrayElementInString("%loc", newPlot["algexp"], locations[loc])
    newPlot["algexp"] = newPlot["algexp"].replace("%loc",loc)
    return newPlot
  
  def mixPlotWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newPlot = copy.deepcopy(plot)
      newPlot["label"] = newPlot["label"].replace("%org",org)
      newPlot["label"] = newPlot["label"].replace("%dst",dst)
      newPlot["algexp"] = substituteDataArrayElementInString("%org", newPlot["algexp"], locations[org])
      newPlot["algexp"] = substituteDataArrayElementInString("%dst", newPlot["algexp"], locations[dst])
      newPlot["algexp"] = newPlot["algexp"].replace("%cell", str(r))
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
    printing.printPlot2(r)

def evar(var, domain):
  def mixVarWithLoc(loc):
    newVar = copy.deepcopy(var)
    newVar["label"] = newVar["label"].replace("%loc",loc)
    checkForAlgebraicExpressions(newVar["expression"],["%loc"],[locations[loc]],str(0))
    solveAlgebraicExpressions(newVar["expression"])
    newVar["expression"] = checkForChains(newVar["expression"])
    insertLocInExpression(newVar["expression"],loc)
    return newVar
  
  def mixVarWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newVar = copy.deepcopy(var)
      newVar["label"] = newVar["label"].replace("%org",org)
      newVar["label"] = newVar["label"].replace("%dst",dst)
      checkForAlgebraicExpressions(newVar["expression"],["%org","%dst"],[locations[org],locations[dst]],str(r))
      solveAlgebraicExpressions(newVar["expression"])
      newVar["expression"] = checkForChains(newVar["expression"])
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
    printing.printVar(r)

def evar2(var, domain):
  def mixVarWithLoc(loc):
    newVar = copy.deepcopy(var)
    newVar["label"] = newVar["label"].replace("%loc",loc)
    newVar["algexp"] = substituteDataArrayElementInString("%loc", newVar["algexp"], locations[loc])
    newVar["algexp"] = newVar["algexp"].replace("%loc",loc)
    return newVar
  
  def mixVarWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newVar = copy.deepcopy(var)
      newVar["label"] = newVar["label"].replace("%org",org)
      newVar["label"] = newVar["label"].replace("%dst",dst)
      newVar["algexp"] = substituteDataArrayElementInString("%org", newVar["algexp"], locations[org])
      newVar["algexp"] = substituteDataArrayElementInString("%dst", newVar["algexp"], locations[dst])
      newVar["algexp"] = newVar["algexp"].replace("%cell", str(r))
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
    printing.printVar2(r)

def emod(mod, domain):
  def mixModWithLoc(loc):
    newMod = copy.deepcopy(mod)
    newMod["boolexp"] = substituteDataArrayElementInString("%loc", newMod["boolexp"], locations[loc])
    newMod["endexp"] = substituteDataArrayElementInString("%loc", newMod["endexp"], locations[loc])
    newMod["boolexp"] = newMod["boolexp"].replace("%loc",loc)
    newMod["endexp"] = newMod["endexp"].replace("%loc",loc)
    insertLocInEffect(newMod["effect"],loc)
    return newMod
  
  def mixModWithOrgDst(matrixCell):
    (org, dst, r) = matrixCell
    if (options.fullMatrix == False and r == 0):
      return nullInstruction
    if (org != dst):
      newMod = copy.deepcopy(mod)
      newMod["boolexp"] = substituteDataArrayElementInString("%org", newMod["boolexp"], locations[org])
      newMod["boolexp"] = substituteDataArrayElementInString("%dst", newMod["boolexp"], locations[dst])
      newMod["boolexp"] = newMod["boolexp"].replace("%cell",str(r))
      newMod["endexp"] = substituteDataArrayElementInString("%org", newMod["endexp"], locations[org])
      newMod["endexp"] = substituteDataArrayElementInString("%dst", newMod["endexp"], locations[dst])
      newMod["endexp"] = newMod["endexp"].replace("%cell",str(r))
      newMod["boolexp"] = newMod["boolexp"].replace("%org",org)
      newMod["boolexp"] = newMod["boolexp"].replace("%dst",dst)
      newMod["endexp"] = newMod["endexp"].replace("%org",org)
      newMod["endexp"] = newMod["endexp"].replace("%dst",dst)
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
    printing.printMod(r)

def expand(domainId, instruction, writer):
  if (domainId in locationLists):
    writer(instruction, locationLists[domainId])
  elif (domainId in locationMatrices):
    writer(instruction, locationMatrices[domainId])
  elif (domainId == "all"):
    writer(instruction, list(locations.keys()))
  else:
    printing.printError("Error, " + domainId + " doesn't exist")
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
      if (l["lstate"] != "" and l["lstate"] != "!_" and l["lstate"] != "?" and l["lstate"] == r["lstate"]):
        return (r["name"], l["name"])
  printing.printError("Error in chain definition!, can't find prev and next sites")
  sys.exit(3)

def getLinkValue(agent, siteName):
  for site in agent["interface"]:
    if (site["name"] == siteName):
      if (site["lstate"] == "" or site["lstate"] == "!_" or site["lstate"] == "?"):
        printing.printError("Error, trying to get link value from a non-integer linking state")
        sys.exit(5)
      return int(site["lstate"][1:])
  printing.printError("Error, can't find siteName in getLinkValue function")
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
