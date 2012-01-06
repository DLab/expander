import sys

output = sys.stdout
nullInstruction = {}

def setOutput(out):
  global output
  output = out

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

def printStr(s="", **printOptions):
  print(s, file=output, **printOptions)

def printError(s):
  print(s, printOptions, file=sys.stderr)

def printLocation(name, dataArray):
  printStr("# defined location " + name + " with data array = " + " ".join(map(str,dataArray)))

def printLocationList(name, llist):
  printStr("# defined location list " + name + " containing: " + ",".join(llist))

def printLocationMatrix(name, lmatrix):
  printStr("# defined location matrix " + name + ":")
  for mrow in lmatrix:
    printStr("#\t" + "\t".join(map(str, mrow)))

def printSignature(agentsignature):
  printStr("%agent: " + agentsignature["name"] + "(", end='')
  sites = []
  for sitesignature in agentsignature["signature"]:
    sites.append(sitesignature["name"] + "".join(sitesignature["istatelist"]))
  printStr(",".join(sites) + ")")

def printRule(rule):
  if (rule != nullInstruction):
    printStr(rule["label"] + " " + expressionToString(rule["lhs"]) + " -> " + expressionToString(rule["rhs"]) + " @ " + rule["rate"])

def printInit(init):
  if (init != nullInstruction):
    e = expressionToString(init["expression"])
    if (len(init["expression"]) > 1):
      e = "(" + e + ")"
    printStr("%init: " + str(init["quantity"]) + " " + e)

def printObs(obs):
  if (obs != nullInstruction):
    printStr("%obs: " + obs["label"] + " " + expressionToString(obs["expression"]))

def printObs2(obs):
  if (obs != nullInstruction):
    printStr("%obs: " + obs["label"] + " " + obs["algexp"])

def printVar(var):
  if (var != nullInstruction):
    printStr("%var: " + var["label"] + " " + expressionToString(var["expression"]))

def printVar2(var):
  if (var != nullInstruction):
    printStr("%var: " + var["label"] + " " + var["algexp"])

def printPlot(plot):
  if (plot != nullInstruction):
    printStr("%plot: " + plot["label"] + " " + expressionToString(plot["expression"]))

def printPlot2(plot):
  if (plot != nullInstruction):
    printStr("%plot: " + plot["label"] + " " + plot["algexp"])

def printMod(mod):
  if (mod != nullInstruction):
    printStr(" ".join(["%mod:",mod["boolexp"],"do",effectToString(mod["effect"])]), end="")
    if (mod["endexp"] != ""):
      printStr(" " + "until" + " " + mod["endexp"], end="")
    printStr()
