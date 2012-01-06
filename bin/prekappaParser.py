import ply.yacc as yacc

import pkexpander
import expanderTokens
import printing

locations = pkexpander.locations
locationLists = pkexpander.locationLists
locationMatrices = pkexpander.locationMatrices
signatures = pkexpander.signatures

# Error rule for syntax errors
def p_error(p):
  printing.printError("Syntax error in input! " + str(p))
  sys.exit(1)
  
def PkaParser(options):

  pkexpander.setOptions(options)
  printing.setOutput(options.outFile)

  tokens = expanderTokens.pkaTokens
  tokens.remove('BACKSLASHNEWLINE')

  precedence = (
    ('left', 'COMMA'),
    ('right', 'ID'),
    ('right', 'DO', 'UNTIL'),
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
      printing.printStr()
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
      printing.printStr()
    pass;

  def p_info(p):
    '''info : comment
            | loc
            | locl
            | locm'''
    pass;

  def p_comment(p):
    'comment : COMMENT'
    printing.printStr(p[1])

  def p_loc(p):
    'loc : LOC ID numberlist'
    global locations
    if locations.__contains__(p[2]):
      sys.stderr.write("Warning!, location " + p[2] + " redefined\n")
    locations[p[2]] = []
    for i in p[3]:
      locations[p[2]].append(i)
    printing.printLocation(p[2], p[3])

  def p_locl(p):
    'locl : LOCL idlist'
    global locationLists
    locationLists[p[2][0]] = p[2][1:]
    printing.printLocationList(p[2][0], p[2][1:])

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
    printing.printLocationMatrix(matrixId, [p[2]] + newMR)
    if (mr[i]["nlines"] > 1):
      printing.printStr()

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
    'numberlist : numberlist number'
    p[0] = p[1] + [(p[2])]

  def p_numberlist_begin(p):
    'numberlist : number'
    p[0] = [(p[1])]

  def p_number_integer(p):
    'number : INTEGER'
    p[0] = int(p[1])
  
  def p_number_float(p):
    'number : FLOAT'
    p[0] = float(p[1])

  def p_esignature(p):
    'esignature : ESIGNATURE ID agentsignature'
    global signatures
    signatures[p[3]["name"]] = p[3]["signature"]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printSignature(p[3])
    pkexpander.expand(p[2], p[3], pkexpander.esignature)

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
    printing.printSignature(p[2])

  def p_erule(p):
    'erule : ERULE ID LABEL reaction'
    p[4]["label"] = p[3]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printRule(p[4])
    pkexpander.expand(p[2], p[4], pkexpander.erule)
  
  def p_erule_no_label(p):
    'erule : ERULE ID reaction'
    p[4]["label"] = ""
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printRule(p[3])
    pkexpander.expand(p[2], p[3], pkexpander.erule)

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
    '''algexp : INTEGER
              | FLOAT
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
    'site : ID istate'
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
  
  def p_istate(p):
    'istate : ISTATE'
    p[0] = p[1]

  def p_istate_algexp(p):
    'istate : TILDE algexp'
    p[0] = p[1] + p[2]

  def p_site_lstate(p):
    'site : ID lstate'
    site = {}
    site["name"] = p[1]
    site["istate"] = ""
    site["lstate"] = p[2]
    p[0] = site

  def p_site_istate_lstate(p):
    'site : ID istate lstate'
    site = {}
    site["name"] = p[1]
    site["istate"] = p[2]
    site["lstate"] = p[3]
    p[0] = site
  
  def p_site_lstate_istate(p):
    'site : ID lstate istate'
    site = {}
    site["name"] = p[1]
    site["lstate"] = p[2]
    site["istate"] = p[3]
    p[0] = site
  
  def p_rule(p):
    'rule : LABEL reaction'
    p[2]["label"] = p[1]
    reaction = p[2]
    reaction["lhs"] = pkexpander.checkForChains(reaction["lhs"])
    reaction["rhs"] = pkexpander.checkForChains(reaction["rhs"])
    printing.printRule(p[2])

  def p_rule_no_label(p):
    'rule : reaction'
    p[1]["label"] = ""
    printing.printRule(p[1])
  
  def p_einit(p):
    'einit : EINIT ID algexp expression'
    init = {}
    init["quantity"] = p[3]
    init["expression"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printInit(init)
    pkexpander.expand(p[2], init, pkexpander.einit)

  def p_init(p):
    'init : INIT number expression'
    init = {}
    init["quantity"] = p[2]
    init["expression"] = p[3]
    init["expression"] = pkexpander.checkForChains(init["expression"])
    printing.printInit(init)

  def p_eobs(p):
    'eobs : EOBS ID LABEL expression'
    obs = {}
    obs["label"] = p[3]
    obs["expression"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printObs(obs)
    pkexpander.expand(p[2], obs, pkexpander.eobs)

  def p_eobs_algexp(p):
    'eobs : EOBS ID LABEL algexp'
    obs = {}
    obs["label"] = p[3]
    obs["algexp"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printObs2(obs)
    pkexpander.expand(p[2], obs, pkexpander.eobs2)

  def p_obs(p):
    'obs : OBS LABEL expression'
    obs = {}
    obs["label"] = p[2]
    obs["expression"] = p[3]
    obs["expression"] = pkexpander.checkForChains(obs["expression"])
    printing.printObs(obs)

  def p_obs_algexp(p):
    'obs : OBS LABEL algexp'
    obs = {}
    obs["label"] = p[2]
    obs["algexp"] = p[3]
    printing.printObs2(obs)
  
  def p_eplot(p):
    'eplot : EPLOT ID LABEL expression'
    plot = {}
    plot["label"] = p[3]
    plot["expression"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printPlot(plot)
    pkexpander.expand(p[2], plot, pkexpander.eplot)

  def p_eplot_algexp(p):
    'eplot : EPLOT ID LABEL algexp'
    plot = {}
    plot["label"] = p[3]
    plot["algexp"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printPlot2(plot)
    pkexpander.expand(p[2], plot, pkexpander.eplot2)

  def p_plot(p):
    'plot : PLOT LABEL expression'
    plot = {}
    plot["label"] = p[2]
    plot["expression"] = p[3]
    plot["expression"] = pkexpander.checkForChains(plot["expression"])
    printing.printPlot(plot)

  def p_plot_algexp(p):
    'plot : PLOT LABEL algexp'
    plot = {}
    plot["label"] = p[2]
    plot["algexp"] = p[3]
    printing.printPlot2(plot)
  
  def p_evar(p):
    'evar : EVAR ID LABEL expression'
    var = {}
    var["label"] = p[3]
    var["expression"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printVar(var)
    pkexpander.expand(p[2], var, pkexpander.evar)

  def p_evar_algexp(p):
    'evar : EVAR ID LABEL algexp'
    var = {}
    var["label"] = p[3]
    var["algexp"] = p[4]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printVar2(var)
    pkexpander.expand(p[2], var, pkexpander.evar2)

  def p_var(p):
    'var : VAR LABEL expression'
    var = {}
    var["label"] = p[2]
    var["expression"] = p[3]
    var["expression"] = pkexpander.checkForChains(var["expression"])
    printing.printVar(var)

  def p_var_algexp(p):
    'var : VAR LABEL algexp'
    var = {}
    var["label"] = p[2]
    var["algexp"] = p[3]
    printing.printVar2(var)

  def p_emod(p):
    'emod : EMOD ID boolexp DO effect UNTIL boolexp'
    mod = {}
    mod["boolexp"] = p[3]
    mod["effect"] = p[5]
    mod["endexp"] = p[7]
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printMod(mod)
    pkexpander.expand(p[2], mod, pkexpander.emod)
  
  def p_emod_noend(p):
    'emod : EMOD ID boolexp DO effect'
    mod = {}
    mod["boolexp"] = p[3]
    mod["effect"] = p[5]
    mod["endexp"] = ""
    printing.printStr("# expanding in " + p[2], end=" ")
    printing.printMod(mod)
    pkexpander.expand(p[2], mod, pkexpander.emod)

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

  def p_mod_noend(p):
    'mod : MOD boolexp DO effect'
    mod = {}
    mod["boolexp"] = p[2]
    mod["effect"] = p[4]
    mod["effect"]["expression"] = pkexpander.checkForChains(mod["effect"]["expression"])
    mod["endexp"] = ""
    printing.printMod(mod)

  def p_mod(p):
    'mod : MOD boolexp DO effect UNTIL boolexp'
    mod = {}
    mod["boolexp"] = p[2]
    mod["effect"] = p[4]
    mod["effect"]["expression"] = pkexpander.checkForChains(mod["effect"]["expression"])
    mod["endexp"] = p[6]
    printing.printMod(mod)
  
  return yacc.yacc()
