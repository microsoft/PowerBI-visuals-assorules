# Copyright (c) Microsoft Corporation.  All rights reserved.

# Third Party Programs. This software enables you to obtain software applications from other sources. 
# Those applications are offered and distributed by third parties under their own license terms.
# Microsoft is not developing, distributing or licensing those applications to you, but instead, 
# as a convenience, enables you to use this software to obtain those applications directly from 
# the application providers.
# By using the software, you acknowledge and agree that you are obtaining the applications directly
# from the third party providers and under separate license terms, and that it is your responsibility to locate, 
# understand and comply with those license terms.
# Microsoft grants you no license rights for third-party software or applications that is obtained using this software.

##PBI_R_VISUAL: VIZGAL_ASSORULES  Analyzing transaction data and visualize discovered assosiation rules  
# Based on arules package, supports several types of visualization (graph, table, parrallel coordinates plot)
# INPUT: 
# The input dataset should include at least one ID-key column (unique values per row)
# At least one column used for Left Hand Side (LHS) items, at least one column for Right Hand Side (RHS) items
# In current implementation by default only the last column is used for RHS, but it can be changed via parameters settings 
# EXAMPLES:
# for R environment
# data(Titanic)# and repeat ech row by frequency
# Titanic1 = as.data.frame(Titanic)
# Titanic2 <- data.frame(Titanic1[rep(c(1:nrow(Titanic1)), Titanic1$Freq), ])
# Titanic2$Freq = NULL
# dataset = cbind(ID = 1:nrow(Titanic2), Titanic2)
#  source("visGal_assoRules.R") #create graphics
#
# WARNINGS:  Time consuming for large datasets
#
# CREATION DATE: 04/01/2016
#
# LAST UPDATE: 04/08/2016
#
# VERSION: 0.0.1
#
# R VERSION TESTED: 3.2.2
# 
# AUTHOR: B. Efraty (boefraty@microsoft.com)
#
# REFERENCES: https://cran.r-project.org/web/packages/arules/arules.pdf
# https://en.wikipedia.org/wiki/Association_rule_learning

fileRda = "C:/Users/boefraty/projects/PBI/R/tempData.Rda"
if(file.exists(dirname(fileRda)))
{
  if(Sys.getenv("RSTUDIO")!="")
    load(file= fileRda)
  else
    save(list = ls(all.names = TRUE), file=fileRda)
}

waitForData = FALSE # if waitForData == TRUE show empty plot 
if(!exists("dataset") && !(exists("BOTH") || (exists("LHS") && exists("RHS"))) )
  waitForData = TRUE

############ User Parameters #########

##PBI_PARAM: Should warnings text be displayed?
#Type:logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
showWarnings = TRUE 
if(exists("settings_additional_params_showWarnings"))
  showWarnings = settings_additional_params_showWarnings

##PBI_PARAM: a numeric value for the minimal support of an item set
#Type: numeric, Default:0.01, Range:[0, 1], PossibleValues:NA, Remarks: NA
threshSupport = 0.01
if(exists("settings_thresholds_params_threshSupport"))
  threshSupport = as.numeric(settings_thresholds_params_threshSupport)

threshSupport = min(1,max(threshSupport,0))


##PBI_PARAM: minimum acceptable confidence for rule 
#Type: numeric, Default:0.6, Range:[0, 1], PossibleValues:NA, Remarks: NA
threshConfidence = 0.6
if(exists("settings_thresholds_params_threshConfidence"))
  threshConfidence = as.numeric(settings_thresholds_params_threshConfidence)

threshConfidence = min(1,max(threshConfidence,0))

##PBI_PARAM: minimum acceptable lift measure for rule 
#Type: numeric, Default:1.1, Range:[0, 10], PossibleValues:NA, Remarks: values larger than 1 are recommended
threshLift = 1.05
if(exists("settings_thresholds_params_threshLift"))
  threshLift = as.numeric(settings_thresholds_params_threshLift)

threshLift = min(1000000,max(threshLift,0))


##PBI_PARAM: an integer value for the minimal number of items per item set
#Type: numeric, Default:2, Range:[1, 10], PossibleValues:NA, Remarks: NA
minRuleLength = 2
if(exists("settings_thresholds_params_minRuleLength"))
  minRuleLength = as.numeric(settings_thresholds_params_minRuleLength)

minRuleLength = min(12,max(minRuleLength,2))


##PBI_PARAM: an integer value for the maximal number of items per item set
#Type: numeric, Default:8, Range:[1, 20], PossibleValues:NA, Remarks: Should be larger than minRuleLength, have gross impact on running time
maxRuleLength = 8
if(exists("settings_thresholds_params_maxRuleLength"))
  maxRuleLength = as.numeric(settings_thresholds_params_maxRuleLength)

maxRuleLength = min(12,max(minRuleLength, maxRuleLength))

showFrom  =  1
if(exists("settings_rules_params_showFrom"))
  showFrom = as.numeric(settings_rules_params_showFrom)

##PBI_PARAM: an integer value for the maximal number of output rules
#Type: integer, Default:20, Range:[1, 100], PossibleValues:NA, Remarks: Large number of rules are less aesthetically pleasing for visual 
maxRules  =  5
if(exists("settings_rules_params_showTo"))
  maxRules = as.numeric(settings_rules_params_showTo)

showTo = maxRules
showTo = max(showTo,showFrom)# can not be smaller

##PBI_PARAM: sort and select the final set of rules by selected measure
#Type: string , Default:"lift", Range:NA, PossibleValues:"lift", "support", "confidence", Remarks: NA
sortRulesBy  =  "lift" 
if(exists("settings_rules_params_sortBy"))
  sortRulesBy = settings_rules_params_sortBy

##PBI_PARAM: visualization method
# Type: string , Default:"graph", Range:NA, PossibleValues:"paracoord"  "table", "graph", "scatter"
# Remarks: Some methods are less convenient for few rules
visualisationMethod  =  "graph" 
if(exists("settings_viz_params_visualisationMethod"))
  visualisationMethod =  settings_viz_params_visualisationMethod#c("graph","paracoord" ,"table", "scatter")[settings_visualisationMethod]

##PBI_PARAM: minimum number of rules per subplot used if visualisationMethod  =  "graph" 
#Type: integer , Default:2, Range:[1, 100], PossibleValues:NA, Remarks: NA
rulesPerGraphPlate  =  1
if(exists("settings_viz_params_rulesPerPlate"))
{
  rulesPerGraphPlate =  as.numeric(settings_viz_params_rulesPerPlate)
  if(is.na(rulesPerGraphPlate))
    rulesPerGraphPlate = 1
}

##PBI_PARAM: color of edges for LHS items for visualisationMethod  =  "graph" 
#Type: color , Default:"green", Range:NA, PossibleValues:"red", "green", "blue", "cyan", Remarks: see colors {grDevices} command in R
edgeColLHS = "green"
if(exists("settings_viz_params_edgeColLHS"))
  edgeColLHS =  settings_viz_params_edgeColLHS

##PBI_PARAM: color of edges for RHS items for visualisationMethod  =  "graph" 
#Type: color , Default:"orange", Range:NA, PossibleValues:"red", "green", "blue", "cyan", Remarks: see colors {grDevices} command in R
edgeColRHS = "orange"
if(exists("settings_viz_params_edgeColRHS"))
  edgeColRHS =  settings_viz_params_edgeColRHS

##PBI_PARAM: font size of labels for visualisationMethod  =  "graph" 
#Type: numeric , Default:1, Range:[0, 5], PossibleValues:NA, Remarks: NA
fontSizeGraph = 1
if(exists("settings_viz_params_textSize"))
  fontSizeGraph =  settings_viz_params_textSize/10

##PBI_PARAM: colors used if visualisationMethod  =  "paracoord" are scaled from gray to green accirding to one of the measures 
#Type: string , Default:"lift", Range:NA, PossibleValues:"lift", "support", "confidence", Remarks: NA
parCoordColorBy = "lift"
if(exists("settings_viz_params_colorBy"))
  parCoordColorBy =  settings_viz_params_colorBy

###############Library Declarations###############

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***",sep=""))
}

libraryRequireInstall("arules")
libraryRequireInstall("arulesViz")
libraryRequireInstall("grDevices")
libraryRequireInstall("gridExtra")
libraryRequireInstall("grid")
libraryRequireInstall("methods")

###############Internal parameters definitions#################

##PBI_PARAM: filter redundant rules by selected measure
#Type: string , Default:"lift", Range:NA, PossibleValues:"lift", "confidence", Remarks: rule is redundant if it is less predictive than a more general rule 
filterRedundantBy =  "lift"

##PBI_PARAM: maximum acceptable support for rule 
#Type: numeric, Default:1, Range:[0, 1], PossibleValues:NA, Remarks: NA
upperThreshSupport = 1


##PBI_PARAM: maximum acceptable confidence for rule 
#Type: numeric, Default:1, Range:[0, 1], PossibleValues:NA, Remarks: NA
upperThreshConfidence = 1

##PBI_PARAM: maximum acceptable confidence for rule 
#Type: positive numeric, Default:Inf, Range:[0, Inf], PossibleValues:NA, Remarks: NA
upperThreshLift = Inf



##PBI_PARAM: minimum number of transactions (rows) to run  
#Type: integer , Default:10, Range:[10, 100], PossibleValues:NA, Remarks: NA
minTransactions  =  10

###############Internal functions definitions#################

#remove corrupt rows, change nominal variables to factors, remove variable columns
# attempt to replace numeric column with few unique values to factor 
prepareData = function(data, onlyColumns = FALSE) 
{
  if(onlyColumns == FALSE)
    data <- data[complete.cases(data), ] #remove incomplete rows
  columns2remove  <-  NULL
  nc <- ncol(data)
  nr <- nrow(data)
  for (c1 in 1:nc)
  {
    #nominal to factor
    if(is.character(data[, c1]))
    {
      data[, c1]  <-  as.factor(data[, c1])
    }else{
      #few unique to factor
      uLen = length(unique(data[, c1]))
      if(uLen<nr && uLen>1)  
      {
        data[, c1]  <-  as.factor(data[, c1])
      }else{# to remove
        columns2remove = c(columns2remove, c1)
      }
    }
  }
  if(!is.null(columns2remove))
    data = data[, -columns2remove]
  return(as.data.frame(data))
}

#predict columns for LHS by default (all but last)
columnsForLHS <- function(data)  return(seq(1, length.out = ncol(data)-1))

#predict columns for RHS by default (only last)
columnsForRHS <- function(data)  return(ncol(data))

#predict columns for both RHS and LHS by default (NULL)
columnsForBoth <- function(data)  return(NULL)

#convert transaction data to appearance list
ConstructAppearanceList <- function(varLHS = NULL, varRHS = NULL, varBoth = NULL, transData)
{
  mapLabels2Variables = transData@itemInfo
  iii = mapLabels2Variables$variables %in% varLHS
  lhs = mapLabels2Variables$labels[iii]
  
  iii = mapLabels2Variables$variables %in% varRHS
  rhs = mapLabels2Variables$labels[iii]
  
  iii = mapLabels2Variables$variables %in% varBoth
  both = mapLabels2Variables$labels[iii]
  
  appearance = list(lhs = lhs, rhs = rhs, both = both, default = 'none')
}

#partition rules into subplots
getGridPartition <- function(numRules, rulesPerGraphPlate)
{
  numPlates = max(1, floor(numRules/rulesPerGraphPlate))
  numCols <- round(sqrt(numPlates))
  numRows <- max(1, floor(numPlates/numCols))
  numPlates = numRows*numCols
  #partition all rules into plates
  rulesPerGraphPlate = floor(numRules/numPlates)
  partit <- rep(rulesPerGraphPlate, numPlates)
  #last subplot takes the rest 
  partit[numPlates] = partit[numPlates]+numRules-sum(partit)
  return(list(partit = partit, numCols = numCols, numRows = numRows))
}

#scale colors from gray to green by values (of measure), small  = > gray, large  = > green
scaleColors <- function(values, col1 = c(0.5, 0.5, 0.5), col2 = c(0, 1, 0))
{
  NV = length(values)
  values = (values-min(values)+1e-10)/(max(values)-min(values)+1e-10)
  colo <- function(a) rgb(t((col1*(1-a)+col2*(a))))
  return(sapply(values, colo))
}

#clear redandant rules 
cleanRedundant <- function(rules, maxRules2process = 3e+06, measure = "lift", meaningfulDig = 4)
{
  rules@quality  =  round(rules@quality, digits = meaningfulDig)
  rules  <-  sort(rules, by = "lift")
  
  if(length(rules)<3)
    return(rules)
  
  if(length(rules)>maxRules2process)
    rules <- rules[1:maxRules2process]
  
  r <- is.redundant(rules, measure  =  measure)
  if(is.redundant(rules[1], measure  =  measure))
    r<-!r
  rules <- rules[!r]#fast but not accurate
  
  
  if(length(rules)<3)
    return(rules)
  
  subsetMat  <-  is.subset(rules)
  
  
  subsetMat[lower.tri(subsetMat, diag = T)]  <-  FALSE
  
  if(length(subsetMat)<=3)
    return(rules)
  
  redundant  <-  which(colSums(subsetMat, na.rm = T) >=  1)
  toRemove = NULL
  for (r1 in redundant)
    for (r2 in which(subsetMat[, r1]))
      if((rules@quality[measure])[r2, 1] == (rules@quality[measure])[r1, 1])
        toRemove = c(toRemove, r1)
  
  if(!is.null(toRemove))
    rules  =  rules[-toRemove]
  
  return(rules)
}
###############Upfront input correctness validations (where possible)#################

inputType = 1 # (1) LHS&RHS or (2)ID&Item

##PBI_PARAM: array of integer indexes for columns used in left hand side of the rule (LHS)
#Type: array of integers or NA, Default:NA, Range:NA, PossibleValues:NA, Remarks: If NA is used all the columns except for the right-most are used 
if(!exists("columnsLHS"))
  columnsLHS = NA

##PBI_PARAM: array of integer indexes for columns used in right hand side of the rule (RHS)
#Type: array of integers or NA, Default:NA, Range:NA, PossibleValues:NA, Remarks: If NA is used, the right-most column is selected 
if(!exists("columnsRHS"))
  columnsRHS = NA

##PBI_PARAM: array of integer indexes for columns used both in RHS and LHS
#Type: array of integers or NA or NULL, Default:NA, Range:NA, PossibleValues:NA, Remarks: If NA is used, no columns are selected
if(!exists("columnsBoth"))
  columnsBoth = NA




pbiWarning <- NULL

if(waitForData==FALSE && (exists("LHS") || exists("RHS") || exists("BOTH")))
{
  if(!exists("RHS"))
    RHS = data.frame()
  if(!exists("LHS"))
    LHS = data.frame()
  if(!exists("BOTH"))
    BOTH = data.frame()
  
  #exclude duplicates
  namesInLHSnRHSnotBOTH= setdiff(intersect(names(LHS),names(RHS)),names(BOTH)) # copy to BOTH
  if(length(namesInLHSnRHSnotBOTH)>0)
  {
    nbo = c(names(BOTH),namesInLHSnRHSnotBOTH)
    BOTH = cbind(BOTH,LHS[,namesInLHSnRHSnotBOTH])
    names(BOTH) = nbo
  }
  namesInLHSnBOTH= intersect(names(LHS),names(BOTH)) #exclude from LHS
  if(length(namesInLHSnBOTH)>0)
    LHS=as.data.frame(LHS[,setdiff(names(LHS),namesInLHSnBOTH)])
  
  namesInRHSnBOTH= intersect(names(RHS),names(BOTH)) #exclude from LHS
  if(length(namesInRHSnBOTH)>0)
    RHS=as.data.frame(RHS[,setdiff(names(RHS),namesInRHSnBOTH)])
  
  
  columnsLHS = columnsRHS = columnsBoth = NULL
  
  NR = max(nrow(LHS),nrow(RHS),nrow(BOTH))
  if(!ncol(RHS))
    RHS = data.frame(row.names = 1:NR)
  if(!ncol(LHS))
    LHS = data.frame(row.names = 1:NR)
  if(!ncol(BOTH))
    BOTH = data.frame(row.names = 1:NR)
  
  
  if(ncol(LHS))
  {
    LHS = prepareData(LHS, onlyColumns = TRUE)
    columnsLHS <- seq(1,length.out = ncol(LHS))
  }
  inLeft = length(columnsLHS)
  
  if(ncol(BOTH))
  {
    BOTH = prepareData(BOTH, onlyColumns = TRUE)
    columnsBoth <- seq(inLeft+1,length.out = ncol(BOTH))
  }
  inBoth = length(columnsBoth)
  
  if(ncol(RHS))
  {
    RHS = prepareData(RHS, onlyColumns = TRUE)
    columnsRHS <- seq(inLeft+inBoth+1,length.out = ncol(RHS))
  }
  inRHS = length(columnsRHS)
  if(nrow(LHS) > 1)
    dataset = cbind(LHS,BOTH,RHS)
  
}

if(!exists("dataset") || is.null(dataset))
{
  if((exists("TransactionID") && exists("Item")))
  {
    dataset = cbind(TransactionID, Item)
    dataset <- dataset[complete.cases(dataset), ] #remove incomplete rows
    if(nrow(dataset)>1)
      inputType = 2
  }
}


if(inputType == 1)
{
  if(exists("dataset") && !is.null(dataset))
  {
    dataset = prepareData(dataset)
  } else {
    dataset = NULL
  }
  
  if(is.null(dataset) || ncol(dataset) < 2)
    columnsLHS = columnsRHS = columnsBoth = NULL;
  
  #LHS, RHS
  if(!is.null(columnsLHS) && is.na(columnsLHS))
    columnsLHS <- columnsForLHS(dataset)
  
  if(!is.null(columnsRHS) &&is.na(columnsRHS))
    columnsRHS <- columnsForRHS(dataset)
  
  if(!is.null(columnsBoth) &&is.na(columnsBoth))
    columnsBoth <- columnsForBoth(dataset)
  
  #check if LHS and RHS non empty
  if(length(columnsLHS) + length(columnsBoth) < 1 || length(columnsRHS)+length(columnsBoth) < 1 || waitForData == TRUE )
  {
    visualisationMethod = "empty"
    pbiWarning <- paste(pbiWarning, "Both LHS and RHS should not be empty", sep = "\n")
    
    #check if minRuleLength<= maxRuleLength
  }
}



if(visualisationMethod != "empty")
{
  if(minRuleLength>maxRuleLength )
  {
    visualisationMethod = "empty"
    pbiWarning <- paste(pbiWarning, "maxRuleLength needs to be >=  minRuleLength", sep = "\n")
  } else if(nrow(dataset)<minTransactions)
  {
    visualisationMethod = "empty"
    pbiWarning <- paste(pbiWarning, "Not enough rows to perform analysis", sep = "\n")
  }
}


##############Main Visualization script###########


rules = NULL

if(visualisationMethod!= "empty")
{
  randSeed = 42
  set.seed(randSeed)
  
  if(inputType == 1)
  {
    #convert dataset to transactions 
    transData = as(dataset, "transactions")
    
    #add names of columns for lhs, rhs 
    appearance = ConstructAppearanceList(colnames(dataset)[columnsLHS], colnames(dataset)[columnsRHS], colnames(dataset)[columnsBoth], transData)
  }
  else # inputType ==2 
  {
    nnn = names(dataset)
    #specific for Your  data type
    transData <- as(split(dataset[,nnn[2]], dataset[,nnn[1]]), "transactions")
    appearance = NULL
  }
  
  
  # find association rules with default settings
  rules  <-  apriori(transData, 
                     parameter = list(minlen = minRuleLength, maxlen = maxRuleLength, supp = threshSupport, confidence = threshConfidence, target = "rules"), 
                     appearance = appearance, 
                     control  =  list(verbose = F))
  
  #removeRules by upperThresholds
  if(!is.null(rules@quality$support))
    rules = rules[rules@quality$support<= upperThreshSupport]
  if( !is.null(rules@quality$confidence))
    rules = rules[rules@quality$confidence<= upperThreshConfidence]
  if(!is.null(rules@quality$lift))
    rules = rules[rules@quality$lift<= upperThreshLift]
  
  #remove rules with low 'lift'  and sort by measure
  rules = rules[rules@quality$lift>threshLift]
  
  #take care of redandancy
  rules <- cleanRedundant(rules, measure  =  filterRedundantBy)
  
  rules  <-  sort(rules, by = sortRulesBy)
  if(length(rules)>maxRules)
    rules = rules[1:maxRules]
}

#cut showFrom to showTo
if(length(rules)>=showFrom)
{
  rules = rules[max(1,showFrom):min(showTo,length(rules))]
}else{
  rules = NULL
}



if(length(rules) == 0)
{
  visualisationMethod = "empty"
  pbiWarning <- paste(pbiWarning, "No rules generated, for current set of thresholds", sep = "\n")
}

#Visualizing Association Rules
if(visualisationMethod == "graph") # graph 
{
  gp = getGridPartition(length(rules), rulesPerGraphPlate)
  
  
  par(oma=0.25*c(1,1,1,1),mar=0.5*c(1,1,1,1), mfrow = c(gp$numRows, gp$numCols), xpd = TRUE)
  # unfortunatly: mar and xpd are overwrited
  
  for (p in 1:length(gp$partit))
  {
    s = sum(gp$partit[seq(1, length.out = p-1)])+1
    e = s+gp$partit[p]-1
    print(s:e)
    
    numEdgesRHS = sum(as(rules[s:e]@rhs, "matrix"))
    numEdgesLHS = sum(as(rules[s:e]@lhs, "matrix"))
    edge.color = c(rep(edgeColLHS, numEdgesLHS),rep(edgeColRHS, numEdgesRHS))
    control = list( alpha = 1, measureLabels = FALSE, 
                   cex = fontSizeGraph, precision = 1, arrowSize = 0.5, 
                   main = "",  layoutParams	 =  list(xpd = T), 
                   labelCol = edge.color)
    
    plot(rules[s:e], method = "graph", control = control, margin = -0.01, frame = FALSE,type = "items")
    
  }
}

if(visualisationMethod == "table") # table 
{
  qualityMetrics <- rules@quality
  lhs <- rules@lhs@data
  rhs <- rules@rhs@data
  
  NR <- nrow(qualityMetrics)
  NR = min(NR, maxRules)
  
  rutable  <-  data.frame(From = rep(NA, NR), To = rep(NA, NR), qualityMetrics[1:NR, ])
  lhsColNames = colnames(rules@lhs)
  rhsColNames = colnames(rules@rhs)
  
  for (r in (1:NR))
  {
    if(any(lhs[, r]))
      rutable$From[r] <- paste(lhsColNames[lhs[, r]], collapse = "\n")
    if(any(rhs[, r]))
      rutable$To[r] <- paste(rhsColNames[rhs[, r]], collapse = "\n")
  }
  
  names(rutable) =  toupper(names(rutable))
  
  tt  <-  ttheme_minimal(
    core = list(bg_params  =  list(fill  =  blues9[1:4], col = "gray"), 
                fg_params = list(fontface = 3, cex = fontSizeGraph)), 
    colhead = list(fg_params = list(col = "orange", fontface = 4L,cex = fontSizeGraph*1.1)), 
    rowhead = list(fg_params = list(col = "white", fontface = 3L)))
  
  plot.new()
  grid.table(rutable, theme = tt)
  title(main = "")
}


if(visualisationMethod == "paracoord")#parcoord 
{
  reorder = (length(rules)<42 && length(rules)>3) #reorder only if it is fast (few rules)
  if(length(rules)>0)
  {
    plot(rules, method = "paracoord", control = list( reorder = reorder, main = "", col = scaleColors(rules@quality[[parCoordColorBy]])))
  }else{
    visualisationMethod = "empty"
    pbiWarning <- paste(pbiWarning, "Paracoord method works only with two or more rules", sep = "\n")
  }
  
}

if(visualisationMethod == "scatter") #scatter (for Debug and Exploration) 
  plot(rules, control = list(cex = fontSizeGraph))# scatter  

if(visualisationMethod == "empty") #scatter (for Debug and Exploration) 
{
  plot.new()
  title(sub = pbiWarning, col.sub = "gray50")
}

remove("dataset")