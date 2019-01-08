#' Feature Extraction

extract_features.openclose <- function(matches,odd_details,pMissThreshold=0.01,trainStart,testStart){

  details = copy(odd_details)
  matches = copy(matches)

  details=details[order(OddChangeDateTime)]
  feature_odd_details=details[,list(Odd_Open=odd[1],Odd_Close=odd[.N]),list(matchId,betType,oddtype,bookmaker)]

  feature_odd_details = merge(matches[,list(matchId,Match_Date)], feature_odd_details,by="matchId")


  #HANDLE MISSINGS
  details_temp = dcast(feature_odd_details, matchId+betType ~ paste0("Odd_Close_",bookmaker)+oddtype, value.var = c("Odd_Close"))
  details_melt = melt(details_temp, id.vars = c("matchId","betType"), measure.vars = names(details_temp)[names(details_temp) %like% "Odd_Close"], value.name = "odd")
  details_melt[,c("OpenClose","bookmaker","oddtype"):=tstrsplit(variable,split="_",keep=c(2:4))]
  details_melt[,variable:=NULL]
  details_melt = merge(matches[,list(matchId,Match_Date)], details_melt,by="matchId",all=T)
  
  bookieMissingness = details_melt[Match_Date >= trainStart,list(.N,percMiss=sum(is.na(odd))/.N),by=list(bookmaker,betType)]
  bookiesToKeep = unique(bookieMissingness[percMiss <= pMissThreshold]$bookmaker)
  cat("Number of bookmakers with proportion of missings below",pMissThreshold,"since",as.character(trainStart),":",length(bookiesToKeep),"\n")

  nonmissingBookmakers_sinceTestStart = unique(details_melt[Match_Date >= testStart, list(.N,NA_SUM=sum(is.na(odd))),by=list(bookmaker,betType)][NA_SUM==0]$bookmaker)
  bookiesToKeep = intersect(bookiesToKeep,nonmissingBookmakers_sinceTestStart)
  cat("Number of bookmakers with no missings since testStart", as.character(testStart), ":", length(bookiesToKeep), "\n")

  details = dcast(feature_odd_details,matchId~oddtype+bookmaker,value.var = c("Odd_Open","Odd_Close"))
  columnsToKeep = grep(paste(bookiesToKeep,collapse="|"),names(details),value=T)
  details = details[,c('matchId',columnsToKeep),with=F]
  #HANDLE MISSINGS END


  details = merge(matches[,-c('Home','Away','Home_Score','Away_Score','Total_Score','Result_Home','Result_Tie','Result_Away','type'),with=F],
                  details,by="matchId",all=T)


  return(features = details)
}



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



extract_features.PreviousResults<- function(matches){

  
  DistinctSides= unique(matches[,c('Home','Away'),with=F])
  DistinctSides[,rowcount:=1:.N]
  
  DistinctSides[,TeamsId:=0]
  for (i in 1:nrow(DistinctSides))
  { 
    DistinctSides[( TeamsId==0 & rowcount==i )| (rowcount>i & Home==DistinctSides[i]$Away & Away==DistinctSides[i]$Home), TeamsId:=i]
  }
  
  setnames(DistinctSides,'rowcount','Identical')
  setkey(DistinctSides,Home,Away)
  setkey(matches,Home,Away)
  
  matches=DistinctSides[matches]
  
  matches[,Winner:=ifelse(Match_Result=='Home',Home,'Else')]
  matches[Winner=='Else',Winner:=ifelse(Match_Result=='Away',Away,'Tie')]
  
  PreviousMatches=matches[,c('Match_Date','matchId','Winner','Home','Away','TeamsId'),with=F]
  
  PreviousMatches[order(Match_Date),Identical1:=shift(Winner,1,type='lag'),by=c('Home','Away') ]
  PreviousMatches[order(Match_Date),Identical2:=shift(Winner,2,type='lag'),by=c('Home','Away') ]
  PreviousMatches[order(Match_Date),Identical3:=shift(Winner,3,type='lag'),by=c('Home','Away') ]
  PreviousMatches[order(Match_Date),Identical4:=shift(Winner,4,type='lag'),by=c('Home','Away') ]
  PreviousMatches[order(Match_Date),Identical5:=shift(Winner,5,type='lag'),by=c('Home','Away') ]  
  PreviousMatches[order(Match_Date),Identical5:=shift(Winner,5,type='lag'),by=c('Home','Away') ]  
  
  PreviousMatches[order(Match_Date),TwoWay1:=shift(Winner,1,type='lag'),by=TeamsId ]
  PreviousMatches[order(Match_Date),TwoWay2:=shift(Winner,2,type='lag'),by=TeamsId ]
  PreviousMatches[order(Match_Date),TwoWay3:=shift(Winner,3,type='lag'),by=TeamsId ]
  PreviousMatches[order(Match_Date),TwoWay4:=shift(Winner,4,type='lag'),by=TeamsId ]
  PreviousMatches[order(Match_Date),TwoWay5:=shift(Winner,5,type='lag'),by=TeamsId ]
  
  # PreviousMatches$Identical5Freq=apply(PreviousMatches[,c('Identical1','Identical2','Identical3','Identical4','Identical5'),with=F],1,getmode)
  # PreviousMatches$TwoWay5Freq=apply(PreviousMatches[,c('TwoWay1','TwoWay2','TwoWay3','TwoWay4','TwoWay5'),with=F],1,getmode)
  
  PreviousMatches$Identical2Freq=apply(PreviousMatches[,c('Identical1','Identical2'),with=F],1,getmode)
  PreviousMatches$TwoWay2Freq=apply(PreviousMatches[,c('TwoWay1','TwoWay2'),with=F],1,getmode)
  
  PreviousMatches$Identical1Freq=apply(PreviousMatches[,c('Identical1'),with=F],1,getmode)
  PreviousMatches$TwoWay1Freq=apply(PreviousMatches[,c('TwoWay1'),with=F],1,getmode)
  
  
  PreviousMatches=PreviousMatches[,c('matchId','Identical2Freq','TwoWay2Freq','Identical1Freq','TwoWay1Freq'),with=F]
  return(PreviousMatches)
  
}

extract_features.HomeAwayPerformances<- function(matches,nmatch = 5){

data= matches
#Get mode(result) of the previous 10 games per home per game 
data <- data[order(Home, -Match_Date)][, HomeMatchRank := 1:.N, list(Home)]
Bound <- max(data$HomeMatchRank)

HomeResults <- data.table()
for (i in (1:Bound))
{
  matchIds=data[HomeMatchRank==i]$matchId
  select_data <- data[HomeMatchRank > i & HomeMatchRank <= i+nmatch][, list(lapply(Match_Result,getmode)), by = c("Home")]
  if (nrow(select_data) >0) {
  colnames=colnames(select_data)
  select_data <-as.data.table(matrix(unlist(select_data), nrow=nrow(select_data)),stringsAsFactors=FALSE)
  setnames(select_data,colnames)
  select_data <- select_data[, (lapply(.SD,getmode)), by = c("Home")]
  select_data=cbind(select_data,matchIds)
  HomeResults <- rbind(HomeResults,select_data)
 }
}

setnames(HomeResults,'V1','HomeResult')

data <- data[order(Away, -Match_Date)][, AwayMatchRank := 1:.N, list(Away)]
Bound <- max(data$AwayMatchRank)

AwayResults <- data.table()
for (i in (1:Bound))
{
  
  matchIds=data[AwayMatchRank==i]$matchId
  select_data <- data[AwayMatchRank > i & AwayMatchRank <= i+nmatch][, list(lapply(Match_Result,getmode)), by = c("Away")]
  if (nrow(select_data) >0) {
  colnames=colnames(select_data)
  select_data <-as.data.table(matrix(unlist(select_data), nrow=nrow(select_data)),stringsAsFactors=FALSE)
  setnames(select_data,colnames)
  select_data <- select_data[, (lapply(.SD,getmode)), by = c("Away")]
  select_data=cbind(select_data,matchIds)
  AwayResults <- rbind(AwayResults,select_data)
  }
}

setnames(AwayResults,'V1','AwayResult')

setkey(AwayResults,matchIds)
setkey(HomeResults,matchIds)

Results=AwayResults[HomeResults]

return(Results)
}


extract_features.ELORatings <- function(){
  
  # read data
  ELORatings=fread("Elo_Ratings.txt")
  return(ELORatings)
}

extract_features.GoalDifferences<- function(matches){
  
  Teams =as.data.table(unique(matches$Home)) 
  Teams2 =as.data.table(unique(matches$Away) )
  Teams=unique(rbind(Teams,Teams2))
  
  
  Goal_v=data.table()
  Goal_va=data.table()
  
  for (i in 1:nrow(Teams)) 
  {
    Team = Teams[i]
    PreviousMatches=matches[Home==Team$V1 | Away==Team$V1][order(Match_Date)]
    PreviousMatches[Home==Team$V1,GolFor := Home_Score]
    PreviousMatches[Away==Team$V1,GolFor := Away_Score]
    PreviousMatches[Away==Team$V1,GolAgainst := Home_Score]
    PreviousMatches[Home==Team$V1,GolAgainst := Away_Score]
    
    PreviousMatches[order(Match_Date),GoalDif:=( shift(GolFor,1,type='lag')-shift(GolAgainst,1,type='lag')+
                                                   shift(GolFor,2,type='lag')-shift(GolAgainst,2,type='lag')+
                                                   shift(GolFor,3,type='lag')-shift(GolAgainst,3,type='lag')+                                    +
                                                   shift(GolFor,4,type='lag')-shift(GolAgainst,4,type='lag')+
                                                   shift(GolFor,5,type='lag')-shift(GolAgainst,5,type='lag')+
                                                   shift(GolFor,6,type='lag')-shift(GolAgainst,6,type='lag')) ]
    
    Goal=PreviousMatches[Home==Team$V1,c('matchId','GoalDif'),with=F]
    GoalA=PreviousMatches[Away==Team$V1,c('matchId','GoalDif'),with=F]
    setnames(Goal,'GoalDif','HomeGoalDif')
    setnames(GoalA,'GoalDif','AwayGoalDif')
    Goal_v=rbind(Goal_v,Goal)
    Goal_va=rbind(Goal_va,GoalA)
  }
  
  setkey(Goal_v,matchId)
  setkey(Goal_va,matchId)
  GoalDifferences=Goal_va[Goal_v]
  GoalDifferences[,DifferenceinGoalDif:=HomeGoalDif-AwayGoalDif]
  GoalDifferences=GoalDifferences[,c(1,4),with=F]
  return(GoalDifferences)
  
  
}


extract_features.FIFARatings <- function(matches){
  
  # read data
  FifaRatings=fread("Fifa_Rating.txt")
  
  Teams=unique(matches[,c('Home','Away'),with=F])

  setkey(Teams,Home)
  setkey(FifaRatings,Team)
  Teams=FifaRatings[Teams]
  
  setnames(Teams,c(2:5),c('HomeAttack','HomeMidfield','HomeDefense','HomeOverall'))
  
  setkey(Teams,Away)
  setkey(FifaRatings,Team)
  Teams=FifaRatings[Teams]
  
  setnames(Teams,c('Attack','Midfield','Defense','Overall'),c('AwayAttack','AwayMidfield','AwayDefense','AwayOverall'))
  setnames(Teams,'Team','Home')
  setnames(Teams,'i.Team','Away')
  
  
  Teams=Teams[,list(AttackDiff=HomeAttack-AwayAttack,
              MidfieldDiff=HomeMidfield-AwayMidfield,
              DefenseDiff=HomeDefense-AwayDefense,
              OverallDiff=HomeOverall-AwayOverall),list(Home,Away)]
  
  MatchIds=matches[,c('matchId','Home','Away'),with=FALSE]
  
  setkey(MatchIds,Home,Away)
  setkey(Teams,Home,Away)
  FifaRatingDiff=Teams[MatchIds]
  return(FifaRatingDiff)
  
}


extract_features.CombineFeatures <- function(matches,odd_details,pMissThreshold=0.01,trainStart,testStart){
  
  OddFeatures=extract_features.openclose(matches,odd_details,pMissThreshold=0.01,trainStart,testStart)
  
  setkey(OddFeatures,matchId)
  
  FifaRatingDiff=extract_features.FIFARatings(matches)
  ELO=extract_features.ELORatings()
  HomeAwayRecentPerf=  extract_features.HomeAwayPerformances(matches,nmatch=10)
  PreviousResults=extract_features.PreviousResults(matches)

  
  MatchIds=matches[,c('matchId','Home','Away'),with=FALSE]
  setkey(MatchIds,Home)
  setkey(ELO,Team)
  Data=ELO[MatchIds]
  setnames(Data,'ELO','HomeELO')
  setnames(Data,'Team','Home')
  setkey(Data,Away)
  setkey(ELO,Team)
  Data=ELO[Data]
  setnames(Data,'ELO','AwayELO')
  setnames(Data,'Team','Away')

  FifaRatingDiff$Home=NULL
  FifaRatingDiff$Away=NULL
  HomeAwayRecentPerf$Away=NULL
  HomeAwayRecentPerf$Home=NULL

  
  setkey(Data,matchId)
  setkey(FifaRatingDiff,matchId)
  Data=FifaRatingDiff[Data]
  
  
  setkey(Data,matchId)
  setkey(HomeAwayRecentPerf,matchIds)
  Data=HomeAwayRecentPerf[Data]
  
  setkey(Data,matchIds)
  setkey(PreviousResults,matchId)
  Data=PreviousResults[Data]
  
  # Data[Identical5Freq==Away,Identical5Freq:=2]
  # Data[Identical5Freq==Home,Identical5Freq:=1]
  # Data[Identical5Freq=='Tie',Identical5Freq:=0]
  # Data[TwoWay5Freq==Away,TwoWay5Freq:=2]
  # Data[TwoWay5Freq==Home,TwoWay5Freq:=1]
  # Data[TwoWay5Freq=='Tie',TwoWay5Freq:=0]
  Data[Identical2Freq==Away,Identical2Freq:=2]
  Data[Identical2Freq==Home,Identical2Freq:=1]
  Data[Identical2Freq=='Tie',Identical2Freq:=0]
  
  Data$Identical2Freq <- as.numeric(Data$Identical2Freq)
  
  Data[TwoWay2Freq==Away,TwoWay2Freq:=2]
  Data[TwoWay2Freq==Home,TwoWay2Freq:=1] 
  Data[TwoWay2Freq=='Tie',TwoWay2Freq:=0]
  
  Data$TwoWay2Freq <- as.numeric(Data$TwoWay2Freq)
  
  
  Data[Identical1Freq==Away,Identical1Freq:=2]
  Data[Identical1Freq==Home,Identical1Freq:=1]
  Data[Identical1Freq=='Tie',Identical1Freq:=0]
  
  Data$Identical1Freq <- as.numeric(Data$Identical1Freq)
  
  Data[TwoWay1Freq==Away,TwoWay1Freq:=2]
  Data[TwoWay1Freq==Home,TwoWay1Freq:=1]
  Data[TwoWay1Freq=='Tie',TwoWay1Freq:=0]

  Data$TwoWay1Freq <- as.numeric(Data$TwoWay1Freq)
  
  Data[AwayResult=='Home',AwayResult:=1]
  Data[AwayResult=='Away',AwayResult:=2]
  Data[AwayResult=='Tie',AwayResult:=0]
  
  Data$AwayResult <- as.numeric(Data$AwayResult)
  
  Data[HomeResult=='Home',HomeResult:=1]
  Data[HomeResult=='Away',HomeResult:=2]
  Data[HomeResult=='Tie',HomeResult:=0]
  
  Data$HomeResult <- as.numeric(Data$HomeResult)
  
  Data[Identical1Freq=='Tie',Identical1Freq:=0]
  
  Data$Identical1Freq <- as.numeric(Data$Identical1Freq)
   
  setkey(Data,matchId)
  setkey(OddFeatures,matchId)
  Data=Data[OddFeatures]
  Data$Away=NULL
  Data$Home=NULL
  
  GoalDiff=  extract_features.GoalDifferences(matches)
  setkey(GoalDiff,matchId)
  setkey(Data,matchId)
  Data=GoalDiff [Data]
  
  setcolorder(Data, c("leagueId","Match_Date","Match_Hour","Match_Result","matchId","DifferenceinGoalDif","Identical2Freq","TwoWay2Freq","Identical1Freq","TwoWay1Freq","AwayResult","HomeResult","AttackDiff","MidfieldDiff","DefenseDiff","OverallDiff","AwayELO","HomeELO","Odd_Open_odd1_10Bet","Odd_Open_odd1_12BET","Odd_Open_odd1_188BET","Odd_Open_odd1_BetVictor","Odd_Open_odd1_Betclic","Odd_Open_odd1_Betsafe","Odd_Open_odd1_Betsson","Odd_Open_odd1_Betway","Odd_Open_odd1_Pinnacle","Odd_Open_odd1_SBOBET","Odd_Open_odd1_Unibet","Odd_Open_odd1_WilliamHill","Odd_Open_odd1_bet365","Odd_Open_odd1_betathome","Odd_Open_odd1_bwin","Odd_Open_odd2_10Bet","Odd_Open_odd2_12BET","Odd_Open_odd2_188BET","Odd_Open_odd2_BetVictor","Odd_Open_odd2_Betclic","Odd_Open_odd2_Betsafe","Odd_Open_odd2_Betsson","Odd_Open_odd2_Betway","Odd_Open_odd2_Pinnacle","Odd_Open_odd2_SBOBET","Odd_Open_odd2_Unibet","Odd_Open_odd2_WilliamHill","Odd_Open_odd2_bet365","Odd_Open_odd2_betathome","Odd_Open_odd2_bwin","Odd_Open_oddX_10Bet","Odd_Open_oddX_12BET","Odd_Open_oddX_188BET","Odd_Open_oddX_BetVictor","Odd_Open_oddX_Betclic","Odd_Open_oddX_Betsafe","Odd_Open_oddX_Betsson","Odd_Open_oddX_Betway","Odd_Open_oddX_Pinnacle","Odd_Open_oddX_SBOBET","Odd_Open_oddX_Unibet","Odd_Open_oddX_WilliamHill","Odd_Open_oddX_bet365","Odd_Open_oddX_betathome","Odd_Open_oddX_bwin","Odd_Close_odd1_10Bet","Odd_Close_odd1_12BET","Odd_Close_odd1_188BET","Odd_Close_odd1_BetVictor","Odd_Close_odd1_Betclic","Odd_Close_odd1_Betsafe","Odd_Close_odd1_Betsson","Odd_Close_odd1_Betway","Odd_Close_odd1_Pinnacle","Odd_Close_odd1_SBOBET","Odd_Close_odd1_Unibet","Odd_Close_odd1_WilliamHill","Odd_Close_odd1_bet365","Odd_Close_odd1_betathome","Odd_Close_odd1_bwin","Odd_Close_odd2_10Bet","Odd_Close_odd2_12BET","Odd_Close_odd2_188BET","Odd_Close_odd2_BetVictor","Odd_Close_odd2_Betclic","Odd_Close_odd2_Betsafe","Odd_Close_odd2_Betsson","Odd_Close_odd2_Betway","Odd_Close_odd2_Pinnacle","Odd_Close_odd2_SBOBET","Odd_Close_odd2_Unibet","Odd_Close_odd2_WilliamHill","Odd_Close_odd2_bet365","Odd_Close_odd2_betathome","Odd_Close_odd2_bwin","Odd_Close_oddX_10Bet","Odd_Close_oddX_12BET","Odd_Close_oddX_188BET","Odd_Close_oddX_BetVictor","Odd_Close_oddX_Betclic","Odd_Close_oddX_Betsafe","Odd_Close_oddX_Betsson","Odd_Close_oddX_Betway","Odd_Close_oddX_Pinnacle","Odd_Close_oddX_SBOBET","Odd_Close_oddX_Unibet","Odd_Close_oddX_WilliamHill","Odd_Close_oddX_bet365","Odd_Close_oddX_betathome","Odd_Close_oddX_bwin"))
  setnames(Data, "DifferenceinGoalDif", "GoalDiff")
  
  return(as.data.table(Data))
  
}

extract_features.pcaComponents <- function(features){
  
  data <- features
  data <- na.omit(data)
  col <- ncol(data)
  row <- nrow(data)
  scaled_data <- data.table()
  data<- as.data.table(data)
  scaled_data <- rbind(scaled_data,features[,1:5])
  colnames <- names(data)
  
  for (j in 6:col)
  {
    scaled_data[,j] <- scale(data[,get(colnames[j])])
    j = j+1
  }
  setnames(scaled_data, colnames)
  pca_prin <- princomp(scaled_data[,6:ncol(scaled_data)])
  load <- pca_prin$loadings
  load <- load[,1:12]
  load <- as.matrix(load)
  
  s <- scaled_data[,1:5]
  t <- scaled_data[,6:ncol(scaled_data)]
  t <- as.matrix(t)
  
  r <- t%*%load
  new_data <- cbind(s,r)
  
  loadings = pca_prin$loadings[]
  p.variance.explained = pca_prin$sdev^2 / sum(pca_prin$sdev^2)
  
  # plot percentage of variance explained for each principal component    
  p.plot <- barplot(100*p.variance.explained[1:12], las=2, xlab='', ylab='% Variance Explained')
  
  return(list(p.plot, new_data))
}


