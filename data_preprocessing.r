

#' Data Preprocessing for Matches Data
#'
#' Makes preprocessing on raw matches data so that it can be used by other functions.
#'
#' @param data A data.table containing raw match data
#' @export
#' @examples
#' matches_data_preprocessing(matches_raw)
#'

matches_data_preprocessing <- function(data){

  temp = copy(data)
  temp = unique(temp,by="matchId") 
  setnames(temp,c("home","away","score","date"),c("Home","Away","Score","Match_Date"))
  temp[,Home:=tolower(Home)]
  temp[,Away:=tolower(Away)]
  temp[,Match_DateTime:=as.POSIXct(Match_Date,tz="UTC",origin = as.POSIXct("1970-01-01",tz="UTC"))]
  temp[,Match_Hour := format(strptime(Match_DateTime,"%Y-%m-%d %H:%M:%OS"),'%H')]
  temp[,Match_Hour := as.numeric(Match_Hour)]
  temp[,Match_Date := as.Date(Match_DateTime,format="%Y-%m-%d")]

  temp[,AWA_FLAG:=0]
  temp[(Score %like% "AWA."),`:=`(Score=gsub("AWA.","",Score),AWA_FLAG=1)]
  temp[,POSTP_FLAG:=0]
  temp[(Score == "POSTP."),`:=`(Score=gsub("POSTP.","",Score),POSTP_FLAG=1)]
  temp[,CAN_FLAG:=0]
  temp[(Score == "CAN."),`:=`(Score=gsub("CAN.","",Score),CAN_FLAG=1)]
  latestDateTimeofKnownScore = max(temp[Score!=""]$Match_DateTime)
  POSTP_toberemoved = temp[which(temp$POSTP_FLAG==1 & temp$Match_DateTime <= latestDateTimeofKnownScore)]$matchId
  if(length(POSTP_toberemoved)!=0){
    cat("Following postponed matches are REMOVED during data_preprocessing:\n")
    print(data[matchId%in%POSTP_toberemoved,])
    temp=temp[!matchId%in%POSTP_toberemoved,]
  }
  POSTP_tobekept = temp[which(temp$POSTP_FLAG==1 & temp$Match_DateTime > latestDateTimeofKnownScore)]$matchId
  if(length(POSTP_tobekept)!=0){
    cat("Following postponed matches are KEPT during data_preprocessing:\n")
    print(data[matchId%in%POSTP_tobekept,])
  }

  temp[,c("Home_Score","Away_Score") := lapply(tstrsplit(Score,":",fixed=T),as.integer)]
  temp[,`:=`(Match_Result = ifelse(Home_Score == Away_Score, "Tie" , ifelse(Home_Score > Away_Score, 'Home' , 'Away'))
             ,Total_Score = Home_Score + Away_Score)]
  temp[,`:=`(Result_Home = ifelse(Match_Result=="Home",1,0)
             ,Result_Tie = ifelse(Match_Result=="Tie",1,0)
             ,Result_Away = ifelse(Match_Result=="Away",1,0))]

  temp[,c('Score','Match_DateTime','AWA_FLAG','POSTP_FLAG','CAN_FLAG'):=NULL]
  gc()
  return(temp)
}

#' Data Preprocessing for Odd Details Data
#'
#' Makes preprocessing on raw odd details data so that it can be used by other functions.
#' only 1x2 bets are considered, if you are interested in other type of odds manipulate ''which_bets''
#' @param data A data.table containing raw odd details data
#' @param remove_bookmaker A character vector containing the bookmakers to be removed
#' @export
#' @examples

details_data_preprocessing <- function(data,matches,which_bets=c('1x2'),remove_bookmaker=c('BetfairExchange','PaddyPower'),removeOlderThan=30){
  # data manipulation for historical odd data
  details = copy(data)
  
  #remove duplicate entries
  details = unique(details)

  details = details[betType %in% which_bets]
  details[,totalhandicap:=NULL]

  details = merge(details,matches[,list(matchId,Match_Date)],by="matchId",all.x=T)
  setnames(details,"date","OddChangeDateTime")
  details[,OddChangeDateTime:=as.POSIXct(OddChangeDateTime,tz="UTC",origin = as.POSIXct("1970-01-01",tz="UTC"))]
  details = details[difftime(Match_Date,OddChangeDateTime, units = "days") <= removeOlderThan] #remove odds seen earlier than 10 days from the match date
  details[, odd := as.numeric(odd)]

  details[,bookmaker:=gsub(" |-","",bookmaker)]
  if(!is.null(remove_bookmaker)){
    details = details[!(bookmaker %in% remove_bookmaker)]
  }

  gc()
  return(details)
}




getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



extract_features.PreviousResults<- function(matches,teststart){
  
  matches=matches[Match_Date<teststart]
  
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
  
  PreviousMatches$Identical5Freq=apply(PreviousMatches[,c('Identical1','Identical2','Identical3','Identical4','Identical5'),with=F],1,getmode)
  PreviousMatches$TwoWay5Freq=apply(PreviousMatches[,c('TwoWay1','TwoWay2','TwoWay3','TwoWay4','TwoWay5'),with=F],1,getmode)
  
  PreviousMatches$Identical2Freq=apply(PreviousMatches[,c('Identical1','Identical2'),with=F],1,getmode)
  PreviousMatches$TwoWay2Freq=apply(PreviousMatches[,c('TwoWay1','TwoWay2'),with=F],1,getmode)
  
  PreviousMatches$Identical1Freq=apply(PreviousMatches[,c('Identical1'),with=F],1,getmode)
  PreviousMatches$TwoWay1Freq=apply(PreviousMatches[,c('TwoWay1'),with=F],1,getmode)
  
 
  PreviousMatches=PreviousMatches[,c('matchId','Identical5Freq','TwoWay5Freq','Identical2Freq','TwoWay2Freq','Identical1Freq','TwoWay1Freq'),with=F]
  return(PreviousMatches)
   
}


