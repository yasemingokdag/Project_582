

rm(list = ls())

require(data.table)
require(TunePareto)
require(glmnet)


testStart=as.Date('2019-01-01')
trainStart=as.Date('2012-07-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_cleaning.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')

matches_data_path='df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_data_path='df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'


# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers
features=extract_features.CombineFeatures(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)


# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart] 

#a <- extract_features.pcaComponents(train_features, test_features)[[2]]
#b <- extract_features.pcaComponents(train_features, test_features)[[3]]

# run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions=train_glmnet(train_features, test_features,not_included_feature_indices=c(1:5), alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)

#saveRDS (predictions,file="PredictionsRound8.RDS")


All=data.table()
Results=data.table()
for (t in 1:8 )
{

  predictions=  readRDS(paste0("PredictionsRound",t,".RDS"))
  
MatchDate=matches[Match_Date>=testStart,c('matchId','Match_Date','Home','Away'),with=F]
MatchDate[order(Match_Date),rank:=1:.N]

PredictionTable=predictions$predictions
PredictionTable= PredictionTable[!is.na(Match_Result)]
PredictionTable=PredictionTable[!is.na(Home)]

MatchIds=MatchDate[rank<=10]$matchId

Outcomes=copy(PredictionTable)
Outcomes[Match_Result=='Away',Away:=1]
Outcomes[Match_Result=='Away',Home:=0]
Outcomes[Match_Result=='Away',Tie:=0]

Outcomes[Match_Result=='Home',Away:=0]
Outcomes[Match_Result=='Home',Home:=1]
Outcomes[Match_Result=='Home',Tie:=0]

Outcomes[Match_Result=='Tie',Away:=0]
Outcomes[Match_Result=='Tie',Home:=0]
Outcomes[Match_Result=='Tie',Tie:=1]

ResTemp=data.frame(Round=t,RPS=mean(RPS_matrix(PredictionTable[matchId %in% MatchIds,c(3:5),with=F],Outcomes[matchId %in% MatchIds,c(3:5),with=F])))
Results=rbind(Results,ResTemp)

setkey(MatchDate,matchId)
setkey(PredictionTable,matchId)
AllTemp=MatchDate[PredictionTable][matchId %in% MatchIds]
AllTemp[,round:= t]
All=rbind(AllTemp,All)


}

