

rm(list = ls())

require(data.table)
require(TunePareto)
require(glmnet)


testStart=as.Date('2018-12-31')
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


# run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions=train_glmnet(train_features, test_features,not_included_feature_indices=c(1,16:19), alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=5,nFolds=10,trace=T)

MatchDate=matches[,c('matchId','Match_Date','Home','Away'),with=F]

PredictionTable=predictions$predictions


setkey(MatchDate,matchId)
setkey(PredictionTable,matchId)

MatchDate[PredictionTable][order(Match_Date)]

PredictionTable


