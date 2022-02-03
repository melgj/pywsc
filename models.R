library(data.table)
library(caret)
library(xgboost)


# Result Prediction Models -------------------------------------------------



train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              verboseIter = TRUE,
                              classProbs = TRUE, 
                              summaryFunction = mnLogLoss)


tune.grid <- expand.grid(eta = c(0.001, 0.01, 0.1),
                         nrounds = c(100, 150),
                         max_depth = 4:6,
                         min_child_weight = c(2.0, 2.5, 3.0),
                         colsample_bytree = c(0.75, 1.0),
                         gamma = c(0, 1),
                         subsample = 1.0)

set.seed(101)

xgbTreeModel <- train(result ~ 
                        odds_home_prob_adj +
                        odds_away_prob_adj +
                        (HSAdvL6_Avg * ASAdvL6_Avg) +
                        (HGDiffL6_Avg * AGDiffL6_Avg),
                      data = ssn1,
                      method = "xgbTree",
                      metric = "logLoss",
                      tuneGrid = tune.grid,
                      trControl = train.control)

saveRDS(xgbTreeModel, "xgbTreeModel.RDS")

varImp(xgbTreeModel)

print(xgbTreeModel)


# goals models for sim tables ------------------------------------------------------------

# These models are simply to produce goal scored/difference values to break final table ties in the season simulations
# for the xgboost Home/Away/Draw predictive model. They use post match result variable so have no predictive value

set.seed(101)

xgbSimHGMod <- train(home_score ~ 
                       HW_XGB_Prob +
                       AW_XGB_Prob +
                       result,
                     data = ssn2Preds,
                     method = "xgbTree",
                     metric = "RMSE",
                     tuneGrid = expand.grid(eta = c(0.01),
                                            nrounds = c(150),
                                            max_depth = 4,
                                            min_child_weight = c(2.0),
                                            colsample_bytree = c(0.75),
                                            gamma = 1.0,
                                            subsample = 1.0),
                     trControl = trainControl(method = "repeatedcv",
                                              number = 5,
                                              repeats = 3,
                                              verboseIter = TRUE))

saveRDS(xgbSimHGMod, "xgb_Sim_HG_Model.RDS")


set.seed(101)

xgbSimAGMod <- train(away_score ~ 
                       HW_XGB_Prob +
                       AW_XGB_Prob +
                       result,
                     data = ssn2Preds,
                     method = "xgbTree",
                     metric = "RMSE",
                     tuneGrid = expand.grid(eta = c(0.01),
                                            nrounds = c(150),
                                            max_depth = 4,
                                            min_child_weight = c(2.0),
                                            colsample_bytree = c(0.75),
                                            gamma = 1.0,
                                            subsample = 1.0),
                     trControl = trainControl(method = "repeatedcv",
                                              number = 5,
                                              repeats = 3,
                                              verboseIter = TRUE))


saveRDS(xgbSimAGMod, "xgb_Sim_AG_Model.RDS")


