#Comparing the 2 Models for Both Formulas:
FORMULA1 = AD ~ X125 + X125.1 + X0
FORMULA2 = AD ~ X1.0 + X0

#test_models() builds the linear and decision tree model using the same train set
#tests both against the same generated test set
#builds the train and test set from build_train_test() on DATA3 (all NAs imputated)
test_models = function(FORMULA, DATA2){
  #create train and test set: 70% train, 30% test
  SETS = build_train_test(DATA2, round(.3*nrow(DATA2)))
  TEST = SETS[[1]]
  TRAIN = SETS[[2]]
  #apply models
  LIN = lm(FORMULA, TRAIN)
  LOG = glm(FORMULA, TRAIN, family = 'binomial')
  TRE = rpart(FORMULA, TRAIN, method = "anova", control = rpart.control(n = 20, cp = 0.01))
  #Predict Values and round
  PRED_LIN = predict(LIN, TEST, type = 'response')
  PRED_LOG = predict(LOG, TEST, type = 'response')
  PRED_TRE = predict(TRE, TEST, type = 'vector')
  #cbind the PRED vectors with the image IDs
  PREDICTIONS = cbind(TEST$ID, TEST$AD, PRED_LIN, PRED_LOG, PRED_TRE)
  #round predictions to 0 and 1: 1 = advertisement
  PREDICTIONS[,3:5] = round(PREDICTIONS[,3:5])
  #change values outside of 1 and 0 in the linear model to 1 and 0
  PREDICTIONS[which(PREDICTIONS[,3] < 0),3]=0
  PREDICTIONS[which(PREDICTIONS[,3] > 1),3]=1
  return(PREDICTIONS)
}

PRE_F1 = test_models(FORMULA1, DATA2)
PRE_F2 = test_models(FORMULA2, DATA2)

####################################################################
