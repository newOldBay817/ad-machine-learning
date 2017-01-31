#Test_model() evaluates the performance of the model by comparing predicted values to the actual values for the test subset:
#returns the number of predictions it got right and number of predictions it got wrong for a given Dataframe
#Input DF must be nrow(test) x 3 columns: ID, Actual values (wheter an ad or not), and predicted values) 
test_model = function(DF){
  #initialize right and wrong variable at 0
  RIG = 0
  WRO = 0
  #loop through DF
  for(i in 1:nrow(DF)){
    #obtain ad bool with model prediction(column 2)
    RESULT = as.vector(DF[i,2:3])
    BOOL = RESULT[1]==RESULT[2]
    #ad bool = model prediction - model got it right!
    if(BOOL == TRUE){
      #add 1 to right
      RIG = RIG+1
    }
    #ad bool != model prediction - model failed. add += 1 to WRO
    else{
      WRO = WRO+1
    }
  }
  return(c(RIG, WRO))
}

#build_train_set() shuffles the ad dataframe and creates test and training sets
#Size of train and test is determined by the input: the first N observations are used as train once the DF is shuffled
#the rest are used as the test set
#ratio of ad to non ad is consistent with original data with slight variance
build_train_test = function(DATAFRAME, SAMPLE_SIZE){
  #take Dataframe and shuffle the rows
  DATAFRAME = DATAFRAME[sample(nrow(DATAFRAME)),]
  #take train set from 1 to SAMPLE_SIZE
  train = DATAFRAME[1:SAMPLE_SIZE,]
  test = DATAFRAME[(SAMPLE_SIZE+1):nrow(DATAFRAME),]
  LIST = list(train, test)
  return(LIST)
}


#return_result() passes a given formula through rpart() using the train and test outputs from build_train_test()
#evaluates accuracy of the model using test_model() function
return_restult = function(FORMULA, LIST){
  TRAIN = LIST[[1]]
  TEST = LIST[[2]]
  #create tree using desired formula and respective train and test sets. 
  #set min number of splits to 20
  #set complexity paramater to 0.005 to prevent too much pre pruning 
  TREE = rpart(FORMULA, TRAIN, method = "anova", control = rpart.control(n = 20, cp = 0.005))
  #use predict() to test the rpart model on the test set
  PREDICTIONS = predict(TREE, newdata = TEST, type = 'vector')
  #set as vector and round to 0 or 1 to determine whether an ad or not
  PREDICTIONS = round(as.vector(PREDICTIONS))
  #cbind with the true ad values to compare
  DF = cbind(TEST$ID, TEST$AD, PREDICTIONS)
  #apply test_model() function to get the results
  RESULTS = test_model(DF)
  PERC_CORRECT = RESULTS[1]/(RESULTS[1]+RESULTS[2])
  return(PERC_CORRECT)
}
return_restult(FORMULA2, build_train_test(DATA2, round(.15*nrow(DATA2))))
