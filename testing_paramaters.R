#evaluating results

#Paramater 1: Number of predictions accurately guessed
#input P is the matrix containing the prediciton vectors
#M is the model testing: 1,2,3 for lin, log, and tree respectively
calc_param_1 = function(P, M){
  RIG = 0
  WRO = 0
  TEST_COLUMN = 0
  switch (M,
          '1' = { #Case lin
            TEST_COLUMN = 3
          },
          '2' = { #Case log
            TEST_COLUMN = 4
          },
          '3' = { #Case tree
            TEST_COLUMN = 5
          }
  )
  #loop through P
  for(i in 1:nrow(P)){
    #obtain ad bool with model prediction(column 2)
    ACTUAL = P[i,2]
    PRED = P[i,TEST_COLUMN]
    BOOL = ACTUAL == PRED
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
  return(RIG/(RIG+WRO))
}

#Paramter 2: # of Ads predicted : actual 3 of ads in test
calc_param_2 = function(P,M){
  TEST_COLUMN = 0
  switch (M,
          '1' = { #Case lin
            TEST_COLUMN = 3
          },
          '2' = { #Case log
            TEST_COLUMN = 4
          },
          '3' = { #Case tree
            TEST_COLUMN = 5
          }
  )
  ACTUAL = as.numeric(table(P[,2])[2])
  PRED = as.numeric(table(P[,TEST_COLUMN])[2])
  return(PRED/ACTUAL)
}
#test
calc_param_2(test_models(FORMULA1, DATA2), 1)


#Paramater 3: # of ads correctly predicted vs # of ads in total set
calc_param_3 = function(P, M){
  TEST_COLUMN = 0
  switch (M,
          '1' = { #Case lin
            TEST_COLUMN = 3
          },
          '2' = { #Case log
            TEST_COLUMN = 4
          },
          '3' = { #Case tree
            TEST_COLUMN = 5
          }
  )
  #get testing and actual columns
  ACTUAL = P[,2]
  PRED = P[,TEST_COLUMN]
  #get indices of Advertisements
  IND = which(ACTUAL == 1)
  #wrap indices around ACTUAL and PRED
  ACTUAL = ACTUAL[IND]
  PRED = PRED[IND]
  #get number of right predictions
  RIGHT = 0
  WRONG = 0
  for(i in 1:length(ACTUAL)){
    #get actual
    AA = ACTUAL[i]
    #get pred
    PP = PRED[i]
    BOOL = AA==PP
    if(BOOL == TRUE){
      #add 1 to right
      RIGHT = RIGHT+1
    }
    #ad bool != model prediction - model failed. add += 1 to WRO
    else{
      WRONG = WRONG+1
    }
  }
  return(RIGHT/(RIGHT+WRONG))
}

calc_param_3(PRE_F2, 1)
