#Running the Tests

TEST_LIST = NULL

for(i in 1:100){
  
  #generate predictions for F1 and F2
  PRE_F1 = test_models(FORMULA1, DATA2)
  PRE_F2 = test_models(FORMULA2, DATA2)
  
  #evaluate lin model on PRE_F1 and PRE_F2
  P1_LIN_F1 = calc_param_1(PRE_F1, 1)
  P1_LIN_F2 = calc_param_1(PRE_F2, 1)
  P2_LIN_F1 = calc_param_2(PRE_F1, 1)
  P2_LIN_F2 = calc_param_2(PRE_F2, 1)
  P3_LIN_F1 = calc_param_3(PRE_F1, 1)
  P3_LIN_F2 = calc_param_3(PRE_F2, 1)
  
  #evaluate log model on PRE_F1 and PRE_F2
  P1_LOG_F1 = calc_param_1(PRE_F1, 2)
  P1_LOG_F2 = calc_param_1(PRE_F2, 2)
  P2_LOG_F1 = calc_param_2(PRE_F1, 2)
  P2_LOG_F2 = calc_param_2(PRE_F2, 2)
  P3_LOG_F1 = calc_param_3(PRE_F1, 2)
  P3_LOG_F2 = calc_param_3(PRE_F2, 2)
  
  #evaluate tree model on PRE_F1 and PRE_F2
  P1_TRE_F1 = calc_param_1(PRE_F1, 3)
  P1_TRE_F2 = calc_param_1(PRE_F2, 3)
  P2_TRE_F1 = calc_param_2(PRE_F1, 3)
  P2_TRE_F2 = calc_param_2(PRE_F2, 3)
  P3_TRE_F1 = calc_param_3(PRE_F1, 3)
  P3_TRE_F2 = calc_param_3(PRE_F2, 3)
  
  #bind to lists
  L1 = list(cbind(P1_LIN_F1, P1_LIN_F2), cbind(P1_LOG_F1, P1_LOG_F2), cbind(P1_TRE_F1, P1_TRE_F2))
  L2 = list(cbind(P2_LIN_F1, P2_LIN_F2), cbind(P2_LOG_F1, P2_LOG_F2), cbind(P2_TRE_F1, P2_TRE_F2))  
  L3 = list(cbind(P3_LIN_F1, P3_LIN_F2), cbind(P3_LOG_F1, P3_LOG_F2), cbind(P3_TRE_F1, P3_TRE_F2))  
  L = list(L1, L2, L3)
  
  #bind to master list
  TEST_LIST[[i]] = L
  print(i)
}

TEST_LIST[[1]][[1]][[2]][1]

get_df = function(Paramater, Formula, Model){
  #DF to bind to
  DF = NULL
  SEARCH = NULL
  #loop through test list
  for(i in 1:100){
    SEARCH = TEST_LIST[[i]][[Paramater]][[Model]][Formula]
    DF = rbind(DF, SEARCH)
  }
  return(DF)
}
summary(get_df(1,2,3))

for(i in 1:3){
  print(summary(get_df(i,2,3)))
}