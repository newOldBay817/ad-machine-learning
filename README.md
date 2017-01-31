# ad-machine-learning

Relevant Files: A description of the files included and what they do:

cleaning.R
Imports data.csv and cleans the file. Imputes na values with kNN imputation. Generates a unique ID for every image.

functions.R 
Contains a preliminary test_model() function used to evaluate parameter 1, build_test_set() which shuffles and creates train and test subsets, and return_result() which was a preliminary function used to pass formulas through rpart()

generate_predictions.R
File used to generate predictions with the OLS, Logarithmic, and Decision Tree Models. Predictions for each of the 3 models are cbinded() to the image IDs and whether the image was actually an ad or not. 

testing_paramaters.R
Contains the functions used to evaluate the model across the 3 parameters

100_Tests.R
Contains script used to test the 3 models over a for loop 100 times and calculate the summary statistics of the given 3 parameters. 

plots.R
Contains scripts used to generate graphs
