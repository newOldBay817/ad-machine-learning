#DATA ORGANIZATION / FORMATTING

#load column names and dataset
column.names = readLines("column.names.txt")
column.names[1560] = 'AD'
column.names = column.names[2:length(column.names)]
data = read.csv('data.csv')

#new var for data
DATA = data
#bind colnames(DATA) to column.names
NAMES = data.frame(cbind(colnames(DATA), column.names), stringsAsFactors = F)

#create unique IDS for all sets of the data
ID = c(1:3278)
DATA = cbind(ID, DATA)

#rename the column name for ADs
colnames(DATA)[1560] = "AD"
#convert to character
DATA$AD = as.character(DATA$AD)

#replace ad. and nonad. with 1 and 0 respectively
DATA$AD[DATA$AD == "ad."] = 1
DATA$AD[DATA$AD == "nonad."] = 0
#convert to int
DATA$AD = as.integer(DATA$AD)

#convert columns 2:5 to characters
for(i in 2:5){
  DATA[,i] = as.character(DATA[,i])
}
#replace ? with NA
for(i in 2:5){
  COL = DATA[,i]
  COL = trimws(COL)
  COL[COL == "?"] = NA
  DATA[,i] = as.numeric(COL)
}

#shuffle the order of data so that ADs and NonADs are mixed up
DATA = DATA[sample(nrow(DATA)),]

#see number of NAs in the 3 continous variables
summary(data[,1:5])

#using kNN imputation to fill in missing values in the continuous variables
library(VIM)
#considering 5 nearest neighbors for the variables height, width, aratio
DATA1 = kNN(DATA, variable = c('X125', 'X125.1'), k = 5)
#Note - columns 1561 to 1563 contain boolean values for whether there were NAs or not. 
#this took forever - maybe quicker to use simple linear regression to impute misssing values or just drop them entirely. 
#nearest neighbor evaluates each observation to find the nearest neighbor along all of the 1559-3 varibles so can conclude that it gives a good estimation of the height width and aratio
help("kNN")

DATA2 = DATA1
#generate ARatio for NA values
for(i in 1:nrow(DATA2)){
  HEIGHT = DATA2$X125[i]
  WIDTH = DATA2$X125.1[i]
  RATIO = DATA$X1.0[i]
  if(is.na(RATIO)==TRUE){
    if(HEIGHT > WIDTH){
      RATIO = HEIGHT/WIDTH
    }else{
      RATIO = WIDTH/HEIGHT
    }
  }else{
    next()
  }
  print(c(i,RATIO))
  DATA2$X1.0[i] = RATIO
}

#Create a new imputed dataframe containing missing values for X1
DATA2 = kNN(DATA2, variable = 'X1', k = 5)
#check for NAs in DATA2
summary(DATA2[2:6])

# #replacing spaces in column names
column.names1 = gsub(" ", "_", column.names)
column.names1 = gsub("\\:", "", column.names1)
column.names1 = gsub("\\.", "", column.names1)

# colnames(DATA2)[2:(ncol(DATA2)-4)] = column.names1 
column.names2 = data.frame(column.names1) 

#split into training and test sets
train = DATA2[1:328,]
test = DATA2[329:nrow(DATA2),]


table(DATA2$AD)
summary(DATA1[,4])
