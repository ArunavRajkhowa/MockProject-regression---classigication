#### a basic model framework for regression and classification
library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)
library(DALEXtra)
library(dplyr)
## REGRESSION---------------------------------------------------

setwd("D:\\IITK Data Analytics\\R\\MockProject-regression---classification\\")
train=read.csv("loan_data_train.csv",stringsAsFactors = F)
test=read.csv("loan_data_test.csv",stringsAsFactors = F)

glimpse(test)

#in test set, the variable to be predicted is not given.(diff columns)
# we create such a column there with NAs and combine both datasets and do dataprep

test$Interest.Rate=NA 
train$data='train' #creating placeholders
test$data='test'   #creating placeholders

df=rbind(train,test)
glimpse(df)
vis_dat(df)
#DATA PREP STARTS
df=df %>%
  mutate(Debt.To.Income.Ratio=gsub("%","",Debt.To.Income.Ratio),
         Interest.Rate=gsub("%","",Interest.Rate))

#changing nonprocessing categories to numerical
col_names=c('Debt.To.Income.Ratio','Interest.Rate','Amount.Requested','Amount.Funded.By.Investors',
            'Open.CREDIT.Lines','Revolving.CREDIT.Balance')
for(col in col_names){
  df[,col]=as.numeric(df[,col]) 
}

#creating dummies
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

char_logical=sapply(df,is.character) #to pass the dummies func in one go
cat_cols=names(df)[char_logical]
cat_cols


cat_cols=cat_cols[!(cat_cols %in% c('data','Interest.Rate'))] #dropping data and response columns
cat_cols


for(col in cat_cols){
  df=CreateDummies(df,col,50)
  # we are using frequency cutoff as 50, there is no magic number here,
  # lower cutoffs will simply result in more number of dummy vars
}
glimpse(df)


#dropping na response rows in training set
df=df[!((is.na(df$Interest.Rate)) & df$data=='train'), ]


for(col in names(df)){
  if(sum(is.na(df[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    df[is.na(df[,col]),col]=mean(df[df$data=='train',col],na.rm=T)
  }
}
 
    

