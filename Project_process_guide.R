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


#dropping NA response rows in training set
df=df[!((is.na(df$Interest.Rate)) & df$data=='train'), ]

#imputing mean values
for(col in names(df)){
  if(sum(is.na(df[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    df[is.na(df[,col]),col]=mean(df[df$data=='train',col],na.rm=T)
  }
}
 

#separating train and test dataset
train=df %>% filter(data=='train') %>% select(-data)
test=df %>% filter(data=='test') %>% select(-data,-Interest.Rate)
vis_dat(df)




#MODEL BUILDING STARTS
#a simple linear regression model 

for_vif=lm(Interest.Rate~.-ID - Amount.Requested - Employment.Length_10years,data=train)
# excluded ID from the modeling process

sort(vif(for_vif),decreasing = T)[1:3]
rm(for_vif)
fit=lm(Interest.Rate~.-ID - Amount.Requested - Employment.Length_10years,data=train)
fit=stats::step(fit)
# there is no step function for algos such as dtree, rf or gbm
# variable selection happens there naturally as part of the process
summary(fit)

#PREDICTION
test.predictions=predict(fit,newdata=test)
write.csv(test.predictions,'proper_name.csv',row.names = F)
# dont be lazy chose a proper name as instructed in the problem statement page






## CLASSIFICATION------------------------------------------


bd_train=read.csv("bd_train.csv",stringsAsFactors = F)
bd_test=read.csv("bd_test.csv",stringsAsFactors = F)
bd_test$Revenue.Grid=NA
bd_train$data='train'
bd_test$data='test'
bd_all=rbind(bd_train,bd_test)
glimpse(bd_all)


bd_all=bd_all %>% select(-post_code,-post_area) #unwanted features drop

#dummies create
char_logical=sapply(bd_all,is.character)
cat_cols=names(bd_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data','Revenue.Grid'))]
cat_cols

for(col in cat_cols){
  bd_all=CreateDummies(bd_all,col,500)
}

#dropping empty respone rows
bd_all=bd_all[!((is.na(bd_all$Revenue.Grid)) & bd_all$data=='train'), ]
#imputing missing values
for(col in names(bd_all)){
  if(sum(is.na(bd_all[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    bd_all[is.na(bd_all[,col]),col]=mean(bd_all[bd_all$data=='train',col],na.rm=T)
  }
}

bd_train=bd_all %>% filter(data=='train') %>% select(-data)
bd_test=bd_all %>% filter(data=='test') %>% select(-data,-Revenue.Grid)



for_vif=lm(Revenue.Grid~.-REF_NO
           -Investment.in.Commudity
           -Investment.in.Derivative
           -Investment.in.Equity
           -Portfolio.Balance
           ,data=bd_train)
sort(vif(for_vif),decreasing = T)[1:3]




#model building
bd_train$Revenue.Grid=as.numeric(bd_train$Revenue.Grid==1)
fit=glm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
        -Investment.in.Derivative
        -Investment.in.Equity
        -Portfolio.Balance, data=bd_train, family='binomial')


test.probs=predict(fit,newdata=bd_test,type='response')
# different algos/functions will have different ways of extracting probabilities
# its mentioned in the project problem statement page as well
write.csv(test.probs,'proper_name.csv',row.names = F)

#Do the steps to measure a good cutoff here and then run the following stepsc
test.class=as.numeric(predict(fit,newdata=bd_test,type='response')>0.3)
test.class=ifelse(test.class==1,'Yes','No')
write.csv(test.class,'proper_name.csv',row.names = F)
