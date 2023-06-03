library(dplyr)

# read in data
s_train &lt;- read.csv(&quot;store_train.csv&quot;)
s_test &lt;- read.csv(&quot;store_test.csv&quot;)

#summary of data
glimpse(s_train)
glimpse(s_test)
#Data Preparation
s_test$store=NA
s_train$data=&quot;train&quot;
s_test$data=&quot;test&quot;
s=rbind(s_train,s_test)
glimpse(s)
str(s)
table(s$country)
table(s$State)
s$store=as.factor(s$store)
glimpse(s)
#Next we will convert all categorical variables to dummies.
#We will write a function which will take care of that instead of converting them one
by one.
CreateDummies=function(data,var,freq_cutoff=0){
t=table(data[,var])
t=t[t&gt;freq_cutoff]
t=sort(t)
categories=names(t)[-1]
for( cat in categories){
name=paste(var,cat,sep=&quot;_&quot;)
name=gsub(&quot; &quot;,&quot;&quot;,name)
name=gsub(&quot;-&quot;,&quot;_&quot;,name)
name=gsub(&quot;\\?&quot;,&quot;Q&quot;,name)
name=gsub(&quot;&lt;&quot;,&quot;LT_&quot;,name)
name=gsub(&quot;\\+&quot;,&quot;&quot;,name)
name=gsub(&quot;\\/&quot;,&quot;_&quot;,name)
name=gsub(&quot;&gt;&quot;,&quot;GT_&quot;,name)
name=gsub(&quot;=&quot;,&quot;EQ_&quot;,name)
name=gsub(&quot;,&quot;,&quot;&quot;,name)
data[,name]=as.numeric(data[,var]==cat)
}
data[,var]=NULL

return(data)
}
#categorical variables by writing following lines of codes
names(s)[sapply(s,function(x) is.character(x))]

#length of var
length(unique(s$countyname))
length(unique(s$storecode))
length(unique(s$Areaname))
length(unique(s$countytownname))
length(unique(s$state_alpha))
length(unique(s$store_Type))

#We will ignore columns or variables like
countyname,storecode,Areaname,countytownname for their High-Cardinality.
#Further we will ignore data column for obvious reason.
s=s %&gt;% select(-countyname,-storecode,-Areaname,-countytownname)

#Above codes will discard those four variables &amp; we are left with 14 variables now.
#Next Let us make dummies for the rest of columns - state_alpha &amp; store_Type.
cat_cols=c(&quot;state_alpha&quot;,&quot;store_Type&quot;)
for(cat in cat_cols){
s=CreateDummies(s,cat,100)
}
glimpse(s)

#Let us see if there is any missing values in our data..
lapply(s,function(x) sum(is.na(x)))
#From above we can see that We do have missing values in columns like country,
population &amp; store.
#Next we impute those missing values with the mean of train data as shown below.
for(col in names(s)){
if(sum(is.na(s[,col]))&gt;0 &amp; !(col %in% c(&quot;data&quot;,&quot;store&quot;))){
s[is.na(s[,col]),col]=mean(s[s$data==&#39;train&#39;,col],na.rm=T)
}
}

#We can always cross check if those NAs has been replaced with mean or not by using
lapply function again.
lapply(s,function(x) sum(is.na(x)))
#Now we are done with data preparation , lets separate the data next.

s_train=s %&gt;% filter(data==&quot;train&quot;) %&gt;% select(-data)
s_test=s %&gt;% filter(data==&quot;test&quot;) %&gt;% select(-data,-store)
#Next we will break our train data into 2 parts. We will build model on one part &amp;
check its performance on the other.
set.seed(2)
s=sample(1:nrow(s_train),0.8*nrow(s_train))
s_train1=s_train[s,]
s_train2=s_train[-s,]

#Model Building
library(randomForest)
#Next we will build our model with 5 variables randomly subsetted at each node i.e
mtry &amp; let just say we want to grow 100 such trees.
model_rf=randomForest(store~.-Id,data=s_train1,mtry=5,ntree=100)
model_rf
#Model Validation
#Lets see performance of this model on the validation data s_train2 that we kept
aside.
val.score=predict(model_rf,newdata=s_train2,type=&#39;response&#39;)
#Again we need to check the accuracy using confusionMatrix from caret package.
#What we will get is an accuracy of 78% which seems to be a fair model.
library(caret)
confusionMatrix(val.score,s_train2$store)
#Now let us calculate probability score for our validation data set s_train2.
val.prob_score=predict(model_rf,newdata=s_train2,type=&#39;prob&#39;)

#In order to check the performance of our model let us calculate its auc score.
#For that we need to first import a package named ‘pROC’.
library(pROC)
auc_score=auc(roc(s_train2$store,val.prob_score[,1]))

#From above it is clear that the auc score or the tentative score performance of our
model is going to be around 0.82
plot(roc(s_train2$store,val.prob_score[,1]))

#Next we will build the random forest model on the entire training data set ‘s_train’
&amp; predict the same on test data set ‘s_test’
model_rf_final=randomForest(store~.-Id,data=s_train,mtry=5,ntree=100)
model_rf_final
#We will now use this model to predict probability score for test data .
test.score=predict(model_rf_final,newdata = s_test,type=&#39;prob&#39;)[,1]

test.score
#Variable Importance
#We will run below codes to find out the importance of variable.
#Higher the mean decrease ginni for any variable better is the variable for prediction.
So population is the most important variable.
d=importance(model_rf_final)
d=as.data.frame(d)
d$VariableNames=rownames(d)
d %&gt;% arrange(desc(MeanDecreaseGini))
#Upon plotting we get a plot like this.
varImpPlot(model_rf_final)
