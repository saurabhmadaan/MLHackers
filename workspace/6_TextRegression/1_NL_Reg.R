setwd('/Users/saurabh/Documents/RWorkSpace/Machine Learning/workspace/6_TextRegression')


# ---- plotting non-linear functions
set.seed(1)
x<-seq(-10,10,by=0.1)
y<-1-x^2+rnorm(length(x),0,5) # some random noise in y


ggplot(data.frame(X=x,Y=y),aes(x=X,y=Y))+
  geom_point() +
  geom_smooth(se=FALSE)

#------- using the poly fn for polynomial regression---
x<-seq(0,1,by=0.01)
y<-sin(2*pi*x)+rnorm(length(x),0,0.1)

df<-data.frame(X=x,Y=y)

ggplot(df,aes(x=X,y=Y))+
  geom_point()

ggplot(data.frame(X=x,Y=y),aes(x=X,y=Y))+
  geom_point() #+
  #geom_smooth(method='lm',se=FALSE)

df<-transform(df,X2=x^2)
df<-transform(df,X3=x^3)

#------------ regularization toy example
set.seed(1)

x<-seq(0,1,by=0.01)
y<-sin(2*pi*x)+rnorm(length(x),0,0.1)

n<-length(x)
indices<-sort(sample(1:n,round(0.5*n))) #sample(1:n); sample(1:n,10)

training.x<-x[indices]
training.y<-y[indices]

test.x<-x[-indices]
test.y<-y[-indices]

training.df<-data.frame(X=training.x,Y=training.y)
test.df<-data.frame(X=test.x,Y=test.y)


rmse<-function(y,h)
{
  return (sqrt(mean((y-h)^2)))
}

perf<-data.frame()

for (d in 1:12)
{
  poly.fit<-lm(Y~poly(X,degree=d),data=training.df)
  perf<-rbind(perf,data.frame(Degree=d,Data='Training',RMSE=rmse(training.y,predict(poly.fit))))
  
  perf<-rbind(perf,data.frame(Degree=d,Data='Test',RMSE=rmse(test.y,predict(poly.fit,newdata=test.df))))
}

perf


ggplot(perf,aes(x=Degree,y=RMSE,linetype=Data))+
  geom_point() +
  geom_line()


#---------------using glmnet

library(glmnet)
set.seed(1)

x<-seq(0,1,by=0.01)
y<-sin(2*pi*x)+rnorm(length(x),0,0.1)

x<-matrix(x)
glmnet(x,y)


set.seed(1)


x<-seq(0,1,by=0.01)
y<-sin(2*pi*x) + rnorm(length(x),0,0.1)

n<-length(x)
indices<-sort(sample(1:n,round(0.5*n)))

x.train<-x[indices]
y.train<-y[indices]

x.test<-x[-indices]
y.test<-y[-indices]


df<-data.frame(X=x,Y=y)
df.test<-data.frame(X=x.test,Y=y.test)
df.train<-data.frame(X=x.train,Y=y.train)

rmse<-function(y,h)
{
  return (sqrt(mean((y-h)^2)))
}

glmnet.fit<-with(df.train,glmnet(poly(X,degree=10),Y))
# summary(glmnet.fit)

lambdas<-glmnet.fit$lambda

for(lambda in lambdas)
{
}

#--------- text regularization --------
library(tm)

#------ 1) Prepare data
ranks<-read.csv('data/oreilly.csv',stringsAsFactors=FALSE)

df.txt<-data.frame(Text=ranks$Long.Desc.)
row.names(df.txt)<-1:nrow(df.txt)

corpus<-Corpus(DataframeSource(df.txt))
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus,removeWords,stopwords('english'))

dtm<-DocumentTermMatrix(corpus)

x<-as.matrix(dtm)
y<-rev(1:100)

#------ 2) split into training/test, and use glmnet for regularization







































