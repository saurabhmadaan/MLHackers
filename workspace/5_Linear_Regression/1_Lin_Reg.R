setwd('/Users/saurabh/Documents/RWorkSpace/Machine\ Learning/workspace/5_Linear_Regression')
library('ggplot2')
dt<-read.csv('data/longevity.csv')


# Look at the data
head(dt)
summary(dt)
pairs(dt)

# plot and play
ggplot(dt,aes((x=AgeAtDeath),fill=factor(Smokes))) + 
  geom_density() +
  facet_grid(Smokes ~ .)

Smokers<-dt[which(dt$Smokes==1),]
NonSmokers<-dt[which(dt$Smokes==0),]

ggplot(Smokers,aes((x=Smokers$AgeAtDeath))) + 
  geom_density() 

ggplot(NonSmokers,aes((x=NonSmokers$AgeAtDeath))) + 
  geom_density()


# Squared Errors (Avg age of a person, assume we know nothing else)
guess<-70
sq.error<-with(dt,mean((AgeAtDeath-guess)^2))

guess<-80
sq.error<-with(dt,mean((AgeAtDeath-guess)^2))

# how does SE vary with guess
guesses<-seq(63,83,by=1)

guess.accuracy<-data.frame()
for(guess in guesses)
{
  sq.error<-with(dt,mean((AgeAtDeath-guess)^2))
  df<-data.frame(Guess=guess,Error=sq.error)
  guess.accuracy<-rbind(guess.accuracy,df)
}

ggplot(guess.accuracy,aes(x=Guess,y=Error)) + 
  geom_point() +
  geom_line()
# plot clearly shows that Squared error is minimum at the mean :)

# Error using the mean as guess
guess<-mean(dt$AgeAtDeath)
root.error.mean<-with(dt,sqrt(mean((AgeAtDeath-guess)^2)))
#  [1] 5.737096


#Smoker guess
smoker.guess<-mean(subset(dt,Smokes==1)$AgeAtDeath)

#NonSmoker guess
nonsmoker.guess<-mean(subset(dt,Smokes==0)$AgeAtDeath)

#Add Custom column (smoker mean for smokers, nonsmoker mean for nonsmokers)
dt$customguess<-ifelse(dt$Smokes==1,smoker.guess,nonsmoker.guess)

# Error using custom guess (smoker mean for smokers, nonsmoker mean for nonsmokers)
root.error.custom<-with(dt,sqrt(mean((AgeAtDeath-customguess)^2)))
# [1] 5.148622


#-------------- using lm ~ ----------------

hw<-read.csv('data/01_heights_weights_genders.csv',header=TRUE,sep=',')
# head(hw)
# pairs(hw)

# plot a linear regression estimate, using geom_smooth with method: lm
ggplot(hw, aes(x=Height,y=Weight))+
  geom_point()+
  geom_smooth(method='lm')



# creating a lin reg  obj

lin.reg<-lm(Weight~Height,data=hw)
lin.reg

# --------- sites data ---- 
top1ksites<-read.csv('data/top_1000_sites.tsv',sep='\t',stringsAsFactors=FALSE)

head(top1ksites)
ggplot(top1ksites,aes(x=PageViews,y=UniqueVisitors)) + geom_point() #no clear pattern discernible due to scale


ggplot(top1ksites,aes(y=log(PageViews),x=log(UniqueVisitors))) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE)   


lm.fit<-lm(log(PageViews)~log(UniqueVisitors),data=top1ksites)

summary(lm.fit)





