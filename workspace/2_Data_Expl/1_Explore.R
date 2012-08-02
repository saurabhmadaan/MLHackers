library('ggplot2')
setwd("/Users/saurabh/Documents/RWorkSpace/Machine Learning/workspace/2_Data_Expl")

#read data
data.file<-file.path('data','01_heights_weights_genders.csv')
head(heights.weights)

#plot as a histogram
ggplot(heights.weights,aes(x=Height)) +
  geom_histogram(binwidth=1)

#oversmooth
ggplot(heights.weights,aes(x=Height)) +
  geom_histogram(binwidth=5)

#undersmooth
ggplot(heights.weights,aes(x=Height)) +
  geom_histogram(binwidth=0.001)


#plot as a density plot
ggplot(heights.weights,aes(x=Height)) +
  geom_density()

# flatness in top of density plot, plot by gender var to see any cause
ggplot(heights.weights,aes(x=Weight, fill=Gender)) +
  geom_density()

# got 2 overlapping curves. now try to see in isolation
ggplot(heights.weights,aes(x=Weight, fill=Gender)) +
  geom_density() +
  facet_grid(Gender ~ .)
# gives a faceted plot

# overview of relationship between variables
pairs(heights.weights)

# Weight vs Height scatterplot
ggplot(heights.weights,aes(x=Height,y=Weight))+
  geom_point()


# Visual depiction... linear... Weight vs Height 
ggplot(heights.weights,aes(x=Height,y=Weight))+
  geom_point()+
  geom_smooth()

# With fewer observation... linear approx gets weaker
ggplot(heights.weights[1:200,],aes(x=Height,y=Weight))+
  geom_point()+
  geom_smooth()


# Classification... see effect of label/parameter
ggplot(heights.weights,aes(x=Height,y=Weight,color=Gender))+
  geom_point()


