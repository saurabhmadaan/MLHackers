# open data source
setwd("/Users/saurabh/Documents/RWorkSpace/Machine Learning/workspace/1_Intro")
ufo<-read.delim('data/ufo/ufo_awesome.tsv',sep='\t',stringsAsFactors=FALSE,header=FALSE,na.strings='')
head(ufo)

# rename cols
names(ufo)<-c('DateOccurred','DateReported',
              'Location','ShortDescription',
              'Duration','LongDescription')
head(ufo)

# format dates
# ufo$DateOccurred<as.Date(ufo$DateOccurred,format='%Y%m%d') #errors out... data not good

# pick valid data
good.row.nums<-ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8,FALSE,TRUE)
length(which(!good.row.nums))# how many rows are not good
ufo<-ufo[good.row.nums,] # keep only the good rows

# format dates now
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")


# test_str="Iowa City, IA"
# x = strsplit(test_str,",")
# x 
# [[1]]
# [1] "Iowa City" " IA" 
# x[[1]][1] is "Iowa City"
# x[[1]][2] is " IA"
# now, to clear the space in " IA"
# gsub("^ ",""," IA") 
# "IA"
# '^' matches the first character
# e.g. ('^x' matches 'x' for an expression beginning with 'x')

get.location<-function(input_str)
{
  loc_str<-tryCatch(strsplit(input_str,',')[[1]],
              error=function(e) return(c(NA,NA)))
  clean.location<-gsub('^ ','',loc_str)         
  if(length(clean.location)>2){
    return(c(NA,NA))
  }  
  else{
    return(clean.location)
  }
}

city.state<-lapply(ufo$Location,get.location) #getting city, state for US-like inputs
# output is a list
# head(city.state)
# [[1]]
# [1] "Iowa City" "IA"       
# [[2]]
# [1] "Milwaukee" "WI"       

# convert list to matrix of 2 columns, using row bind
location.matrix<-do.call(rbind,city.state)

# head(location.matrix)
# [,1]               [,2]
# [1,] "Iowa City"        "IA"
# [2,] "Milwaukee"        "WI"
# [3,] "Shelton"          "WA"
# [4,] "Columbia"         "MO"

# now incorporate this in to the ufo data frame
ufo<-transform(ufo,USCity=location.matrix[,1],USState=tolower(location.matrix[,2]),
               stringsAsFactors=FALSE)

# removing those records that do not have a valid us state
us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct",
               "de", "fl", "ga", "hi", "ia", "id", "il",
               "in", "ks", "ky", "la", "ma", "md", "me",
               "mi", "mn", "mo", "ms", "mt", "nc", "nd",
               "ne", "nh", "nj", "nm", "nv", "ny", "oh",
               "ok", "or", "pa", "ri", "sc", "sd", "tn",
               "tx", "ut", "va", "vt", "wa", "wi", "wv",
               "wy")
# match(ufo$USState,us.states) -- returns a list of nums between 1 and 50 (2nd vector  match index)
# -- NA if no match
ufo$USState<- us.states[match(ufo$USState,us.states)]
ufo$USCity[is.na(ufo$USState)]<-NA
ufo.us<-subset(ufo,!is.na(USState))
head(ufo.us)

summary(ufo.us$DateOccurred)

# plot histogram of occurrence wrt date
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks =date_breaks("50 years"),labels = date_format("%Y"))

#save plot
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 6,
       width = 8)


#most sightings between 1960 and 2010, we are interested in records 1990 or later
ufo.us<-subset(ufo.us,DateOccurred>=as.Date('1990-01-01'))
nrow(ufo.us)

# add a month,yr column
ufo.us$YearMonth<-strftime(ufo.us$DateOccurred,format='%Y%m')




