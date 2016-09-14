rm(list=ls())                                                                    #Clear workspace.

# Basic operations in R
2*3                                                                              #Pretty advanced stuff.
x <- 2; y <- 3; x*y                                                              #Store values and carry out operations.
y <- 1:5                                                                         #Store a sequence of values in a vector.
x*y                                                                              #Can do operation against the whole vector.
z <- x*y                                                                         #Can store the result also.
z                                                                                #How to see the result.
mean(y); sum(y); length(y)                                                       #Some basic functions.
tmp <- mean(y)                                                                   #You can store the result of a function also.
plot(y,z)                                                                        #How to plot the result.
plot(x,z)                                                                        #Let's get our first bug.
plot(rep(x,5), z)                                                                #One way to fix it...note embedded functions.
plot(rep(x,length(z)), z)                                                        #Even more embedded functions.
?plot                                                                            #Let's look at the help for plot.
plot(x=rep(x,length(z)), y=z)                                                    #It's good practice to make function calls easier to understand.
tmp <- data.frame(x, y, z)                                                       #Creating a table of our variables.
tmp                                                                              #Let's see what happened...
plot(x=tmp$x, y=tmp$z)                                                           #Now we can plot using the dataframe.
with(tmp, plot(x=x, y=z))                                                        #The same thing.
with(data.frame(x, y, z), plot(x=x, y=z))                                        #Ill-advised, but exercise in using what we've learned.
with(tmp, plot(x=x, y=z, main="Silly Plot", xlab="x-axis", pch=23))              #A couple more options.
rm(tmp,x)                                                                        #Removing one object.
rm(list=ls())                                                                    #Clear workspace.
# This has already been far more work than building a predictive model!


# Working with data.
rm(list=ls())                                                                    #Clear workspace.

dfd <- effects::TitanicSurvival                                                  #Get dataset to work with.
head(dfd)                                                                        #Look at the first rows of the dataset.
View(dfd)                                                                        #Look at the dataset as a pretty table.
str(dfd)                                                                         #View the structure of the object.
nrow(dfd)                                                                        #Get the number of rows.
summary(dfd)                                                                     #Produce a summary of the dataset.

table(dfd$sex)                                                                   #Get a basic frequency table.
with(dfd, table(sex, survived))                                                  #Cross sex with survival.
with(dfd, prop.table(table(sex, survived), margin=1))                            #Look at percent survival by sex.

mean(dfd$age)                                                                    #Hmmm...this doesn't give us what we expected.
mean(dfd$age, na.rm=TRUE)                                                        #Now we get it!
quantile(dfd$age, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)                    #Just as an aside...
sum(is.na(dfd$age))                                                              #Count the missing values for age.
mean(!is.na(dfd$age))                                                            #Percent of passengers for which age is known.

View(subset(dfd, age<1))                                                         #Get a subset (and view the dataset).
View(dfd[dfd$age<1,])                                                            #Same thing, harder to read, but important to understand...and a gotcha.
View(dfd[dfd$age<1 & !is.na(dfd$age),])                                          #How to get the 'right' answer.

# (Writing and) reading data.
rm(list=setdiff(ls(), "dfd"))                                                    #Remove everything except our dataframe.
tempdir()                                                                        #A temporary space, just so you know where it is.

write.table(dfd, file=paste(tempdir(), "/Titanic.txt", sep="")
           ,quote=FALSE, sep="\t", row.names=FALSE)                              #Write a tab delimited file.

tmp <- read.delim(file=paste(tempdir(), "/Titanic.txt", sep=""))                 #Read back in with a related function...that already has nice defaults.
summary(dfd); summary(tmp)                                                       #Quick peak to see similarity.

rm(list=setdiff(ls(), "dfd"))                                                    #Remove everything except our dataframe.
require(RMySQL)                                                                  #Load a package to connect to external database.
con <- dbConnect(MySQL(), host='cskpcloudxp0356.cloud.kp.org'
                ,user='ruser', password='ruser', dbname='tutorials')             #Connect to external database.
#dbWriteTable(con, "Titanic", dfd, overwrite = TRUE, row.names=FALSE)             #Writing data from R to a database.
dbListTables(con)                                                                #List tables in the database.
dbListFields(con,'Titanic')                                                      #List fields for the table of interest.
qry <- dbSendQuery(con, "SELECT * FROM Titanic;")                                #Build query to extract data.
tmp <- fetch(qry, n=-1)                                                          #Pull all the rows.
summary(dfd); summary(tmp)                                                       #Quick peak to see similarity.

rm(list=setdiff(ls(), "dfd"))                                                    #Remove everything except our dataframe.
tmp <- dfd; x <- 1:5; y <- LETTERS[1:5]                                                      #Create a sill 
save(tmp, x, y, file=paste(tempdir(), "/Titanic.Rdat", sep=""))                  #Store a bunch of stuff in a binary, Rdata format.
rm(list=setdiff(ls(), "dfd"))                                                    #Remove everything except our dataframe.
load(file=paste(tempdir(), "/Titanic.Rdat", sep=""))                             #Get back all the stuff.
summary(dfd); summary(tmp)                                                       #Quick peak to see similarity.

# Building our first model, dealing with non-linearity,
# and looking a informative missingness.
rm(list=ls())                                                                    #Clean workspace.
require(splines)                                                                 #Load a package for non-linear fits in many kinds of models.

dfd <- data.frame(x=-10:10)                                                      #Make a fake dataframe.
set.seed(621)                                                                    #Set a repeatable random seed.
dfd$y <- dfd$x^3 + 100*runif(nrow(dfd))                                          #Get y based upon x and some noise.
with(dfd, plot(x=x, y=y, xlab="Some X", ylab="Some Y", main="Some Plot"))        #Build the basic plot.

mdl <- lm(y ~ x, data=dfd)                                                       #Build a linear model.
summary(mdl)                                                                     #Get a summary of the model.
dfd$prd.lin <- predict(mdl)                                                      #Add the predicted values to the data.

mdl <- lm(y ~ ns(x,5), data=dfd)                                                 #Build a linear model...with a spline fit.
summary(mdl)                                                                     #Get a summary of the model.
dfd$prd.ns3 <- predict(mdl)                                                      #Add the predicted values to the data.

with(dfd, lines(x=x, y=prd.lin, col="black"))                                    #Add lines to see how the two model.
with(dfd, lines(x=x, y=prd.ns3, col="blue"))

# ...Now, let's build a model where we're missing data randomly.
set.seed(621)                                                                    #Reset seed for repeatability.
trn <- sample(1:nrow(dfd), size=nrow(dfd)/2)                                     #Specify a randomly selected training set.

mdl <- lm(y ~ ns(x,3), data=dfd[trn,])                                           #Build a linear model...with a spline fit...but only using random set.
summary(mdl)                                                                     #Get a summary of the model.
dfd$prd.msR <- predict(mdl)                                                      #Add the predicted values to the data...note error message
dfd$prd.msR <- predict(mdl, newdata=dfd)                                         #Get the predictions by specifying that we're using new data.
with(dfd[trn,], points(x=x, y=y, pch=19, col="black"))                           #Indicate which values were used to train.
with(dfd, lines(x=x, y=prd.msR, col="blue", lty=3))                              #See how model does.

dfd$nml <- with(dfd, ifelse(x <= 0, 1, 0))                                       #Create a new variable indicating normal values.
mdl <- lm(y ~ ns(x,3), data=dfd[nml==0,])                                        #Try to build model with 'abnormal' data and note it fails...
mdl <- lm(y ~ ns(x,3), data=subset(dfd, nml==0))                                 #Build a linear model...with a spline fit...but only using 'abnormal' values.
summary(mdl)                                                                     #Get a summary of the model.
dfd$prd.msI <- predict(mdl, newdata=dfd)                                         #Get predictions.
with(subset(dfd, nml==0), points(x=x, y=y, pch=22, cex=1.5, lwd=2, col="red"))   #Indicate which values were used.
with(dfd, lines(x=x, y=prd.msI, col="red"))                                      #See how the model does.

# Some notes on sampling...
sample(1:10, size=5)                                                             #Get a random sample of 5 digits from 10.
sample(1:10, size=5)                                                             #Get another random sample with the same call.

set.seed(621); sample(1:10, size=5)                                              #Specify seed to ensure results are replicable.
set.seed(621); sample(1:10, size=5)

sample(1:10, size=5, replace=TRUE)                                               #Basis of the bootstrap--sample with replacement.

# Let's see what chunk of the original sample gets included in the bootstrap.
# This also tells us what is left over for testing.
# Without losing any sample size.
# http://www.stat.washington.edu/courses/stat527/s13/readings/EfronTibshirani_JASA_1997.pdf

rm(list=ls())                                                                    #Clear workspace.
set.seed(621)                                                                    #Set the seed to ensure reproducibility.
x <- 1:1000                                                                      #Define a vector that's just a sequence.
unq <- cum <- rep(NA,1000)                                                       #Define two more vectors for sample used and cumulative mean.
for(i in 1:length(unq)) {                                                        #Loop through number of specified samples to draw...
   unq[i] <- length(unique(sample(x, replace=TRUE)))                             #Get a bootstrap sample and count the number of unique numbers.
   cum[i] <- mean(unq, na.rm=TRUE)                                               #Calculate the cumulative mean so far.
}
unq <- unq/length(x); cum <- cum/length(x)                                       #Divide by the original number of 'records' to get an inclusion rate.
mean(unq); tail(cum,1)                                                           #Display the mean of unique values and show this is the same as the last cumulative point.
plot(x=1:length(cum), y=cum, type="l", main="Getting a Sense of the Bootstrap"
    ,xlab="Bootstrap Sample (Cumulative)", ylab="% Included")                    #Plot the values.
mtext(paste("Mean Sample Percent: ", round(mean(unq)*100,2), "%", sep="")
      ,line=0.5, cex=0.9)                                                        #Provide more information in the plot.
grid()                                                                           #Provide a grid for reference.
abline(h=tail(cum,1), col="blue")                                                #Show the final mean.
lines(x=1:length(cum), y=cum)                                                    #Plot the line again so that it is clear.

# How about we build our first function?

fncBootTest <- function(n=1000, drw=1000, seed=621
                       ,mtxt_cex=0.7, ylm=NA) {
   set.seed(seed)                                                                #Set random number seed.
   x <- 1:n                                                                      #Define a vector that's just a sequence.
   unq <- cum <- rep(NA,drw)                                                     #Define two more vectors for sample used and cumulative mean.
   for(i in 1:length(unq)) {                                                     #Loop through number of specified samples to draw...
      unq[i] <- length(unique(sample(x, replace=TRUE)))                          #Get a bootstrap sample and count the number of unique numbers.
      cum[i] <- mean(unq, na.rm=TRUE)                                            #Calculate the cumulative mean so far.
   }
   unq <- unq/length(x); cum <- cum/length(x)                                    #Divide by the original number of 'records' to get an inclusion rate.
   mean(unq); tail(cum,1)                                                        #Display the mean of unique values and show this is the same as the last cumulative point.
   if(is.na(ylm[1])) ylm <- range(cum)                                           #Calculate y-limits if not provided.
   plot(x=1:length(cum), y=cum, type="l", main="Getting a Sense of the Bootstrap"
       ,xlab="Bootstrap Sample (Cumulative)", ylab="% Included"
       ,ylim=ylm)                                                                #Plot the values.
   mtext(paste("Mean Sample Percent: ", round(mean(unq)*100,2), "% "
              ,"with Sample=",prettyNum(n,big.mark=",")
              ," and Draw=",prettyNum(drw,big.mark=","), sep="")
         ,line=0.5, cex=mtxt_cex)                                                #Provide more information in the plot.
   grid()                                                                        #Provide a grid for reference.
   abline(h=tail(cum,1), col="blue")                                             #Show the final mean.
   lines(x=1:length(cum), y=cum)                                                 #Plot the line again so that it is clear.
}

par(mfrow=c(2,2))                                                                #Set up 2x2 plotting area.
fncBootTest(ylm=c(0.63, 0.64))                                                   #Run function with defaults (except y-limits).
fncBootTest(n=100, ylm=c(0.63, 0.64))                                            #Run function with different parameters.
fncBootTest(drw=100, ylm=c(0.63, 0.64))
fncBootTest(seed=628, ylm=c(0.63, 0.64))

