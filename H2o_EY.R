#H2o NOTE:
#Training Frame
iris_frames.L103295<-h2o.splitFrame(data=iris.L103295,ratios=0.8,see=125)
iris.train.L103295=iris_frames.L103295[[1]]                                    
iris.train.L103295=h2o.assign(iris.train.L103295,"iris.train.L103295")

nrow(iris.train.L103295)
head(iris.train.L103295)

#test frame
iris.test.L103295=iris_frames.L103295[[2]]                                    
iris.test.L103295=h2o.assign(iris.test.L103295,"iris.test.L103295")

nrow(iris.test.L103295)
head(iris.test.L103295)

nrow(iris.train.L103295)+nrow(iris.test.L103295)
#
##################################################################
# This is a demo of H2O's Capabilities as part of introductory class
# Iris example
# Update Author : Taposh Roy
##################################################################

#H2O cloud is running so load the library
library(h2o)

#Connect to the ip address provided to your login.
h2o.init(ip="", port=, strict_version_check= FALSE)


nrow(iris)

#iris adding an ID column
iris$id <- c(1:150)

head(iris)

#Move the sample Iris data-set to H2O data frame
#use your nuid. I am using mine L103295
iris.L103295 = as.h2o(iris)

#Check the number of rows.
nrow(iris.L103295)

#Split the frames into test and train
iris_frames.L103295 <- h2o.splitFrame(data = iris.L103295 , ratios = 0.8, seed=125)

#Training Frame
iris.train.L103295 = iris_frames.L103295[[1]]
iris.train.L103295 = h2o.assign(iris.train.L103295, "iris.train")
nrow(iris.train.L103295)
head(iris.train.L103295)

#Testing Frame
iris.test.L103295 = iris_frames.L103295[[2]]
iris.test.L103295 = h2o.assign(iris.test.L103295, "iris.test")

#Check total rows
nrow(iris.train.L103295) + nrow(iris.test.L103295)

#Gradient Boosting Machines Model
# We run the GBM model
iris.gbm.L103295 = h2o.gbm(x = 1:4, y = 5, training_frame = iris.train.L103295)
#Look at the model
print(iris.gbm.L103295)

#Save Model
mygbmpath.L103295 <- h2o.saveModel(iris.gbm.L103295,path= "/tmp",force = TRUE)

#Load Model
newgbmobj.L103295 <- h2o.loadModel(mygbmpath.L103295)

#Prediction
iris.predict.L103295 <- h2o.predict(iris.gbm.L103295,iris.test.L103295)

#Look at prediction
head(iris.predict.L103295$setosa)

#convert from h2o frame
pred <- as.data.frame(iris.predict.L103295$setosa)
head(pred)
#Model Performance
#h2o.performance(model, newdata = NULL, train = FALSE, valid = FALSE,

#                xval = FALSE, data = NULL) 
perf <- h2o.performance(model = iris.gbm.L103295, newdata=iris.train.L103295)
h2o.mse(perf)

perf.test <- h2o.performance(model = iris.gbm.L103295, newdata=iris.test.L103295)
h2o.mse(perf.test)


#Creating a submission file
testid<-as.data.frame(iris.test.L103295$id)
submit <- cbind(testid,pred)

#write out the submission file
write.csv(submit,file="submit_iris.csv",row.names = FALSE)
