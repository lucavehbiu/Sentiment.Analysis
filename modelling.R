#~~~~~~~~~~~~~~~~~~~~~~
#Sentiment analysis - Luca Vehbiu
#06/02/2018
#~~~~~~~~~~~~~~~~~~~~~~~

#Load libraries
require(pacman)
p_load(tidyverse, ggplot2, doParallel, modelr, caret, plotly)

#Set up parallel processing
detectCores()
cl <- makeCluster(4) #make the cluster
registerDoParallel(cl) #register the cluster
getDoParWorkers() #check how many
stopCluster(cl)


##read the small matrix datas
iphone <- read.csv("iphone_smallmatrix_labeled_8d.csv", header = T)
galaxy <- read.csv("galaxy_smallmatrix_labeled_8d.csv", header = T)

##read the validation sets obtained from AWS EMR
first.run <- read.csv("firstrun.csv", header = T)
second.run <- read.csv("secondrun.csv", header = T)
third.run <- read.csv("thirdrun.csv", header = T)

valid <- rbind(first.run, second.run, third.run) #bind them together


#add predictions of iphonesentiment
add_predictions(valid, model_rf) %>% plyr:: rename(c("pred" = "iphonesentiment")) ->  valid
#add predictions of galaxysentiment
add_predictions(valid, model_galaxy_rf) %>% plyr:: rename(c("pred" = "galaxysentiment")) ->  valid

valid %>% group_by(iphonesentiment, galaxysentiment) %>% count()


ggplot(valid) + geom_bar(aes(iphonesentiment, fill = "blue")) + 
  geom_bar(aes(galaxysentiment, fill = 'green'), position = 'dodge')


#check distribution of phone sentiments
plot_ly(iphone, x = ~iphonesentiment,  type='violin')
plot_ly(valid, x = ~iphonesentiment,  type='histogram', color = "green")

plot_ly(galaxy, x = ~galaxysentiment,  type='violin')
plot_ly(valid, x = ~galaxysentiment,  type='histogram')


#under and over sample --> To correct for imbalancedness
no.redundant %>% filter(iphonesentiment == 2) -> dupli#duplicate

dupli <- dupli[sample(1:nrow(dupli), 1500),]
no.redundant <- rbind(no.redundant, dupli)


##no redundant models
no.redundant$iphonesentiment <- as.factor(no.redundant$iphonesentiment)

set.seed(568)

trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)
a <- createDataPartition(y = no.redundant$iphonesentiment, p = 0.75, list = F)
train <- no.redundant[a,]
test <- no.redundant[-a,]
set.seed(122)

#rf
model_rf <- train(iphonesentiment ~., data = train,
      method = "rf",
      trControl = trctrl, importance = T, allowParallel = T, tuneGrid = grid)

grid <- expand.grid(size = c(1:10), decay = c(0.1, 0.2))

#c5.0
model_cart <- train(iphonesentiment ~., data = train,
                    method = "C5.0",
                    trControl = trctrl, allowParallel = T, importance = T)

#svmliner and neural net
model_net <- train(iphonesentiment ~., data = train,
                   method = "nnet",
                   trControl = trctrl, allowParallel = T, importance = T)

model_svm <- train(iphonesentiment ~., data = train,
                   method = "svmLinear3",
                   trControl = trctrl, allowParallel = T, importance = T)


 #bwplot the results
results <- resamples(list("Random Forest" = model_rf, 
                          "Neural Net" = model_net,
                          C5.0 = model_cart,
                          SVM = model_svm))
bwplot(results) 


#predictions and confusion matrix           
pred <- predict(model_rf, newdata = test)
confusionMatrix(pred, test$iphonesentiment)

#confusion matrix using ggplot2
as.data.frame(table(pred, test$iphonesentiment)) -> confusion_matrix

ggplot(data = confusion_matrix,
       mapping = aes(x = pred,
                     y = confusion_matrix$Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%2.0f", Freq)), vjust = 2) +
  scale_fill_gradient(low = "lightblue",
                      high = "orange",
                      trans = "log") +
  labs( x = "Predictions", y = "Actual Values", title = "iPhone") +
  theme(legend.position = "none")



####no redundant models for galaxy phone
no.redundant_galaxy$galaxysentiment <- as.factor(no.redundant_galaxy$galaxysentiment)

set.seed(579)

trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)
a <- createDataPartition(y = no.redundant_galaxy$galaxysentiment, p = 0.75, list = F)
training <- no.redundant_galaxy[a,]
testing <- no.redundant_galaxy[-a,]

set.seed(1225)
#rf
model_galaxy_rf <- train(galaxysentiment ~., data = training,
               method = "rf",
               trControl = trctrl, allowParallel = T, importance = T, tuneGrid = grid)

grid <- expand.grid(mtry = c(1:11))

#c5.0
model_cart.galaxy <- train(galaxysentiment ~., data = training,
                    method = "C5.0",
                    trControl = trctrl, allowParallel = T, importance = T)

#svmliner and kknn
model_svm.galaxy <- train(galaxysentiment ~., data = training,
                   method = "kknn",
                   trControl = trctrl, allowParallel = T, importance = T)

#tuning
grid <- expand.grid(kmax = c(1:20), distance = c(1:10), kernel = "optimal")                     

#predictions and confusion matrix           
pred <- predict(model_cart.galaxy, newdata = testing)
confusionMatrix(pred, testing$galaxysentiment)






