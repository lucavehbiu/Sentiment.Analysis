#PreProcess

#order column names
no.redundant %>% select(order(colnames(.))) -> no.redundant

#hot-encoding
iphoneRC$iphonesentiment <- as.factor(iphone$iphonesentiment)

no.redundant <- no.redundant %>% 
  mutate_at("iphonesentiment", 
            funs(recode(., '0' = 1,  '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4, 
                        .default = NaN)))

no.redundant_galaxy <- no.redundant_galaxy %>% 
  mutate_at("galaxysentiment", 
            funs(recode(., '0' = 1,  '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4, 
                        .default = NaN)))


##Recursive feature elimination
#######
### Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = FALSE)
no.redundant_galaxy_sample <- no.redundant_galaxy[sample(1:nrow(no.redundant_galaxy), 1000, replace=FALSE),]

### Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(no.redundant_galaxy_sample[,1:58], 
                  no.redundant_galaxy_sample$galaxysentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl, allowParallel = T)

### Get results
rfeResults

### Plot results
plot(rfeResults, type=c("g", "o"))

predictors(rfeResults)

### add the dependent variable to iphoneRFE
#########

##create the non-redundant dataset (obtained from step above)
no.redundant <- iphone %>% 
  dplyr:: select(c("samsunggalaxy","iphone", "iphonedisunc", "htcphone" ,
                   "googleandroid","iphonedisneg", "iphoneperpos", "sonyxperia", 
                   "iphonedispos","iphonecamneg" , "iphonecamunc" , "ios" ,           
                   "htcdispos", "iphoneperneg", "htccampos" , "iphoneperunc" ,   
                   "htcperpos", "iphonecampos", "htcdisneg" , "iphonesentiment")) #iphone

no.redundant_galaxy <- galaxy %>% 
  dplyr:: select(c("iphone" ,   "samsunggalaxy", "googleandroid", "htcphone", "iphonedisunc", "iphoneperpos",
                   "htcdispos", "sonyxperia",    "iphoneperneg",  "iphonedisneg",  "iphonedispos" , "htcperpos" ,   
                   "htccampos", "iphonecamneg",  "ios" ,  "iphonecampos",  "iphoneperunc" , "htcperneg",    
                   "iphonecamunc", "galaxysentiment"))




#remove near zero variance rows
no.redundant <- no.redundant %>% dplyr:: select(-iphonesentiment)


no.redundant[rowSums(no.redundant) == 0 | rowSums(no.redundant) == 1,] <- NA

#take those fucking outliers out
no.redundant[no.redundant >= 60] <- NA

no.redundant$iphonesentiment <- iphone$iphonesentiment #add the dependent var back
no.redundant <- drop_na(no.redundant) # drop rows where their sum is 1



#remove near zero variance rows for galaxy as well
no.redundant_galaxy <- no.redundant_galaxy %>% dplyr:: select(-galaxysentiment)


no.redundant_galaxy[rowSums(no.redundant_galaxy) == 0 | rowSums(no.redundant_galaxy) == 1,] <- NA

#take  those fucking outliers out
no.redundant_galaxy[no.redundant_galaxy >= 60] <- NA

no.redundant_galaxy$galaxysentiment <- galaxy$galaxysentiment #add the dependent var back
no.redundant_galaxy <- drop_na(no.redundant_galaxy) # drop rows where their sum is 1



##remove near zero variance rows for validation set as well
valid <- valid %>% dplyr:: select(-c( id))


valid[rowSums(valid) == 0 | rowSums(valid) == 1,] <- NA
valid[valid >= 60] <- NA

valid$iphonesentiment <- valid$iphonesentiment #add the dependent var back
valid$galaxysentiment <- valid$galaxysentiment
valid <- drop_na(valid) # drop rows where their sum is 1



#remove highly correlated features
require(corrplot)
no.redundant_galaxy$galaxysentiment <- as.numeric(no.redundant_galaxy$galaxysentiment)


cor <- cor(no.redundant_galaxy)

corrplot(cor, addCoef.col = "black", order = "AOE")

#remove correlated values
data.new <- no.redundant[,!apply(cor,2,function(x) any(x > 0.9))]



      
#create object containing centered, scaled PCA components from training set

# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(training[,-20], 
                               method=c("center", "scale", "pca"), thresh = 0.94)
print(preprocessParams)

train.pca <- predict(preprocessParams, training[,-20])

# add the dependent to train
train.pca$galaxysentiment <- training$galaxysentiment

# use predict to apply pca parameters, create test, exclude dependant
test.pca <- predict(preprocessParams, testing[,-20])

# add the dependent to train
test.pca$galaxysentiment <- testing$galaxysentiment

train.pca$galaxysentiment <- as.factor(train.pca$galaxysentiment)
test.pca$galaxysentiment <- as.factor(test.pca$galaxysentiment)

# inspect results
str(train.pca)
str(test.pca)
