
rm(list = ls())

library(readxl)

#preprocessing
##After testing models using a database without any variables with more than 15% of missing values,
##I decided to include Moisture (%) even though it has around 30% of nulls.
##
shelf_life_study_data_for_analytics_challenge_prediction <- read_excel("shelf-life-study-data-for-analytics-challenge_prediction.xlsx")
#View(shelf_life_study_data_for_analytics_challenge_prediction)
data <- shelf_life_study_data_for_analytics_challenge_prediction

##classifying obs as MSF - maximum shelf life
for (i in 1:nrow(data))
{
  if (data[i,7] >= 20)
    data[i,17] = "1"
  else 
    data[i,17] = "0"
}

data_clean_td <- data[,-c(7,8,9,10,12,14,15,16)]
colnames(data_clean_td) <- c("study.number", "sample.id", "product.type", "base.ingredient", "process.type",
                          "sample.age", "processing.agent", 
                          "moisture","not.fresh")


##deleting rows with missing values for base ingredient
missing <- which(is.na(data_clean_td$`base.ingredient`)== TRUE)
data_clean_td <- data_clean_td[-missing,]

##deleting rows with missing values for moisture
missing <- which(is.na(data_clean_td$moisture)== TRUE)
data_clean_td <- data_clean_td[-missing,]

data_clean_td$product.type <- as.factor(data_clean_td$product.type)
data_clean_td$base.ingredient <- as.factor(data_clean_td$base.ingredient)
data_clean_td$process.type <- as.factor(data_clean_td$process.type)
data_clean_td$not.fresh <- as.factor(data_clean_td$not.fresh)

##spliting training and testing data sets
set.seed(123)
proportion <- 0.70*nrow(data_clean_td) 
train_data <- data_clean_td[sample(1:nrow(data_clean_td),proportion, replace = FALSE),-c(1,2)]
test_data <- data_clean_td[-sample(1:nrow(data_clean_td),proportion, replace = FALSE),-c(1,2)]
summary(train_data)
summary(test_data)




#ML - Random Forest

tuneGrid <- data.frame(
  .mtry = c(1:17),
  .splitrule = "gini",
  .min.node.size = 16
)

model_rf <- train(not.fresh~., 
                  tuneGrid = tuneGrid,
                  data=train_data, 
                  method="ranger", 
                  trControl = trainControl
                  (
                    method = "cv",
                    number = 10,
                    verboseIter = TRUE
                  )
)
model_rf
model_rf[["results"]]


p<-predict.train(model_rf, test_data[,1:7], type = "raw")
confusionMatrix(p, reference = test_data$not.fresh)

library(caTools)

# Make ROC curve
colAUC(as.numeric(p), as.numeric(test_data$not.fresh), plotROC = TRUE)


##making predictions for the whole data set

p_full <- predict.train(model_rf, data_clean_td, type = "raw")
pred <- cbind(data_clean_td,p_full)

write.csv(pred, "C:/Users/alyss/Desktop/MS BAIS/Fall 2019/Pepsico/pred")



