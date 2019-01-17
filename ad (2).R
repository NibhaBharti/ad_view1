#Convert duration into seconds in excel
'
Function CalcSeconds(ByVal value As String) As Long
Dim ending As String
ending = Right(value, 1)
value = Replace(value, "PT", "")
value = Replace(value, "H", "*3600" + IIf(ending = "M" Or ending = "S", "+", ""))
value = Replace(value, "M", "*60" + IIf(ending = "S", "+", ""))
CalcSeconds = Evaluate(Replace(value, "S", ""))
End Function 

'
#Read Datasets
train1 <- read.csv('train.csv')
test1 <- read.csv('test.csv')

#Remove Vidid and Published variables
train <- train1[,-c(1,7)]
test <- test1[,-c(1,6)]

library(dplyr)

#Change inappropriate factor columns to numeric
fac1.names = train %>% select_if(is.factor) %>% colnames()
fac2.names = test %>% select_if(is.factor) %>% colnames()

train[,fac1.names] = data.frame(sapply(train[,fac1.names], as.numeric))
test[,fac2.names] = data.frame(sapply(test[,fac2.names], as.numeric))

library(dummies)

#One hot encoding for Categorical Variables
train <- dummy.data.frame(train,"category")
test <- dummy.data.frame(test,"category")

#Avoiding Dummy trap
train <- train[,-14]
test <- test[,-13]

library(xgboost)
library(Metrics)
set.seed(123)
## Model Building
xgbFit = xgboost(data = as.matrix(train[, -1]), nfold = 5, label = as.matrix(log(train$adview)), 
                 nrounds = 1000, verbose = TRUE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01,max_depth = 12)
#Model specifications
print(xgbFit)

## Predictions
adview <- ceiling(exp(predict(xgbFit, newdata = as.matrix(test))))

#Solution File
sample <- cbind(test1['vidid'], adview)

write.csv(sample, 'solution.csv', row.names=FALSE)
