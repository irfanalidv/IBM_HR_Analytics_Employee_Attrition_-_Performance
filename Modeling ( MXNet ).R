#install and load neural network packages
#install.packages("mxnet")
require(mxnet)

#encode labels as a factor & view proportion of each class "Yes", "No".
factor(WA_HR_Employee_Attrition$Attrition)
prop.table(table(WA_HR_Employee_Attrition$Attrition))
#out[]
# No       Yes 
# 0.8387755 0.1612245 

#train, test split: create the training and test data sets 

#train, test split: create the training and test data sets 
Attr_train <- train
Attr_train$Attrition<-NULL
Attr_train <- data.matrix(Attr_train)
Attr_test <- test
Attr_test$Attrition<- NULL
Attr_test <- data.matrix(Attr_test)

Attr_train_labels <- train$Attrition
Attr_test_labels <- test$Attrition

#Build the neural network model with 10 hidden nodes
mx.set.seed(123)

model <- mx.mlp(Attr_train, Attr_train_labels, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.accuracy)

#view the computation graph: will show fully connected layers, activation i.e tanh, relu etc and the softmaxoutput()
graph.viz(model$symbol)

#make predictions on the test set
predict_attr = predict(model, Attr_test)
#evaluate model accuracy of class probabilities
#creates confusion matrix
predict.label = max.col(t(predict_attr))-1
table(predict.label, Attr_test_labels)
      