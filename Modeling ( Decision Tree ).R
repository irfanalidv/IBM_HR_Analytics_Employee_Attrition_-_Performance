set.seed(3221)

# Getting rid of long variable names & certain unuseful variables 
unique(ibm$JobRole)
levels(ibm$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(ibm$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
ibm <- ibm[c(-9,-10,-22,-27)]


n <- nrow(ibm)
rnd <- sample(n, n * .70)
train <- ibm[rnd,]
test <- ibm[-rnd,]

colnames(train)
feature.names<-names(train[])
feature.names

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    train[[f]] <- as.integer(train[[f]])
    test[[f]]  <- as.integer(test[[f]])
  }
}

# Modeling 
dtree <- rpart(Attrition ~., data = train)
preds <- predict(dtree, test)
#preds <- predict(dtree, test, type = "class")

rocv <- roc(as.numeric(test$Attrition), as.numeric(preds))
rocv$auc
#Area under the curve: 0.6967

prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)

#Prunning & plotting the tree

dtreepr <- prune(dtree, cp = 0.01666667)
predspr <- predict(dtreepr, test)
#predspr <- predict(dtreepr, test, type = "class")

rocvpr <- roc(as.numeric(test$Attrition), as.numeric(predspr))
rocvpr$auc
#Area under the curve: 0.662

rpart.plot(dtreepr, 
           type = 4, 
           extra = 104, 
           tweak = 0.9, 
           method = "anova",
           fallen.leaves = F)
#Error: prp: illegal argument "method"
