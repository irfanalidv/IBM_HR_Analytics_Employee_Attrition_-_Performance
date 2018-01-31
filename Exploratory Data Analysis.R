suppressMessages(library(ggplot2))
suppressMessages(library(grid))
#A rewrite of the graphics layout capabilities, plus some support for interaction.
suppressMessages(library(gridExtra))
#Provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables.
suppressMessages(library(plyr))
#Tools for Splitting, Applying and Combining Data
suppressMessages(library(rpart))
#Recursive partitioning for classification, regression and survival trees
suppressMessages(library(rpart.plot))
#An Enhanced Version of 'plot.rpart'
#install.packages("rpart.plot")
suppressMessages(library(randomForest))
#Classification and regression based on a forest of trees using random inputs.
suppressMessages(library(caret))
#Misc functions for training and plotting classification and regression models.
suppressMessages(library(gbm))
#Generalized Boosted Regression Models
suppressMessages(library(survival))
#Contains the core survival analysis routines, including definition of Surv objects, 
#Kaplan-Meier and Aalen-Johansen (multi-state) curves, Cox models, and parametric accelerated failure time models.
suppressMessages(library(pROC))
#Tools for visualizing, smoothing and comparing receiver operating characteristic 
#(ROC curves). (Partial) area under the curve (AUC) can be compared with statistical 
#tests based on U-statistics or bootstrap. Confidence intervals can be computed for (p)AUC or ROC curves.
suppressMessages(library(DMwR))
#Functions and data for "Data Mining with R"
#install.packages("DMwR")
suppressMessages(library(scales))
#Scale Functions for Visualization
ibm<- WA_HR_Employee_Attrition

g1 <- ggplot(ibm, 
             aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g2 <- ggplot(ibm, 
             aes(x = HourlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g3 <- ggplot(ibm, 
             aes(x = DailyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g4 <- ggplot(ibm, 
             aes(x = MonthlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

# 
# ggplot(ibm, 
#        aes(y = YearsSinceLastPromotion, x = YearsAtCompany, colour = OverTime)) + 
#   geom_jitter(size = 1, alpha = 0.7) + 
#   geom_smooth(method = "gam") + 
#   facet_wrap(~ Attrition) + 
#   ggtitle("Attrition") + 
#   scale_colour_manual(values = c("#386cb0","#fdb462")) + 
#   theme(plot.title = element_text(hjust = 0.5))

ggplot(ibm, 
       aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "OverTime") +
  facet_grid(~Attrition) +
  scale_fill_manual(values = c("#386cb0","#fdb462")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

##work life balance

ggplot(ibm, 
       aes(x= WorkLifeBalance, y=DistanceFromHome, group = WorkLifeBalance, fill = WorkLifeBalance)) + 
  geom_boxplot(alpha=0.7) + 
  theme(legend.position="none") + 
  facet_wrap(~ Attrition) + 
  ggtitle("Attrition") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ibm, 
       aes(x= BusinessTravel,  group=Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill="Business Travel") +
  facet_grid(~Attrition) +
  scale_y_continuous(labels=percent) + 
  scale_fill_manual(values = c("#386cb0","#ef3b2c", "#fdb462")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")