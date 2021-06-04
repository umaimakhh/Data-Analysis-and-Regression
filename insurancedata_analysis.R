library(MASS)

#identifying correlation
cor(insurance_dataset)
cor(insurance_dataset[c("age", "bmi", "children", "gender_num","smoker_num","region_num")])

#model summary and automatic feature selection
model <- lm(expenses ~ age+bmi+children+gender_num+smoker_num+region_num , data=insurance_dataset)
summary(model)
step <- (model, direction = "backward")

m6 <- lm(expenses ~ 1, data=insurance_dataset)
summary(m6)stepAIC

step <- stepAIC(m6,direction="forward", scope=list(upper=model,lower=m6)) # just for testing

step.model <- stepAIC(model, direction = "both", 
                      trace = FALSE)
summary(step.model)

model <- lm(expenses ~ age+bmi+children+smoker_num , data=insurance_dataset)
summary(model)

#data visualization
hist(insurance_dataset$expenses, col="yellow") 


#Bar Plot
counts <- table(insurance_dataset$children)
barplot(counts, main="Number of observations",ylab="Number of observations", xlab="Children", col=c("blue","green"))


#interaction plot
library(plotly)
demog_plot <- ggplot(insurance_dataset, aes(smoker, expenses, fill=smoker)) +
  geom_point(position = position_jitter(width= 0.2, height = 0), size = 2)
ggplotly(demog_plot)

correlationData <- insurance_dataset[,c(1,3,4,5,7,9,10)] #extracting nomial features
M<-cor(correlationData, method="spearman")
M
corrplot(M, method = "square")


library(GGally)
continousVariables <- insurance_dataset[,c(1,4,10)] #extracting scatterplots only for age,bmi,expense
ggpairs(continousVariables)
