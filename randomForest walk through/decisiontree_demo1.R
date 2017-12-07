library(faraway)
library(rpart)

# faraway package data :: https://cran.r-project.org/web/packages/faraway/faraway.pdf

doctor.data <- esdcomp
happy.little.tree <- rpart(complaints ~  revenue + residency + gender, data = doctor.data, method = "anova")

# Let's Plot The Decision Tree
plot(happy.little.tree, uniform=TRUE, main="Regression Tree for Complaints ")
text(happy.little.tree, use.n=TRUE, all=TRUE, cex=.8)

# Show split data on residency.
sum(doctor.data$complaints[doctor.data$residency == "N"])

# Let's look at the average complaints for residency with the revenue split.
#mean(doctor.data$complaints[doctor.data$residency == "Y" & doctor.data$revenue<238])
#mean(doctor.data$complaints[doctor.data$residency == "Y" & doctor.data$revenue>238])

#printcp(happy.little.tree)