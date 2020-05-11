install.packages("arules")
library(arules)
data(AdultUCI)
dim(AdultUCI)
AdultUCI[1:3,]

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0, 25, 40, 60, 168)), labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]], c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]), Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]], c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]), Inf)), labels = c("none", "low", "high"))
Adult <- as(AdultUCI, "transactions")
Adult

summary(Adult)

aa=as(Adult,"matrix")
## transforms transaction matrix into an incidence matrix
aa[1:2,] # print the first two rows of the incidence matrix

itemFrequencyPlot(Adult[, itemFrequency(Adult) > 0.2], cex.names = 1)

rules <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))
rules
summary(rules)

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
inspect(sort(rulesIncomeSmall, by = "confidence")[1:3])

rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)
inspect(sort(rulesIncomeLarge, by = "confidence")[1:3])
