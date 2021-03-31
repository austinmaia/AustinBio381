## Homework 8 Work
# H0 - Whistles in social and foraging contexts will have the same average fundamental frequency.
# H1 - Whistles in the social and foraging contexts will have different fundamental frequencies.
# ANOVA
# group 1 - control, group 2 - social, group 3 - foraging

library(tidyverse)

nGroup <- 3
nName <- c("Control","Social","Foraging")
nSize <- c(500,500,500)
nMean <- c(17000, 16364, 17101)
# Trial 1 nMean <- c(16760, 14000, 19000) - sig
# Trial 2 nMean <- c(16760, 15000, 18000) - sig
# Trial 3 (actual means) nMean <- c(17000, 16364, 17101) - sometimes sig, sometimes not (depends on data generation)
# Trial 4 nMean <- c(16760, 16500, 16900) - still sometimes sig, usually not
# Trial 5 nMean <- c(16760, 16600, 16800) - not sig

nSD <- c(3000, 3000, 3000)

ID <- 1:(sum(nSize))
resVar <- c(rnorm(n=nSize[1],mean=nMean[1], sd = nSD[1]),
            rnorm(n=nSize[2],mean=nMean[2], sd = nSD[2]),
            rnorm(n=nSize[3],mean=nMean[3], sd = nSD[3]))
TGroup <- rep(nName, nSize)
ANOdata <- data.frame(ID,TGroup,resVar)
str(ANOdata)

ANOmodel <- aov(resVar~TGroup,data=ANOdata)
print(ANOmodel)

ANOPlot <- ggplot(data=ANOdata, aes(x=TGroup, y=resVar, fill=TGroup)) + geom_boxplot()

print(ANOPlot)
summary(ANOmodel)


