## Homework 9 Work
#Preliminaries
library(ggplot2)
library(tidyverse)

source("functionsHW9.R")

#Global variables
control <- rnorm(n=500, mean=17000, sd=3000)
social<- rnorm(n=500, mean=16364, sd=3000)
foraging <-rnorm(n=500, mean=17101, sd=3000)

# run functions

meanFreq <- getData(control, social, foraging)

anovaData <- anovaAnalysis(meanFreq$freqeuncy, meanFreq$behavior, meanFreq)

plotANOVA(meanFreq$freqeuncy, meanFreq$behavior, anovaData)

testSig(meanFreq$freqeuncy, meanFreq$behavior, meanFreq)



