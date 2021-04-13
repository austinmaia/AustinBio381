############################
# FUNCTION: getData
#Create simulated data set of dolphin whistles
#input: list of sizes, means, and SD for each group
#output: frame with randomized data in given range
#---------------------------
getData <- function(control=runif(500),
                     social=runif(500),
                     foraging=runif(500)) {
  
  frame1 <- data.frame(control, 
                       social, 
                       foraging)
  frame2 <- gather(frame1,
                   behavior,
                   freqeuncy,
                   control:foraging)
  
  return(frame2)  
}
############################


# ############################
## FUNCTION: anovaAnalysis
##Carry out ANOVA analysis
## input: frequency values, behavioral state, and data frame with all values
## output: anova analysis
##---------------------------
anovaAnalysis <- function(frequency=runif(500),
                          behavior=runif(500),
                          dataFrame=runif(1000)) {
  
  anova <- aov(frequency~behavior, data = dataFrame)
  
  
  return(anova)  
  
}
#############################

############################
# FUNCTION: plotANOVA
#Create box and whiskers plot of ANOVA analysis
#input: frequency values, behavior state, and anova results
#output: box and whiskers plot
#---------------------------
plotANOVA <- function(frequency=runif(500),
                      behavior=runif(500),
                      dataFrame=runif(1000)) {
  ANOPlot <- ggplot(data=dataFrame, aes(x=behavior, y=frequency, fill=behavior)) + geom_boxplot()
  
  print(ANOPlot)
}
############################

# SIMILAR FUNCTIONS

######################################
# FUNCTION: testSig
# test statistical significance of the code using ANOVA
# input: frequency values, behavioral state, data Frame
# output: pairwise significance and significance evaluation
testSig <- function(frequency=runif(500),
                        behavior=runif(500),
                        dataFrame=runif(1000)) {
  
  anovaRes <- aov(frequency~behavior, data = dataFrame)
  p <-  summary(anovaRes)[[1]][["Pr(>F)"]][[1]]
  if (p < 0.05){
    p <- paste(c("p (",p,") is statiscally significant"), collapse = " ")
  }
  else {
    p <- paste(c("p (",p,") is NOT statiscally significant"), collapse = " ")
  }
  return(p)
}
