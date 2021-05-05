# HW 11 Work

#1. Repeat the exercise from the Batch Processing Lecture (13 April), but do it using real data sets rather than purely simulated. Check with folks in your lab to see if there are multiple data sets available for analysis, or ask Nick or Lauren for suggestions for other data sources. Stick to simple data analyses and graphics, but try to set it up as a batch process that will work on multiple files and save summary results to a common file.

library(matrixStats)
#----------------------------------
# FUNCTION contour_parameter_extraction
# description: Take raw values of extracted contour from Luscinia software and pull out basic descriptive stats of each whistle
# inputs: file_name = csv file, behavioral state (character string), group ID (character string)
# outputs: list containing start value, end value, delta, and mean for each whistle of given parameter
###################################
contour_parameter_extraction <- function(d=NULL, behavior=NULL, groupID = NULL) {
  start <- d[,2]
  end <- d[,51]
  delta <- rep(NA, nrow(d))
  for (i in seq_along(start)){
    delta[i] <- abs(end[i]-start[i])
  }
  mean <- rowMeans(d[,2:51])
  sd <- rowSds(as.matrix(d[,2:51]))
  parameter_list <- data.frame(behavior=behavior,
                        group=groupID,
                        start=start,
                        end=end,
                        delta=delta,
                        mean=mean,
                        sd=sd)
  return(parameter_list)
  
} # end of contour_parameter_extraction
#----------------------------------

#----------------------------------
# FUNCTION reg_stats
# description: fit linear models, extract model stats
# inputs: 6-column data frame, desired x (dependent) column number and desired y (independent) column number
# outputs: slope, p-value, and r2
###################################
reg_stats <- function(d=NULL, y=NULL, x=NULL) {
  . <- lm(data=d,d[,y]~d[,x])
  . <- summary(.)
  stats_list <- list(Slope=.$coefficients[2,1],
                     pVal=.$coefficients[2,4],
                     r2 = .$r.squared)
  return(stats_list)
} # end of reg_stats
#----------------------------------

# Data import - creating new files for each group using parameter extraction function
group1for <- read.csv("hw11Files/FF/sotaliaG1ForFF.csv")
g1forFF <- contour_parameter_extraction(group1for, "Foraging","Group1")

group1soc <- read.csv("hw11Files/FF/sotaliaG1SocFF.csv")
g1socFF <- contour_parameter_extraction(group1soc, "Social","Group1")

group1trav <-read.csv("hw11Files/FF/sotaliaG1TravFF.csv")
g1travFF <- contour_parameter_extraction(group1trav, "Traveling","Group1")

group3for <- read.csv("hw11Files/FF/sotaliaG3ForFF.csv")
g3forFF <- contour_parameter_extraction(group3for, "Foraging","Group3")

group6trav <- read.csv("hw11Files/FF/sotaliaG6TravFF.csv")
g6travFF <- contour_parameter_extraction(group6trav, "Traveling","Group6")

group5Trav <- read.csv("hw11Files/FF/sotaliaG5TravFF.csv")
g5travFF <- contour_parameter_extraction(group5Trav, "Traveling","Group5")

group7Trav <- read.csv("hw11Files/FF/sotaliaG7TravFF.csv")
g7travFF <- contour_parameter_extraction(group7Trav, "Traveling","Group7")

group1FF <- rbind(g1forFF, g1socFF, g1travFF)

write.table(x=group1FF,
            file="hw11Files/cleaned/group1FF.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)
travFF <- rbind(g1travFF,g6travFF,g5travFF,g7travFF)
write.table(x=travFF,
            file="hw11Files/cleaned/travelingFF.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)
allFF <- rbind(g1forFF, g1socFF, g3forFF, g1travFF,g6travFF,g5travFF,g7travFF)
write.table(x=allFF,
            file="hw11Files/cleaned/allSotaliaFF.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)

#################################################################
# Batch Processing

library(TeachingDemos)
char2seed("Flatpicking solo")

#############################
#Global variables
file_folder <- "hw11Files/cleaned/"
file_out <- "StatsSummary1.csv"
treat_col <- 6
resp_col <- 7
############################

#Call data
file_names <- list.files(path=file_folder)

#Create a data frame to hold summary file statistics
ID <- seq_along(file_names)
file_name <- file_names
slope <- rep(NA,length(file_names))
p_val <- rep(NA,length(file_names))
r2 <- rep(NA,length(file_names))

stats_out <- data.frame(ID,file_name,slope,p_val,r2)

for(i in seq_along(file_names)){
  data <- read.table(file=paste(file_folder, file_names[i], sep=""), sep=",",
                     header=TRUE)
  d_clean <- data[complete.cases(data),]
  . <- reg_stats(d_clean, treat_col, resp_col)
  stats_out[i,3:5] <- unlist(.)
}
write.table(cat("# summary stats for ",
                "batch processing of regression models",
                "\n",
                "for columns ", treat_col, " and ", resp_col,
                "#timestamp: ", as.character(Sys.time()),
                "\n",
                file=file_out,
                row.names="",
                col.names="",
                sep=""))

#add data frame
write.table(x=stats_out,
            file=file_out,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)


