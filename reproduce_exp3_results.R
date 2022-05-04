library(plyr)
library(stringr)
library(afex)
library(superb)
library(gridExtra)


#LOAD PJ TRIMMING ROUTINE
source("pjTrimmingV2.R")

file_list = list.files(path = "Experiment 3 Data/sjDataFiles", full.names = T)

#...only keep the filenames that are "SJ" and ".txt"
file_list = file_list[str_detect(file_list,pattern="SJ") & str_detect(file_list,pattern=".txt")]

#...OPEN THE INDIVIDIDUAL DATA FILES AND COMBINE THEM
mergedDataSet <- ldply(file_list, 
                       read.delim, 
                       header=FALSE, 
                       stringsAsFactors = FALSE,
                       sep = "")  #for each item in the list apply the function read.delim

#..ADD HEADERS
names(mergedDataSet) = c("sj",
                         "cb",
                         "blockNumber",
                         "blockType",
                         "trialNum",
                         "target",
                         "targetImage",
                         "distractor",
                         "distractorImage",
                         "posture",
                         "setSize",
                         "rt",
                         "resp",
                         "cresp",
                         "ac")

mergedDataSet <- mergedDataSet[!mergedDataSet$blockType=="practice",]

##remove RTs below 100 ms (doesn't seem like there are any)

mergedDataSet <- mergedDataSet %>%
  filter(rt > 100) %>%
  mutate(pure.error = 1-ac, 
         error.w.timeouts = ifelse(rt < 1500, 1-ac, 1))

error_means <- mergedDataSet %>%
  group_by(sj, setSize, posture) %>%
  summarize(meanPE = 100 * mean(pure.error),
            meanPE.w.timeouts = 100 * mean(error.w.timeouts))

error_means_wide <- error_means %>%
  pivot_wider(names_from = c("setSize", "posture"), 
              values_from = c("meanPE", "meanPE.w.timeouts"))
write.csv(error_means_wide, file = "error_means.csv", row.names = F)

exp3_anova_pe <- aov_ez(data = error_means,
                        dv = 'meanPE.w.timeouts',
                        id = 'sj',
                        within = c('posture', 'setSize'),
                        type = 3,
                        anova_table = list(es = "pes")
)
  
acc_plot1 <-
  superbPlot(error_means_wide,
             WSFactors = c("Posture(2)", "SetSize(2)"),
             variables = colnames(error_means_wide)[2:5],
             errorbar = "SE",
             plotStyle = "line",
             factorOrder = c("SetSize","Posture"),
             adjustments = list(purpose = "difference"))+
  theme_classic() +
  scale_x_discrete(labels=c("1" = "Set Size 4", "2" = "Set Size 8")) +
  scale_color_discrete(labels = c("Sitting", "Standing")) +
  ylim(0, 5.5) +
  labs(y = "Percent Error")

acc_plot2 <-
  superbPlot(error_means_wide,
             WSFactors = c("Posture(2)", "SetSize(2)"),
             variables = colnames(error_means_wide)[6:9],
             errorbar = "SE",
             plotStyle = "line",
             factorOrder = c("SetSize","Posture"),
             adjustments = list(purpose = "difference"))+
  theme_classic() +
  scale_x_discrete(labels=c("1" = "Set Size 4", "2" = "Set Size 8")) +
  scale_color_discrete(labels = c("Sitting", "Standing")) +
ylim(0,5.5) +
labs(y = "Percent Error (including timeouts)")

grid.arrange(acc_plot1, acc_plot2, ncol=2)
