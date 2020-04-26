

# Load data
df = read.csv(file="./data/relevancy_accuracy.csv", header=TRUE, sep=",")
df[,2] = ifelse(df[,2]==1, 'AR', ifelse(df[,2]==2, 'INTERACTIVE', 'STATIC'))

###
#  Does sense of presence correlate with relevancy
###

#
# Sense of presence and relevancy for AR condition
#

# Olympics
summary(Kendall(df[df$CONDITION=='AR', 'PRESENCE_TOTAL_OLYM'], df[df$CONDITION=='AR', 'Rel_O']))
# tau = -0.0394, 2-sided pvalue =0.77843

# Syria
summary(Kendall(df[df$CONDITION=='AR', 'PRESENCE_TOTAL_SYRIA'], df[df$CONDITION=='AR', 'Rel_S']))
# tau = 0.0709, 2-sided pvalue =0.60967

# Thai
summary(Kendall(df[df$CONDITION=='AR', 'PRESENCE_TOTAL_THAI'], df[df$CONDITION=='AR', 'Rel_T']))
# tau = 0.0298, 2-sided pvalue =0.83634


#
# Sense of presence and relevancy for Interactive condition
#
summary(Kendall(df[df$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_OLYM'], df[df$CONDITION=='INTERACTIVE', 'Rel_O']))
# tau = 0.0309, 2-sided pvalue =0.84264

# Syria
summary(Kendall(df[df$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_SYRIA'], df[df$CONDITION=='INTERACTIVE', 'Rel_S']))
# tau = -0.00309, 2-sided pvalue =1

# Thai
summary(Kendall(df[df$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_THAI'], df[df$CONDITION=='INTERACTIVE', 'Rel_T']))
# tau = 0.0586, 2-sided pvalue =0.69141

#
# Sense of presence and relevancy for Static condition
#
# Olympics
summary(Kendall(df[df$CONDITION=='STATIC', 'PRESENCE_TOTAL_OLYM'], df[df$CONDITION=='STATIC', 'Rel_O']))
# tau = -0.364, 2-sided pvalue =0.014004 **

# Syria
summary(Kendall(df[df$CONDITION=='STATIC', 'PRESENCE_TOTAL_SYRIA'], df[df$CONDITION=='STATIC', 'Rel_S']))
# tau = -0.281, 2-sided pvalue =0.059177

# Thai
summary(Kendall(df[df$CONDITION=='STATIC', 'PRESENCE_TOTAL_THAI'], df[df$CONDITION=='STATIC', 'Rel_T']))
# tau = 0.229, 2-sided pvalue =0.12396


###
#  Does sense of presence correlate with accuracy
###

#
# Sense of presence and accuracy for AR condition
#
# Olympics
summary(Kendall(df[df$CONDITION=='AR', 'PRESENCE_TOTAL_OLYM'], df[df$CONDITION=='AR', 'Acc_O']))
# tau = -0.099, 2-sided pvalue =0.46412

# Syria
summary(Kendall(df[df$CONDITION=='AR', 'PRESENCE_TOTAL_SYRIA'], df[df$CONDITION=='AR', 'Acc_S']))
# tau = 0.00247, 2-sided pvalue =1

# Thai
summary(Kendall(df[df$CONDITION=='AR', 'PRESENCE_TOTAL_THAI'], df[df$CONDITION=='AR', 'Acc_T']))
# tau = -0.0297, 2-sided pvalue =0.83641

#
# Sense of presence and accuracy for Interactive condition
#
# Olympics
summary(Kendall(df[df$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_OLYM'], df[df$CONDITION=='INTERACTIVE', 'Acc_O']))
# tau = -0.115, 2-sided pvalue =0.42704

# Syria
summary(Kendall(df[df$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_SYRIA'], df[df$CONDITION=='INTERACTIVE', 'Acc_S']))
# tau = -0.0247, 2-sided pvalue =0.87735

# Thai
summary(Kendall(df[df$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_THAI'], df[df$CONDITION=='INTERACTIVE', 'Acc_T']))
# tau = 0.105, 2-sided pvalue =0.46667

#
# Sense of presence and accuracy for Static condition
#
# Olympics
summary(Kendall(df[df$CONDITION=='STATIC', 'PRESENCE_TOTAL_OLYM'], df[df$CONDITION=='STATIC', 'Acc_O']))
# tau = -0.25, 2-sided pvalue =0.091561

# Syria
summary(Kendall(df[df$CONDITION=='STATIC', 'PRESENCE_TOTAL_SYRIA'], df[df$CONDITION=='STATIC', 'Acc_S']))
# tau = -0.214, 2-sided pvalue =0.15012

# Thai
summary(Kendall(df[df$CONDITION=='STATIC', 'PRESENCE_TOTAL_THAI'], df[df$CONDITION=='STATIC', 'Acc_T']))
# tau = -0.393, 2-sided pvalue =0.0079143 **


# Convert columns to numeric
# Relevance
df$Rel_O = as.numeric(df$Rel_O)
df$Rel_T = as.numeric(df$Rel_T)
df$Rel_S = as.numeric(df$Rel_S)
# Accuracy
df$Acc_O = as.numeric(df$Acc_O)
df$Acc_T = as.numeric(df$Acc_T)
df$Acc_S = as.numeric(df$Acc_S)

summary(df)

# Standard Error Accuracy Olympics
sd(df$Acc_O)/sqrt(length(df$Acc_O))
# Standard Error Accuracy Thai
sd(df$Acc_T)/sqrt(length(df$Acc_T))
# Standard Error Accuracy Syria
sd(df$Acc_S)/sqrt(length(df$Acc_S))

# Standard Error Relevance Olympics
sd(df$Rel_O)/sqrt(length(df$Rel_O))
# Standard Error Relevance Thai
sd(df$Rel_T)/sqrt(length(df$Rel_T))
# Standard Error Relevance Syria
sd(df$Rel_S)/sqrt(length(df$Rel_S))
