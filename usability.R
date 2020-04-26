

# Load data
df = read.csv(file="./data/usability.csv", header=TRUE, sep=",")
df[,2] = ifelse(df[,2]==1, 'AR', ifelse(df[,2]==2, 'INTERACTIVE', 'STATIC'))
colnames(df)

df$USABILITY_O = as.numeric(df$USABILITY_O)
df$USABILITY_T = as.numeric(df$USABILITY_T)
df$USABILITY_S = as.numeric(df$USABILITY_S)

# Standard Error Usability Olympics
sd(df$USABILITY_O)/sqrt(length(df$USABILITY_O))
# Standard Error Usability Thai
sd(df$USABILITY_T)/sqrt(length(df$USABILITY_T))
# Standard Error Usability Syria
sd(df$USABILITY_S)/sqrt(length(df$USABILITY_S))

summary(df)


