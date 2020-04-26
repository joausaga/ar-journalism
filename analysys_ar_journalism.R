# Script that analyzes the effect of 
# augmented reality in journalisms

# Load libraries
library(foreign)
library(stringr)
library(reshape)
library(phia)
library(ARTool)
library(emmeans)
library(nlme)
library(coin)
library(Kendall)
library(dplyr)
library(lattice)
library(FSA)

do_analysis = function(cols, analysis_name) {
  # Select interested columns
  df_sp = df_c[,cols]
  # Rename columns
  colnames(df_sp) = c('subject','modality','olympic', 'syria', 'thai')
  # Melting data
  m_df_sp = melt(df_sp, id=c('subject', 'modality'))
  colnames(m_df_sp) = c('subject', 'modality', 'piece', 'value')
  # Plot interactions
  with(m_df_sp, interaction.plot(x.factor=piece, trace.factor=modality, 
                                 response=value, main=analysis_name))
  # Define model
  m = art(value ~ piece * modality + (1|subject), data=m_df_sp)
  # Conduct anova
  print(anova(m))
  # Analyze difference in interactions
  print(testInteractions(artlm(m, "piece:modality"), pairwise=c("piece", "modality"), 
                         adjustment="holm"))
}

cor_kendall = function(df, independent_vars, depedendent_vars) {
  conditions = c('AR', 'INTERACTIVE', 'STATIC')
  stories = c('OLYM', 'SYRIA', 'THAI')
  significant_cors = data.frame(matrix(ncol = 6, nrow = 0))
  sc_cols = c('i_var', 'd_var', 'story', 'condition', 'tau', 'p_value')
  colnames(significant_cors) = sc_cols
  for (i_var in independent_vars) {
    for (d_var in depedendent_vars) {
      for (story in stories) {
        for (condition in conditions) {
          writeLines(paste0('Independent variable: ', i_var, ''))
          writeLines(paste0('Dependenent variable: ', d_var))
          writeLines(paste0('Story: ', story))
          writeLines(paste0('Condition: ', condition))
          d_var_concat = paste0(d_var, story)
          r = Kendall(df[df$CONDITION==condition, i_var], df[df_c$CONDITION==condition, d_var_concat])
          tau = as.numeric(r$tau)
          p_value = as.numeric(r$sl)
          if (p_value < 0.05) {
            writeLines(paste0('tau: ', tau, ' (p-value: ', p_value, ')*'))
            significant_cor = data.frame(i_var=i_var, d_var=d_var, story=story, condition=condition, tau=tau, p_value=p_value)
            significant_cors = rbind(significant_cors, significant_cor)
          } else {
            writeLines(paste0('tau: ', tau, ' (p-value: ', p_value, ')'))
          }
        }
        cat('\n')
      }
      cat('\n')
    }
    cat('\n')
  }
  return(significant_cors)
}

# Load data
df = read.csv(file="./data/AR_ALL_DATA_ADDITIONS_LAST.csv", header=TRUE, sep=";")

# Prepare data
remove_comma = function(col) {
  return (as.numeric(gsub(',', '.', col)))
}
df_c = cbind(as.data.frame(apply(df[,c(1:4)], 2, as.factor)),
             as.data.frame(apply(df[,c(5:ncol(df))], 2, remove_comma)))
df_c[,2] = ifelse(df[,2]==1, 'AR', ifelse(df[,2]==2, 'INTERACTIVE', 'STATIC'))

#######
## Evaluating Sense of Presence
#######
i_cols = c('SUBJECT','CONDITION','PRESENCE_TOTAL_OLYM', 'PRESENCE_TOTAL_SYRIA', 
           'PRESENCE_TOTAL_THAI')

i_cols = c('SUBJECT','CONDITION', 'SCREEN_TIME_VISUALS_O', 
           'SCREEN_TIME_VISUALS_S', 'SCREEN_TIME_VISUALS_T')

i_cols = c('SUBJECT','CONDITION', 'USAB_hedon_O', 'USAB_hedon_T', 
           'USAB_hedon_S')

V = df_c[, i_cols]


i_cols = c('SUBJECT','CONDITION', 'SCREEN_TIME_TEXT_O', 
           'SCREEN_TIME_TEXT_S', 'SCREEN_TIME_TEXT_T')

v = V[complete.cases(V), ]

colnames(df_c)[grep('filter*', colnames(df_c), ignore.case = T)]

df_c[,'filter_.']

m_df_sp$modality = as.factor(m_df_sp$modality)


m = art(value ~ modality * piece + (1|subject), data=m_df_sp)
summary(m)
anova(m)
aov(m)

lme1 = lme(value ~ modality * piece, random= ~1|subject, data=m_df_sp)
summary(lme1)
Anova(lme1, type=c("III"))

vignette("art-contrasts")

testInteractions(artlm(m, "modality:piece"), pairwise=c("modality", "piece"), 
                 adjustment="holm")




View(df_c[,c('SUBJECT','CONDITION','PRESENCE_TOTAL_OLYM', 'PRESENCE_TOTAL_SYRIA', 
        'PRESENCE_TOTAL_THAI')])

View(cbind(m$data[,c('subject', 'modality', 'piece', 'value')], m$cell_means))




m.linear = lm(value ~ modality*piece, data=m_df_sp)
contrast(emmeans(m.linear, ~ modality:piece), method="pairwise", interaction=TRUE)

p_data = m_df_sp
a = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "thai",]$value, 
            p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "thai",]$value, 
            paired=FALSE)
b= wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "thai",]$value, 
            p_data[p_data$modality == "STATIC" & p_data$piece == "thai",]$value, 
            paired=FALSE)
c=wilcox.test(p_data[p_data$modality == "STATIC" & p_data$piece == "thai",]$value, 
            p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "thai",]$value, 
            paired=FALSE)
p.adjust(c(a$p.value, b$p.value, c$p.value), method="holm")


a = wilcox.test(p_data[p_data$modality == "AR",]$value, 
                p_data[p_data$modality == "INTERACTIVE",]$value, 
                paired=FALSE)
b= wilcox.test(p_data[p_data$modality == "AR",]$value, 
               p_data[p_data$modality == "STATIC",]$value, 
               paired=FALSE)
c=wilcox.test(p_data[p_data$modality == "STATIC",]$value, 
              p_data[p_data$modality == "INTERACTIVE",]$value, 
              paired=FALSE)
p.adjust(c(a$p.value, b$p.value, c$p.value), method="holm")


a = wilcox.test(p_data[p_data$piece == "thai",]$value, 
                p_data[p_data$piece == "olympic",]$value, 
                paired=TRUE)
b= wilcox.test(p_data[p_data$piece == "thai",]$value, 
               p_data[p_data$piece == "syria",]$value, 
               paired=TRUE)
c=wilcox.test(p_data[p_data$piece == "syria",]$value, 
              p_data[p_data$piece == "thai",]$value, 
              paired=TRUE)
p.adjust(c(a$p.value, b$p.value, c$p.value), method="holm")

a = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "syria",]$value, 
                p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "syria",]$value, 
                paired=FALSE)
b= wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "syria",]$value, 
               p_data[p_data$modality == "STATIC" & p_data$piece == "syria",]$value, 
               paired=FALSE)
c=wilcox.test(p_data[p_data$modality == "STATIC" & p_data$piece == "syria",]$value, 
              p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "syria",]$value, 
              paired=FALSE)
p.adjust(c(a$p.value, b$p.value, c$p.value), method="holm")


a = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "olympic",]$value, 
                p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "olympic",]$value, 
                paired=FALSE)
b= wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "olympic",]$value, 
               p_data[p_data$modality == "STATIC" & p_data$piece == "olympic",]$value, 
               paired=FALSE)
c=wilcox.test(p_data[p_data$modality == "STATIC" & p_data$piece == "olympic",]$value, 
              p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "olympic",]$value, 
              paired=FALSE)
p.adjust(c(a$p.value, b$p.value, c$p.value), method="holm")


d = list(c('AR', 'INTERACTIVE'), c('AR', 'STATIC'), c('INTERACTIVE', 'STATIC'))
d[[1]]
d[2]
d[3]

for (i in 1:length(d)) {
  print(d[[i]])
  print(d[[i]][1])
  print(d[[i]][2])
}

a = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "olympic",]$value, 
                p_data[p_data$modality == "AR" & p_data$piece == "thai",]$value, 
                paired=TRUE)
b = wilcox.test(p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "olympic",]$value, 
                p_data[p_data$modality == "INTERACTIVE" & p_data$piece == "thai",]$value, 
                paired=TRUE)
p.adjust(c(a$p.value, b$p.value), method="holm")


a = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "olympic",]$value, 
                p_data[p_data$modality == "AR" & p_data$piece == "thai",]$value, 
                paired=TRUE)
b = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "olympic",]$value, 
                p_data[p_data$modality == "AR" & p_data$piece == "syria",]$value, 
                paired=TRUE)
c = wilcox.test(p_data[p_data$modality == "AR" & p_data$piece == "thai",]$value, 
                p_data[p_data$modality == "AR" & p_data$piece == "syria",]$value, 
                paired=TRUE)
p.adjust(c(a$p.value, b$p.value, c$p.value), method="holm")

colnames(df_c)

View(df_c[,c('SUBJECT','REMEM_TEXT_24_O', 'REMEM_VISUAL_24_O', 'REMEM_BOTH_24_O')])



###
# Differences between pre and post per condition per article
###
# Wilconox SIGNED-RANK to analyze the 
pre_pos_effect = function(df, condition, x_column, y_column) {
  df_f = filter(df,CONDITION==condition)
  pre = data.frame(cbind(df_f[,x_column]), rep('Pre', nrow(df_f)))
  colnames(pre) = c('score', 'when')
  pre$score = as.numeric(pre$score)
  print(mean(pre$score, na.rm=T))
  pos = data.frame(cbind(df_f[,y_column]), rep('Pos', nrow(df_f)))
  colnames(pos) = c('score', 'when')
  pos$score = as.numeric(pos$score)
  print(mean(pos$score, na.rm=T))
  full = data.frame(rbind(pre, pos))
  print(wilcoxsign_test(score ~ when, data=full, distribution="exact", paired=T))
}

# Thai and AR
pre_pos_effect(df_c, 'AR', 'KNOWS_THAI', 'PerceiveLearnGeneral_T')
# Mean KNOWS_THAI 4.27
# Mean PerceiveLearnGeneral_T 4.51
# Z = 6.3801, p-value = 1.749e-14

# Syria and AR
pre_pos_effect(df_c, 'AR', 'KNOWS_SYRIA', 'PerceiveLearnGeneral_S')
# Mean KNOWS_SYRIA 3.53
# Mean PerceiveLearnGeneral_S 4.03
# Z = 6.0262, p-value = 4.151e-12

# Olympic and AR
pre_pos_effect(df_c, 'AR', 'KNOWS_OLYMP', 'PerceiveLearnGeneral_O')
# Mean KNOWS_OLYMP 2.45
# Mean PerceiveLearnGeneral_O 3.18
# Z = 5.5024, p-value = 1.198e-09

# Thai and Interactive
pre_pos_effect(df_c, 'INTERACTIVE', 'KNOWS_THAI', 'PerceiveLearnGeneral_T')
# Mean KNOWS_THAI 4.70
# Mean PerceiveLearnGeneral_T 4.5
# Z = 6.1872, p-value = 5.329e-15

# Syria and Interactive
pre_pos_effect(df_c, 'INTERACTIVE', 'KNOWS_SYRIA', 'PerceiveLearnGeneral_S')
# Mean KNOWS_SYRIA 2.95
# Mean PerceiveLearnGeneral_S 3.79
# Z = 5.7605, p-value = 2.927e-11

# Olympic and Interactive
pre_pos_effect(df_c, 'INTERACTIVE', 'KNOWS_OLYMP', 'PerceiveLearnGeneral_O')
# Mean KNOWS_OLYMP 2.21
# Mean PerceiveLearnGeneral_O 2.71
# Z = 3.3926, p-value = 0.000489

# Thai and Static
pre_pos_effect(df_c, 'STATIC', 'KNOWS_THAI', 'PerceiveLearnGeneral_T')
# Mean KNOWS_THAI 4.39
# Mean PerceiveLearnGeneral_T 4.73
# Z = 5.7961, p-value = 4.206e-12

# Syria and Static
pre_pos_effect(df_c, 'STATIC', 'KNOWS_SYRIA', 'PerceiveLearnGeneral_S')
# Mean KNOWS_SYRIA 3.57
# Mean PerceiveLearnGeneral_S 3.83
# Z = 5.4524, p-value = 4.612e-10

# Olympic and Static
pre_pos_effect(df_c, 'STATIC', 'KNOWS_OLYMP', 'PerceiveLearnGeneral_O')
# Mean KNOWS_OLYMP 2.63
# Mean PerceiveLearnGeneral_O 3.53
# Z = 5.1168, p-value = 1.313e-08


####
# Differences in the potential change between conditions and articles
####

# Perceived Learning in Olympic between modalities
t = df_c[,c('SUBJECT','CONDITION','PerceiveLearnGeneral_O')]
t$CONDITION = as.factor(t$CONDITION)
kruskal.test(PerceiveLearnGeneral_O ~ CONDITION, data = t)
# Kruskal-Wallis chi-squared = 3.1284, df = 2, p-value = 0.2093

# Perceived Learning in Thai between modalities
t = df_c[,c('SUBJECT','CONDITION','PerceiveLearnGeneral_T')]
t$CONDITION = as.factor(t$CONDITION)
kruskal.test(PerceiveLearnGeneral_T ~ CONDITION, data = t)
# Kruskal-Wallis chi-squared = 0.16995, df = 2, p-value = 0.9185

# Perceived Learning in Syria between modalities
t = df_c[,c('SUBJECT','CONDITION','PerceiveLearnGeneral_S')]
t$CONDITION = as.factor(t$CONDITION)
kruskal.test(PerceiveLearnGeneral_S ~ CONDITION, data = t)
# Kruskal-Wallis chi-squared = 0.067441, df = 2, p-value = 0.9668




###
# Does sense of presence correlate with perceived authenticity and realism?
###

#
# Sense of presence and realism for AR condition
#

# Olympics
# The non-parametric test Kendall was chosen because we are studying likert items which are
# not continuos but ordinal. Also, Kendall was chosen instead of Spearman becasue
# it can handle ties
summary(Kendall(df_c[df_c$CONDITION=='AR', 'PRESENCE_TOTAL_OLYM'], df_c[df_c$CONDITION=='AR', 'AUTENTICITY_TOTAL_OLYM']))
# t=0.2807882, p-value = 0.03315 (Weak but significant correlation, which might be explained by the sample size)

# Syria
summary(Kendall(df_c[df_c$CONDITION=='AR', 'PRESENCE_TOTAL_SYRIA'], df_c[df_c$CONDITION=='AR', 'AUTENTICITY_TOTAL_SYRIA']))
# t=0.499, p-value=0.00016212 (Strong and significant correlation)

# Thai
summary(Kendall(df_c[df_c$CONDITION=='AR', 'PRESENCE_TOTAL_THAI'], df_c[df_c$CONDITION=='AR', 'AUTENTICITY_TOTAL_THAI']))
# t=0.491, p-value=0.00020337 (Strong and significantly correlation)


#
# Sense of presence and realism for Interactive condition
#

# Olympics
summary(Kendall(df_c[df_c$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_OLYM'], df_c[df_c$CONDITION=='INTERACTIVE', 'AUTENTICITY_TOTAL_OLYM']))
# tau=0.0525, p-value=0.72421

# Syria
summary(Kendall(df_c[df_c$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_SYRIA'], df_c[df_c$CONDITION=='INTERACTIVE', 'AUTENTICITY_TOTAL_SYRIA']))
# tau=0.397, p-value=0.0047827 (Weak but significant correlation, which might be explained by the sample size)

# Thai
summary(Kendall(df_c[df_c$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_THAI'], df_c[df_c$CONDITION=='INTERACTIVE', 'AUTENTICITY_TOTAL_THAI']))
# tau=0.0833, p-value=0.5664

#
# Sense of presence and realism for Static condition
#

# Olympics
summary(Kendall(df_c[df_c$CONDITION=='STATIC', 'PRESENCE_TOTAL_OLYM'], df_c[df_c$CONDITION=='STATIC', 'AUTENTICITY_TOTAL_OLYM']))
# tau=0.207, p-value=0.16469

# Syria
summary(Kendall(df_c[df_c$CONDITION=='STATIC', 'PRESENCE_TOTAL_SYRIA'], df_c[df_c$CONDITION=='STATIC', 'AUTENTICITY_TOTAL_SYRIA']))
# tau=0.493, p-value=0.00081217 (Strong and significant correlation)

# Thai
summary(Kendall(df_c[df_c$CONDITION=='STATIC', 'PRESENCE_TOTAL_THAI'], df_c[df_c$CONDITION=='STATIC', 'AUTENTICITY_TOTAL_THAI']))
# tau=0.583, p-value=0.00011408 (Strong and significant correlation)


###
#  Does sense of presence correlate with journalistic norms?
###

#
# Sense of presence and journalistic norms for AR condition
#

# Olympics
summary(Kendall(df_c[df_c$CONDITION=='AR', 'PRESENCE_TOTAL_OLYM'], df_c[df_c$CONDITION=='AR', 'JOURNAL_TOTAL_OLYM']))
# tau = 0.212, 2-sided p-value =0.11084

# Syria
summary(Kendall(df_c[df_c$CONDITION=='AR', 'PRESENCE_TOTAL_SYRIA'], df_c[df_c$CONDITION=='AR', 'JOURNAL_TOTAL_SYRIA']))
# tau = 0.215, 2-sided p-value =0.11392

# Thai
summary(Kendall(df_c[df_c$CONDITION=='AR', 'PRESENCE_TOTAL_THAI'], df_c[df_c$CONDITION=='AR', 'JOURNAL_TOTAL_THAI']))
# tau = 0.103, 2-sided pvalue =0.45272

#
# Sense of presence and journalistic norms for Interactive condition
#

# Olympics
summary(Kendall(df_c[df_c$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_OLYM'], df_c[df_c$CONDITION=='INTERACTIVE', 'JOURNAL_TOTAL_OLYM']))
# tau = 0.108, 2-sided pvalue =0.45339

# Syria
summary(Kendall(df_c[df_c$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_SYRIA'], df_c[df_c$CONDITION=='INTERACTIVE', 'JOURNAL_TOTAL_SYRIA']))
# tau = 0.243, 2-sided pvalue =0.08557

# Thai
summary(Kendall(df_c[df_c$CONDITION=='INTERACTIVE', 'PRESENCE_TOTAL_THAI'], df_c[df_c$CONDITION=='INTERACTIVE', 'JOURNAL_TOTAL_THAI']))
# tau = 0.00309, 2-sided pvalue =1

#
# Sense of presence and journalistic norms for Static condition
#

# Olympics
summary(Kendall(df_c[df_c$CONDITION=='STATIC', 'PRESENCE_TOTAL_OLYM'], df_c[df_c$CONDITION=='STATIC', 'JOURNAL_TOTAL_OLYM']))
# tau = 0.0797, 2-sided pvalue =0.60244

# Syria
summary(Kendall(df_c[df_c$CONDITION=='STATIC', 'PRESENCE_TOTAL_SYRIA'], df_c[df_c$CONDITION=='STATIC', 'JOURNAL_TOTAL_SYRIA']))
# tau = 0.152, 2-sided pvalue =0.30916

# Thai
summary(Kendall(df_c[df_c$CONDITION=='STATIC', 'PRESENCE_TOTAL_THAI'], df_c[df_c$CONDITION=='STATIC', 'JOURNAL_TOTAL_THAI']))
# tau = 0.229, 2-sided pvalue =0.12396


####
## Correlation between dwell time and depedent variables (see below)
####

#####
# Dependent variables: 
## Perceived knowledge change
## Sense of presence (PRESENCE_TOTAL_*)
## Engagement (ENGAGE_TOTAL_*)
## Perceived learning (PERCEIV_LEARN_VISU_TOTAL_*)
## Objective learning (OBJECT_LEARN_DESC_TOT_*)
## Usability (USABIL_TOTAL_*)
## Information recall
## Interest in adopting the technology (WILLING_TOTAL_*)
## Journalism norms (JOURNAL_TOTAL_*)
## Realism (AUTENTICITY_TOTAL_*)
#####

names(df_c)[names(df_c) == 'USABILT_TOTAL_OLYM'] = 'USABIL_TOTAL_OLYM'
indep_vars = c('SCREEN_TIME_TOTAL_O', 'SCREEN_TIME_TOTAL_S', 'SCREEN_TIME_TOTAL_T', 'SCREEN_TIME_VISUALS_O', 'SCREEN_TIME_VISUALS_S',
               'SCREEN_TIME_VISUALS_T', 'SCREEN_TIME_TEXT_O', 'SCREEN_TIME_TEXT_S', 'SCREEN_TIME_TEXT_T')
depen_vars = c('PRESENCE_TOTAL_', 'ENGAGE_TOTAL_', 'PERCEIV_LEARN_VISU_TOTAL_', 'USABIL_TOTAL_', 'WILLING_TOTAL_', 'JOURNAL_TOTAL_',
               'AUTENTICITY_TOTAL_')
significant_cors = cor_kendall(df_c, indep_vars, depen_vars)
write.csv(significant_cors, file="data/dwell_time.csv", row.names=FALSE)


####
## Fee Recall: Comparisons between the memory counts in the post-survey in the experiment and 24 hours after. 
## We currently report the counts (tables 4 and 5 in the IEEE VR paper), but we don’t report if the differences are 
## statistically significant
####

#####
### Vars
## After the experiment
## Text: MEMORIES_FROM_TEXT_O, MEMORIES_FROM_TEXT_T, MEMORIES_FROM_TEXT_S, 
## Visual: MEMORIES_FROM_VISUALS_O, MEMORIES_FROM_VISUALS_T, MEMORIES_FROM_VISUALS_S
## Both: MEMORIES_FROM_BOTH_O, MEMORIES_FROM_BOTH_T, MEMORIES_FROM_BOTH_S
###
## After 24 hours
## Text: REMEM_TEXT_24_O, REMEM_TEXT_24_T, REMEM_TEXT_24_S
## Visual: REMEM_VISUAL_24_O, REMEM_VISUAL_24_T, REMEM_VISUAL_24_S
## Both: REMEM_BOTH_24_O, REMEM_BOTH_24_T, REMEM_BOTH_24_S 
###
## Test: Kruskal-Wallis one-way ANOVA by ranks (three groups, non-parametric data)
## Alternative test: Friedman's two-way ANOVa test when data are not independent, which is not the case
####

run_kruskal_wallis = function(df, independent_vars, dependent_vars) {
  alpha_level = 0.05
  for (i_var in independent_vars) {
    for (d_var in dependent_vars) {
      writeLines(paste0('Checking difference between ', i_var, ' and ', d_var))
      res = kruskal.test(df[, d_var] ~ df[, i_var])
      writeLines(paste0('Kruskal-Wallis chi-squared = ', as.numeric(res$statistic), ' df = ', as.numeric(res$parameter), 
                        ', p-value = ', res$p.value))
      if (!is.na(res$p.value) && res$p.value < alpha_level) {
        writeLines(paste0('**** Significant difference between conditions for ', d_var, ' *****'))
      }
      cat('\n')
    }
  }
}
indep_vars = c('CONDITION')
depen_vars = c('MEMORIES_FROM_TEXT_O', 'MEMORIES_FROM_TEXT_T', 'MEMORIES_FROM_TEXT_S', 
               'MEMORIES_FROM_VISUALS_O', 'MEMORIES_FROM_VISUALS_T', 'MEMORIES_FROM_VISUALS_S',
               'MEMORIES_FROM_BOTH_O', 'MEMORIES_FROM_BOTH_T', 'MEMORIES_FROM_BOTH_S',
               'REMEM_TEXT_24_O', 'REMEM_TEXT_24_T', 'REMEM_TEXT_24_S',
               'REMEM_VISUAL_24_O', 'REMEM_VISUAL_24_T', 'REMEM_VISUAL_24_S',
               'REMEM_BOTH_24_O', 'REMEM_BOTH_24_T', 'REMEM_BOTH_24_S')
run_kruskal_wallis(df_c, indep_vars, depen_vars)

# Conduct a post-hoc test for the significant differences (Dunn test)
## 1. MEMORIES_FROM_TEXT_S
df_c$CONDITION = factor(df_c$CONDITION, levels=c("AR", "INTERACTIVE", "STATIC"))
# Exploratory Analysis
histogram(~ MEMORIES_FROM_TEXT_S | CONDITION, data=df_c, layout=c(1,3))
xtabs( ~ CONDITION + MEMORIES_FROM_TEXT_S, data=df_c)
xt = xtabs( ~ CONDITION + MEMORIES_FROM_TEXT_S, data=df_c)
prop.table(xt, margin=1)
Summarize(MEMORIES_FROM_TEXT_S ~ CONDITION, data=df_c, digits=3)
# Post-hoc Test
dunnTest(MEMORIES_FROM_TEXT_S ~ CONDITION, data=df_c, method="bh")
# The significant difference is between AR (mean=3.48, median=4) and Interactive (mean=2.58, median=2.5) (Z=2.65, p-value=0.02)

## 2. REMEM_VISUAL_24_T
df_c$CONDITION = factor(df_c$CONDITION, levels=c("AR", "INTERACTIVE", "STATIC"))
# Exploratory Analysis
histogram(~ REMEM_VISUAL_24_T | CONDITION, data=df_c, layout=c(1,3))
xtabs( ~ CONDITION + REMEM_VISUAL_24_T, data=df_c)
xt = xtabs( ~ CONDITION + REMEM_VISUAL_24_T, data=df_c)
prop.table(xt, margin=1)
Summarize(REMEM_VISUAL_24_T ~ CONDITION, data=df_c, digits=3)
# Post-hoc Test
dunnTest(REMEM_VISUAL_24_T ~ CONDITION, data=df_c, method="bh")
# The significant difference is between AR (mean=0.8, median=1) and Interactive (mean=1.6, median=2) (Z=-2.43, p-value=0.04)


#####
## 
## Comparison of free recall between a moment right after the experiment and 24 hours later
## Wilcoxon signed-rank test (two-sample paired)
####
run_wilcox_sign = function(df, independent_vars, comparison_pairs, conditions){
  for (i_var in independent_vars) {
    for (condition in conditions) {
      for(idx in 1:nrow(comparison_pairs)) {
        writeLines(paste0('Condition: ', condition ,' -- Comparison: ', comparison_pairs[idx,][1], ' and ', comparison_pairs[idx,][2]))
        a_df = rbind(data.frame('Subject'=df[df$CONDITION==condition, i_var], 
                                'Likert'=df[df$CONDITION==condition, comparison_pairs[idx,][1]],
                                'Moment'='after_experiment'), 
                     data.frame('Subject'=df[df$CONDITION==condition, i_var], 
                                'Likert'=df[df$CONDITION==condition, comparison_pairs[idx,][2]],
                                'Moment'='24_hours_later')
        )
        # remove na likert values
        a_df = filter(a_df, !(i_var %in% as.vector(a_df[is.na(a_df$Likert),i_var])))        
        # summary of data
        res_sum = Summarize(Likert ~ Moment, data=a_df)
        if (res_sum[1,'mean']==0 && res_sum[2,'mean']==0) {
          print('Both means are equal to zero!')
          next
        }
        else {
          print(res_sum)
          # prepare data
          t1 = a_df$Likert[a_df$Moment=='after_experiment']
          t2 = a_df$Likert[a_df$Moment=='24_hours_later']
          # compute test
          res = wilcoxsign_test(t1~t2, zero.method='Pratt')
          print(res) 
        }
      }
    }
  }  
}
indep_vars = c('SUBJECT')
comparison_pairs = matrix(c('MEMORIES_FROM_TEXT_O','REMEM_TEXT_24_O'), ncol=2)
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_TEXT_T', 'REMEM_TEXT_24_T'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_TEXT_S', 'REMEM_TEXT_24_S'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_VISUALS_O', 'REMEM_VISUAL_24_O'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_VISUALS_T', 'REMEM_VISUAL_24_T'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_VISUALS_S', 'REMEM_VISUAL_24_S'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_BOTH_O', 'REMEM_BOTH_24_O'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_BOTH_T', 'REMEM_BOTH_24_T'))
comparison_pairs = rbind(comparison_pairs, c('MEMORIES_FROM_BOTH_S', 'REMEM_BOTH_24_S'))
conditions = c('AR', 'INTERACTIVE', 'STATIC')
run_wilcox_sign(df_c, indep_vars, comparison_pairs, conditions)

#####
## Significant results
####

# Condition: AR -- Comparison: MEMORIES_FROM_TEXT_S and REMEM_TEXT_24_S
# Moment              n nvalid     mean        sd min Q1 median Q3 max percZero
# 1 after_experiment 29     29 3.482759 1.2989196   0  3      4  4   5 3.448276
# 2   24_hours_later 29     25 2.640000 0.9949874   1  2      2  3   5 0.000000
# Z = 2.232, p-value = 0.02562

# Condition: STATIC -- Comparison: MEMORIES_FROM_BOTH_T and REMEM_BOTH_24_T
# Moment              n nvalid      mean        sd min Q1 median Q3 max percZero
# 1 after_experiment 24     24 0.1250000 0.3378320   0  0      0  0   1 87.50000
# 2   24_hours_later 24     19 0.6315789 0.6839856   0  0      1  1   2 47.36842
# Z = -2.5381, p-value = 0.01115

# Condition: STATIC -- Comparison: MEMORIES_FROM_TEXT_S and REMEM_TEXT_24_S
# Moment              n nvalid     mean       sd min Q1 median Q3 max  percZero
# 1 after_experiment 24     24 2.916667 1.176460   0  2      3  4   5  4.166667
# 2   24_hours_later 24     19 2.000000 1.290994   0  1      2  3   4 10.526316
# Z = 2.4167, p-value = 0.01566
