# load libraries
library(dplyr)


compute_frequencies_condition = function(df, n_A, n_B, n_C) {
  freq_mat = matrix(nrow=length(unique(df$condition)), ncol=length(unique(df$sub_category)))
  i = 1
  for (cond in sort(unique(as.character(df$condition)))) {
    row = c()
    for (sub_cat in sort(unique(df$sub_category))) {
      n = nrow(unique(df%>%filter(sub_category==sub_cat)%>%filter(condition==cond)%>%select(subject)))
      if (cond == 'A') {
        per = round(n/n_A, 3)
      } else if (cond == 'B') {
        per = round(n/n_B, 3)
      } else {
        per = round(n/n_C, 3)
      }
      row = cbind(row, per)
      writeLines(paste0('Condition: ', cond, ' Sub-category: ', sub_cat, ' n: ', n, ' per: ', per))
    }
    freq_mat[i,] = row
    i = i + 1
  }
  rownames(freq_mat) = sort(unique(as.character(df$condition)))
  colnames(freq_mat) = sort(unique(df$sub_category))
  return (freq_mat)
}


# Load data
df = read.csv(file="./data/interviews.csv", header=TRUE, sep=",")

# Preprocess
colnames(df) = c('subject', 'condition', 'piece', 'question', 'category', 'category_num', 'sub_category')
df$subject = as.factor(trimws(df$subject))
df$piece = as.factor(df$piece)

# Compute N for condition A (AR), B (Interactive), C (Static)
n_A = nrow(unique(df%>%filter(condition=='A')%>%select('subject')))
n_B = nrow(unique(df%>%filter(condition=='B')%>%select('subject')))
n_C = nrow(unique(df%>%filter(condition=='C')%>%select('subject')))

# Compute N for piece 1 (Olympics), 2 (Syria), 3 (Thai)
n_1 = nrow(unique(df%>%filter(piece=='1')%>%select('subject')))
n_2 = nrow(unique(df%>%filter(piece=='2')%>%select('subject')))
n_3 = nrow(unique(df%>%filter(piece=='3')%>%select('subject')))

# Select the category and question of interest
sub_df = df %>% filter(question==4) %>% filter(category_num==7)

# Replace subcategory numbers by their description
sub_df[sub_df$sub_category==32,'sub_category'] = 'Context'
sub_df[sub_df$sub_category==33,'sub_category'] = 'Detail'
sub_df[sub_df$sub_category==34,'sub_category'] = 'Dimensions'
sub_df[sub_df$sub_category==35,'sub_category'] = 'Mechanics'
sub_df[sub_df$sub_category==36,'sub_category'] = 'Location'
sub_df[sub_df$sub_category==37,'sub_category'] = 'Scale'
sub_df[sub_df$sub_category==38,'sub_category'] = 'Shape'
sub_df[sub_df$sub_category==39,'sub_category'] = 'Size'
sub_df[sub_df$sub_category==40,'sub_category'] = 'Perspective'
sub_df[sub_df$sub_category==41,'sub_category'] = 'Process'
sub_df[sub_df$sub_category==42,'sub_category'] = 'Experience'
sub_df[sub_df$sub_category==43,'sub_category'] = 'Utility'

# Most frequent categories in conditions
freq_mat = matrix(nrow=length(unique(sub_df$condition)), ncol=length(unique(sub_df$sub_category)))
i = 1
for (cond in sort(unique(as.character(sub_df$condition)))) {
  row = c()
  for (sub_cat in sort(unique(sub_df$sub_category))) {
    n = nrow(unique(sub_df%>%filter(sub_category==sub_cat)%>%filter(condition==cond)%>%select(subject)))
    if (cond == 'A') {
      per = round(n/n_A, 3)
    } else if (cond == 'B') {
      per = round(n/n_B, 3)
    } else {
      per = round(n/n_C, 3)
    }
    row = cbind(row, per)
    writeLines(paste0('Condition: ', cond, ' Sub-category: ', sub_cat, ' n: ', n, ' per: ', per))
  }
  freq_mat[i,] = row
  i = i + 1
}
rownames(freq_mat) = sort(unique(as.character(sub_df$condition)))
colnames(freq_mat) = sort(unique(sub_df$sub_category))

# Most frequent categories for condition A
sort(freq_mat[1,], decreasing = TRUE)

# Most frequent categories for condition B
sort(freq_mat[2,], decreasing = TRUE)

# Most frequent categories for condition C
sort(freq_mat[3,], decreasing = TRUE)

# Check if the difference in the proportion of conditions is significant
# Detail A-B
x_A = n_A * freq_mat[1,'Detail']
x_B = n_B * freq_mat[2,'Detail']
prop.test(c(x_A, x_B), c(n_A, n_B), alternative="two.sided", correct=F)  # p-value=0.04618
# Detail A-C
x_A = n_A * freq_mat[1,'Detail']
x_C = n_C * freq_mat[3,'Detail']
prop.test(c(x_A, x_C), c(n_A, n_C), alternative="two.sided", correct=F)
# Detail B-C
x_B = n_B * freq_mat[2,'Detail']
x_C = n_C * freq_mat[3,'Detail']
prop.test(c(x_B, x_C), c(n_B, n_C), alternative="two.sided", correct=F)

# Mechanics A-B
x_A = n_A * freq_mat[1,'Mechanics']
x_B = n_B * freq_mat[2,'Mechanics']
prop.test(c(x_A, x_B), c(n_A, n_B), alternative="two.sided", correct=F)
# Mechanics A-C
x_A = n_A * freq_mat[1,'Mechanics']
x_C = n_C * freq_mat[3,'Mechanics']
prop.test(c(x_A, x_C), c(n_A, n_C), alternative="two.sided", correct=F)
# Mechanics B-C
x_B = n_B * freq_mat[2,'Mechanics']
x_C = n_C * freq_mat[3,'Mechanics']
prop.test(c(x_B, x_C), c(n_B, n_C), alternative="two.sided", correct=F)

# Experience A-B
x_A = n_A * freq_mat[1,'Experience']
x_B = n_B * freq_mat[2,'Experience']
prop.test(c(x_A, x_B), c(n_A, n_B), alternative="two.sided", correct=F)
# Experience A-C
x_A = n_A * freq_mat[1,'Experience']
x_C = n_C * freq_mat[3,'Experience']
prop.test(c(x_A, x_C), c(n_A, n_C), alternative="two.sided", correct=F)
# Experience B-C
x_B = n_B * freq_mat[2,'Experience']
x_C = n_C * freq_mat[3,'Experience']
prop.test(c(x_B, x_C), c(n_B, n_C), alternative="two.sided", correct=F)

# Size A-B
x_A = n_A * freq_mat[1,'Size']
x_B = n_B * freq_mat[2,'Size']
prop.test(c(x_A, x_B), c(n_A, n_B), alternative="two.sided", correct=F)
# Size A-C
x_A = n_A * freq_mat[1,'Size']
x_C = n_C * freq_mat[3,'Size']
prop.test(c(x_A, x_C), c(n_A, n_C), alternative="two.sided", correct=F)
# Size B-C
x_B = n_B * freq_mat[2,'Size']
x_C = n_C * freq_mat[3,'Size']
prop.test(c(x_B, x_C), c(n_B, n_C), alternative="two.sided", correct=F)

# Most frequent categories in conditions
freq_mat_piece = matrix(nrow=length(unique(sub_df$piece)), ncol=length(unique(sub_df$sub_category)))
i = 1
for (piece in sort(unique(as.character(sub_df$piece)))) {
  row = c()
  for (sub_cat in sort(unique(sub_df$sub_category))) {
    n = nrow(unique(sub_df%>%filter(sub_category==sub_cat)%>%filter(piece==piece)%>%select(subject)))
    if (piece == '1') {
      per = round(n/n_1, 3)
    } else if (piece == '2') {
      per = round(n/n_2, 3)
    } else {
      per = round(n/n_3, 3)
    }
    row = cbind(row, per)
    writeLines(paste0('Piece: ', piece, ' Sub-category: ', sub_cat, ' n: ', n, ' per: ', per))
  }
  freq_mat_piece[i,] = row
  i = i + 1
}
rownames(freq_mat_piece) = sort(unique(as.character(sub_df$piece)))
colnames(freq_mat_piece) = sort(unique(sub_df$sub_category))

# Most frequent categories for piece 1
sort(freq_mat_piece[1,], decreasing = TRUE)

# Most frequent categories for piece 2
sort(freq_mat_piece[2,], decreasing = TRUE)

# Most frequent categories for piece 3
sort(freq_mat_piece[3,], decreasing = TRUE)

# Check if the difference in the proportion of conditions is significant
# Detail 1-2
x_1 = n_1 * freq_mat_piece[1,'Detail']
x_2 = n_2 * freq_mat_piece[2,'Detail']
prop.test(c(x_1, x_2), c(n_1, n_2), alternative="two.sided", correct=F)  # p-value=0.88
# Detail 1-3
x_1 = n_1 * freq_mat_piece[1,'Detail']
x_3 = n_3 * freq_mat_piece[3,'Detail']
prop.test(c(x_1, x_3), c(n_1, n_3), alternative="two.sided", correct=F)  # p-value=0.45
# Detail 2-3
x_2 = n_2 * freq_mat_piece[2,'Detail']
x_3 = n_3 * freq_mat_piece[3,'Detail']
prop.test(c(x_2, x_3), c(n_2, n_3), alternative="two.sided", correct=F)  # p-value=0.54

# Mechanics 1-2
x_1 = n_1 * freq_mat_piece[1,'Mechanics']
x_2 = n_2 * freq_mat_piece[2,'Mechanics']
prop.test(c(x_1, x_2), c(n_1, n_2), alternative="two.sided", correct=F)  # p-value=0.90
# Mechanics 1-3
x_1 = n_1 * freq_mat_piece[1,'Mechanics']
x_3 = n_3 * freq_mat_piece[3,'Mechanics']
prop.test(c(x_1, x_3), c(n_1, n_3), alternative="two.sided", correct=F)  # p-value=0.52
# Mechanics 2-3
x_2 = n_2 * freq_mat_piece[2,'Mechanics']
x_3 = n_3 * freq_mat_piece[3,'Mechanics']
prop.test(c(x_2, x_3), c(n_2, n_3), alternative="two.sided", correct=F)  # p-value=0.60

# Experience 1-2
x_1 = n_1 * freq_mat_piece[1,'Experience']
x_2 = n_2 * freq_mat_piece[2,'Experience']
prop.test(c(x_1, x_2), c(n_1, n_2), alternative="two.sided", correct=F)  # p-value=0.95
# Experience 1-3
x_1 = n_1 * freq_mat_piece[1,'Experience']
x_3 = n_3 * freq_mat_piece[3,'Experience']
prop.test(c(x_1, x_3), c(n_1, n_3), alternative="two.sided", correct=F)  # p-value=0.74
# Experience 2-3
x_2 = n_2 * freq_mat_piece[2,'Experience']
x_3 = n_3 * freq_mat_piece[3,'Experience']
prop.test(c(x_2, x_3), c(n_2, n_3), alternative="two.sided", correct=F)  # p-value=0.79

# Size 1-2
x_1 = n_1 * freq_mat_piece[1,'Size']
x_2 = n_2 * freq_mat_piece[2,'Size']
prop.test(c(x_1, x_2), c(n_1, n_2), alternative="two.sided", correct=F)  # p-value=0.77
# Size 1-3
x_1 = n_1 * freq_mat_piece[1,'Size']
x_3 = n_3 * freq_mat_piece[3,'Size']
prop.test(c(x_1, x_3), c(n_1, n_3), alternative="two.sided", correct=F)  # p-value=0.80
# Size 2-3
x_2 = n_2 * freq_mat_piece[2,'Size']
x_3 = n_3 * freq_mat_piece[3,'Size']
prop.test(c(x_2, x_3), c(n_2, n_3), alternative="two.sided", correct=F)  # p-value=0.83


# Most frequent categories per article 1 per condition
sub_df_piece_1 = sub_df %>% filter(piece==1)
n_A_p1 = nrow(unique(sub_df_piece_1%>%filter(condition=='A')%>%select('subject')))
n_B_p1 = nrow(unique(sub_df_piece_1%>%filter(condition=='B')%>%select('subject')))
n_C_p1 = nrow(unique(sub_df_piece_1%>%filter(condition=='C')%>%select('subject')))
freq_mat = compute_frequencies_condition(sub_df_piece_1, n_A_p1, n_B_p1, n_C_p1)
# Most frequent categories for condition A
sort(freq_mat[1,], decreasing = TRUE)
# Most frequent categories for condition B
sort(freq_mat[2,], decreasing = TRUE)
# Most frequent categories for condition C
sort(freq_mat[3,], decreasing = TRUE)
# Mechanics A-B
x_A = n_A_p1 * freq_mat[1,'Mechanics']
x_B = n_B_p1 * freq_mat[2,'Mechanics']
prop.test(c(x_A, x_B), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.29
# Mechanics A-C
x_A = n_A_p1 * freq_mat[1,'Mechanics']
x_C = n_C_p1 * freq_mat[3,'Mechanics']
prop.test(c(x_A, x_C), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.76
# Mechanics B-C
x_B = n_B_p1 * freq_mat[2,'Mechanics']
x_C = n_C_p1 * freq_mat[3,'Mechanics']
prop.test(c(x_B, x_C), c(n_B_p1, n_C_p1), alternative="two.sided", correct=F)  # p-value=0.88
# Detail A-B
x_A = n_A_p1 * freq_mat[1,'Detail']
x_B = n_B_p1 * freq_mat[2,'Detail']
prop.test(c(x_A, x_B), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.51
# Detail A-C
x_A = n_A_p1 * freq_mat[1,'Detail']
x_C = n_C_p1 * freq_mat[3,'Detail']
prop.test(c(x_A, x_C), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.29
# Detail B-C
x_B = n_B_p1 * freq_mat[2,'Detail']
x_C = n_C_p1 * freq_mat[3,'Detail']
prop.test(c(x_B, x_C), c(n_B_p1, n_C_p1), alternative="two.sided", correct=F)  # p-value=0.79


# Most frequent categories per article 2 per condition
sub_df_piece_2 = sub_df %>% filter(piece==2)
n_A_p2 = nrow(unique(sub_df_piece_2%>%filter(condition=='A')%>%select('subject')))
n_B_p2 = nrow(unique(sub_df_piece_2%>%filter(condition=='B')%>%select('subject')))
n_C_p2 = nrow(unique(sub_df_piece_2%>%filter(condition=='C')%>%select('subject')))
freq_mat = compute_frequencies_condition(sub_df_piece_2, n_A_p2, n_B_p2, n_C_p2)
# Most frequent categories for condition A
sort(freq_mat[1,], decreasing = TRUE)
# Most frequent categories for condition B
sort(freq_mat[2,], decreasing = TRUE)
# Most frequent categories for condition C
sort(freq_mat[3,], decreasing = TRUE)
# Detail A-B
x_A = n_A_p1 * freq_mat[1,'Detail']
x_B = n_B_p1 * freq_mat[2,'Detail']
prop.test(c(x_A, x_B), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.41
# Detail A-C
x_A = n_A_p1 * freq_mat[1,'Detail']
x_C = n_C_p1 * freq_mat[3,'Detail']
prop.test(c(x_A, x_C), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.40
# Detail B-C
x_B = n_B_p1 * freq_mat[2,'Detail']
x_C = n_C_p1 * freq_mat[3,'Detail']
prop.test(c(x_B, x_C), c(n_B_p1, n_C_p1), alternative="two.sided", correct=F)  # p-value=0.53


# Most frequent categories per article 3 per condition
sub_df_piece_3 = sub_df %>% filter(piece==3)
n_A_p3 = nrow(unique(sub_df_piece_3%>%filter(condition=='A')%>%select('subject')))
n_B_p3 = nrow(unique(sub_df_piece_3%>%filter(condition=='B')%>%select('subject')))
n_C_p3 = nrow(unique(sub_df_piece_3%>%filter(condition=='C')%>%select('subject')))
freq_mat = compute_frequencies_condition(sub_df_piece_3, n_A_p3, n_B_p3, n_C_p3)
# Most frequent categories for condition A
sort(freq_mat[1,], decreasing = TRUE)
# Most frequent categories for condition B
sort(freq_mat[2,], decreasing = TRUE)
# Most frequent categories for condition C
sort(freq_mat[3,], decreasing = TRUE)
# Experience A-C
x_A = n_A_p1 * freq_mat[1,'Experience']
x_C = n_C_p1 * freq_mat[3,'Experience']
prop.test(c(x_A, x_C), c(n_A_p1, n_C_p1), alternative="two.sided", correct=F)  # p-value=0.64
# Scale B-C
x_B = n_B_p1 * freq_mat[2,'Scale']
x_C = n_C_p1 * freq_mat[3,'Scale']
prop.test(c(x_B, x_C), c(n_B_p1, n_C_p1), alternative="two.sided", correct=F)  # p-value=0.50
# Size A-B
x_A = n_A_p1 * freq_mat[1,'Size']
x_B = n_B_p1 * freq_mat[2,'Size']
prop.test(c(x_A, x_B), c(n_A_p1, n_B_p1), alternative="two.sided", correct=F)  # p-value=0.71