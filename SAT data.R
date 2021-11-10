library(dplyr)
library(tibble)

#randomly generate admissions rates for 500 universities 
K = rnorm(500, .5, .2)
K = data.frame(K)

names(K)[1] = 'admit_rate'

K = K %>%
#correct admissions rates less than 5% or greater than 95%
  transmute(admit_rate = case_when(K$admit_rate < 0.02 ~ 0.02,
                                   K$admit_rate > 0.98 ~ .98,
                                   K$admit_rate > 0.02 & K$admit_rate < 0.98 ~ K$admit_rate)) %>%
#generate admissions formula parameters from admissions rates
  mutate(alpha = case_when(K$admit_rate <= quantile(K$admit_rate, 0.25) ~ .35, 
                           quantile(K$admit_rate, 0.25) < K$admit_rate & K$admit_rate < quantile(K$admit_rate, .75) ~ .4,
                           K$admit_rate >= quantile(K$admit_rate, .75) ~ .45)
         ) %>%
  mutate (beta = alpha) %>%
#create columns to record average student income
  mutate(avg_income = rep(NA_integer_, 500), no_SAT_avg_income_1 = rep(NA_integer_, 500), no_SAT_avg_income_2 = rep(NA_integer_, 500))

#order universities by admissiosn rates
K = K[order(K$admit_rate), ]

#randomly generate matrix of student data 
n = scale(matrix(rnorm(12000000), ncol = 4)) 

names(n)[1] = 'income'
names(n)[2] = 'SAT'
names(n)[3] = 'GPA'
names(n)[4] = 'Other'

#cholesky decomposition to ensure independence between student vectors
c1 = cor(n)
chol1 <- solve(chol(c1)) 
n =  n %*% chol1

#create specified correlation matrix
cor_matrix <- matrix( 
  c(1, 0.35, 0.25, 0.4,
    0.35, 1, 0.5, 0.6,
    0.25, 0.5, 1, 0.5,
    0.4, 0.6, 0.5, 1), ncol=4)

#check matrix is positive definite 
eigen(cor_matrix)$values 

#correlate data to specified matrix
chol2 <- chol(cor_matrix)
n = n %*% chol2
n = data.frame(n)

#scale income to specified parameters
n[, 1] = n[, 1]*15 + 90
#standardize admissions info
n[, 2] = scale(n[, 2])
n[, 3] = scale(n[, 3])
n[, 4] = scale(n[, 4])

names(n)[1] = 'income'
names(n)[2] = 'SAT'
names(n)[3] = 'GPA'
names(n)[4] = 'Other'

#create modifiable student lists
n_list = n
n_list_no_SAT_1 = n
n_list_no_SAT_2 = n


#Currently editing to streamline code and make operations faster
for(k in 1:nrow(K)){
  #generate admissions scores for each student
  score = K[k, "alpha"] * n_list[, 2] + K[k, "beta"] * n_list[, 3] + (1 -  K[k, "alpha"] - K[k, "beta"]) * n_list[, 4]
  no_SAT_score_1 = (K[k, "alpha"] + K[k, "beta"]) * n_list_no_SAT_1[, 3] + (1 -  K[k, "alpha"] - K[k, "beta"]) * n_list_no_SAT_1[, 4]
  no_SAT_score_2 = K[k, "beta"] * n_list_no_SAT_2[, 3] + (1- K[k, "beta"]) * n_list_no_SAT_2[, 4]
  
  #find lowest possible admissable score for university k
  admit_score = sort(score, decreasing = TRUE)[5000]
  no_SAT_admit_score_1 = sort(no_SAT_score_1, decreasing = TRUE)[5000]
  no_SAT_admit_score_2 = sort(no_SAT_score_2, decreasing = TRUE)[5000]
  
  #find students to admit
  score = data.frame(score)
  score = rownames_to_column(score, 'row_num')
  admit_row = filter(score, score > admit_score)$row_num
  admit_row = as.numeric(admit_row)
  
  no_SAT_score_1 = data.frame(no_SAT_score_1)
  no_SAT_score_1 = rownames_to_column(no_SAT_score_1, 'row_num')
  no_SAT_admit_row_1 = filter(no_SAT_score_1, no_SAT_score_1 > no_SAT_admit_score_1)$row_num
  no_SAT_admit_row_1 = as.numeric(no_SAT_admit_row_1)
  
  no_SAT_score_2 = data.frame(no_SAT_score_2)
  no_SAT_score_2 = rownames_to_column(no_SAT_score_2, 'row_num')
  no_SAT_admit_row_2 = filter(no_SAT_score_2, no_SAT_score_2 > no_SAT_admit_score_2)$row_num
  no_SAT_admit_row_2 = as.numeric(no_SAT_admit_row_2)
  
  #find average income of admitted students
  K[k, "avg_income"] = mean(n_list[admit_row, 1])
  K[k, "no_SAT_avg_income_1"] = mean(n_list_no_SAT_1[no_SAT_admit_row_1, 1])
  K[k, "no_SAT_avg_income_2"] = mean(n_list_no_SAT_2[no_SAT_admit_row_2, 1])
  
  #delete admitted students from student lists
  n_list = n_list[-admit_row, ]
  n_list_no_SAT_1 = n_list_no_SAT_1[-no_SAT_admit_row_1, ]
  n_list_no_SAT_2 = n_list_no_SAT_2[-no_SAT_admit_row_2, ]
}
