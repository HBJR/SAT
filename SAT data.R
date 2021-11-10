library(dplyr)
library(tibble)

K = rnorm(500, .5, .2)
K = data.frame(K)

names(K)[1] = 'admit_rate'

K = K %>%
  transmute(admit_rate = case_when(K$admit_rate < 0.05 ~ 0.05,
                                   K$admit_rate > 0.95 ~ .95,
                                   K$admit_rate > 0.05 & K$admit_rate < 0.95 ~ K$admit_rate)) %>%
  mutate(alpha = case_when(K$admit_rate <= quantile(K$admit_rate, 0.25) ~ .35, 
                           quantile(K$admit_rate, 0.25) < K$admit_rate & K$admit_rate < quantile(K$admit_rate, .75) ~ .4,
                           K$admit_rate >= quantile(K$admit_rate, .75) ~ .45)
         ) %>%
  mutate (beta = alpha) %>%
  mutate(avg_income = rep(NA_integer_, 500))

K = K[order(K$admit_rate), ]

n = rnorm(3000000, 90, 15)
scores <- scale(matrix(rnorm(9000000), ncol =3))
n = cbind(scale(n), scores)

c1 = cor(n)
chol1 <- solve(chol(c1)) 
n =  n %*% chol1

cor_matrix <- matrix( 
  c(1, 0.35, 0.25, 0.4,
    0.35, 1, 0.5, 0.6,
    0.25, 0.5, 1, 0.5,
    0.4, 0.6, 0.5, 1), ncol=4)

eigen(cor_matrix)$values #if all positive, matrix is positive definite

chol2 <- chol(cor_matrix)

n = n %*% chol2

n = data.frame(n)

n[, 1] = n[, 1]*15 + 90
n[, 2] = scale(n[, 2])
n[, 3] = scale(n[, 3])
n[, 4] = scale(n[, 4])

n_list = n


for(k in 1:nrow(K)){
  score = K[k, "alpha"] * n_list[, 2] + K[k, "beta"] * n_list[, 3] + (1 -  K[k, "alpha"] - K[k, "beta"]) * n_list[, 4]
  admit_score = sort(score, decreasing = TRUE)[5000]
  score = data.frame(score)
  score = rownames_to_column(score, 'row_num')
  admit_row = filter(score, score > admit_score)$row_num
  admit_row = as.numeric(admit_row)
  K[k, "avg_income"] = mean(n_list[admit_row, 1])
  n_list = n_list[-admit_row, ]
}
