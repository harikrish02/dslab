install.packages("dplyr")
library(dplyr)
head(mtcars)
distance_mat <- dist(mtcars, method = 'euclidean')
distance_mat
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl
plot(Hierar_cl)
abline(h = 110, col = "green")
fit <- cutree(Hierar_cl, k = 3 )
fit
table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")
