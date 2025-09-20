library(Rcpp)
library(microbenchmark)
library(ggplot2)

library(devtools)
find_rtools()
source("C:/Users/arab/Desktop/task1-knn/knn_s3_formula.R")
Rcpp::sourceCpp("C:/Users/arab/Desktop/task1-knn/knn_pred.cpp")

ns <- c(500, 1000, 2000)
ps <- c(3, 5, 10)

results <- data.frame()

for (n in ns) {
  for (p in ps) {
    data <- simulate_knn_data(n = n, p = p, m = 200, k = 10)
    bench <- microbenchmark(
      Rcpp = knn_pred_cpp(data$train_x, data$train_y, data$test_x, data$k),
      R = knn_pred_R(data$train_x, data$train_y, data$test_x, data$k),
      times = 10
    )
    df <- data.frame(n=n, p=p, method=as.character(bench$expr), time_ms=bench$time/1e6)
    results <- rbind(results, df)
  }
}

write.csv(results, "benchmark_results.csv", row.names = FALSE)

ggplot(results, aes(x=factor(p), y=time_ms, fill=method)) +
  geom_boxplot() +
  facet_wrap(~ n, scales="free_y") +
  theme_minimal()

ggsave("benchmark_plot.png")
