# 繰り返し回数
n = 500000

set.seed(0)

# thetaが範囲内かチェックする関数
theta_flag <- function(th1, th2) {
  f1 <- abs((-th1 + sqrt(th1^2 + 4*th2)) / (2*th2))
  f2 <- abs((-th1 - sqrt(th1^2 + 4*th2)) / (2*th2))
  if(th1^2 + 4*th2 < 0 || th2 == 0) {
    return(F)
  }
  if(f1 > 1 && f2 > 1) {
    return(T)
  }
  return(F)
}


# 平均0, 分散1の正規乱数で発生させたthetaの候補たち
theta1 <- rnorm(n, mean = 0, sd = 3)
theta2 <- rnorm(n, mean = 0, sd = 3)
plt_th1 <- c()
plt_th2 <- c()

for(i in 1:n) {
  if(theta_flag(theta1[i], theta2[i])) {
    plt_th1 <- c(plt_th1, theta1[i])
    plt_th2 <- c(plt_th2, theta2[i])
  }
}

# プロット
plot(plt_th1, plt_th2, xlim = c(-2, 2), ylim = c(-2, 2), pch=20, col=4, xlab = "theta1", ylab = "theta2", main = "モンテカルロ法 プロット")
abline(0, 0, 0, 0, col = 2, lty = 2)
