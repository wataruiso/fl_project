return_mat <- function(data, index){
  ones <- rep(1, nrow(data))
  mean <- ones%*%t(colMeans(data))#平均行列を作る
  dmat <- as.matrix(data)-mean#偏差行列を作る
  vmat <- t(dmat)%*%dmat/nrow(data)#分散共分散行列を作る
  if(index == 1) return(mean)
  else if(index == 2) return(dmat)
  else if(index == 3) return(vmat)
}

get_beta <- function(data) {
  x_var2 <- vmat(data)[1, 1]
  cov2 <- vmat(data)[1, ncol(data)]
  beta2_var <- cov2 / x_var2
  return(beta2_var)
}

#型をデータフレームから行列に変換
change_data_to_mat <- function(data) {
  mat <- apply(apply(data, 1, as.numeric), 1, as.numeric)
  return(mat)
}

standarize <- function(data){
  ones <- rep(1, nrow(data))
  mean <- ones%*%t(colMeans(data))
  dmat <- as.matrix(data)-mean
  sdmat <- (t(dmat)%*%dmat/nrow(data))^(1/2) #対角要素に標準偏差が入った行列
  unit <- diag(1, nrow(sdmat), ncol(sdmat)) #分散共分散行列と同じ大きさの単位行列
  f <- unit/sdmat #対角要素に標準偏差の逆数が入った行列
  z <- dmat%*%f #標準化データ行列
  return(z)
}

linear <- function(x, y){
  inv.x <- solve(t(x)%*%x) #(X'X)^(-1) #分母
  covxy <- t(x)%*%y #X'y #分子
  coef <- inv.x%*%covxy #回帰係数ベクトル
  return(coef)
}
