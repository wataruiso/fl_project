standarize <- function(data){
  ones <- rep(1, nrow(data))
  mean <- ones%*%t(colMeans(data))
  dmat <- as.matrix(data)-mean
  sdmat <- (t(dmat)%*%dmat/nrow(data))^(1/2) #対角要素に標準偏差が入った行列
  unit <- diag(1, nrow(sdmat), ncol(sdmat)) #分散共分散行列と同じ大きさの単位行列
  f <- unit/sdmat #対角要素に標準偏差の逆数が入った行列
  f[is.nan(f)] <- 0
  z <- dmat%*%f #標準化データ行列
  return(z)
}

standarize_v <- function(data){
  m <- rep(mean(data), length(data))
  s <- rep(sd(data), length(data))
  return((data - test)/test1)
}

process_data <- function(data, col) {
  data <- data[, col[1,]]#必要なカラムの抽出
  for (i in 1:ncol(data)) {
    
    data[is.na(data)] <- 0
    if(as.numeric(col[2,i]) > 0 && as.numeric(col[2,i]) < 1000) {#ダミーじゃなかったら
      data <- data[!(data[,i] == "8"), ]
      data <- data[!(data[,i] == "9"), ]
      
      if(as.numeric(col[2,i]) <= 10) {
        val_max <- as.numeric(col[2,i])
        data <- data[!(data[,i] > val_max), ]
        for (l in 1:nrow(data)) {
          data[l, i] <- abs(val_max - (data[l, i] - 1)) #値が高いほど点数が高いように調整
        }
      }else {
        val_max <- as.numeric(col[2,i]) - 10
        data <- data[!(data[,i] > val_max), ]
      }
      
    }
    # else if(as.numeric(col[2,i]) > 1000) {
    #   val_max <- as.numeric(col[2,i]) - 1000
    #   for(j in 1:nrow(data)){
    #     if(data[j,i] > val_max) data[j,i] <- 0
    #   }
    # }
    else if(as.numeric(col[2,i]) == 0) {
      for(k in 1:nrow(data)){
        if(data[k,i] == "8") data[k,i] <- 0
      }
    }
  }
  
  data_score <- data[,as.numeric(col[2,]) != 0 & as.numeric(col[2,]) < 1000]
  data_std <- cbind(data_score,data[,as.numeric(col[2,]) == 0],data[,as.numeric(col[2,]) > 1000])
  # data_std <- cbind(standarize(data_score),data[,as.numeric(col[2,]) == 0],data[,as.numeric(col[2,]) > 1000])
  return(data_std)
}

make_dummies_mat <- function(control) {
  #ダミーラベルの順番→(婚姻状態)/同居家族/性別/年齢/職業/地域/暮らしのゆとり/都市サイズ
  # 婚姻状態
  control[,1][control[,1] > 3] <- 0
  control[,2][control[,2] > 1] <- 0
  control[,4][control[,4] > 2] <- 0
  control[,6][control[,6] > 3] <- 0
  control[,7][control[,7] > 10] <- 0
  control[,8][control[,8] > 5] <- 0
  control[,9][control[,9] > 5] <- 0
  control[,10][control[,10] > 5] <- 0
  control[,1][control[,1] == 3] <- 2
  control[,1][control[,1] == 1] <- 0
  control[,3][control[,3] == 0] <- 2
  control[,3][control[,3] != 2] <- 0
  # 性別←なし
  #年齢
  control[,5][control[,5] < 60] <- 0
  control[,5][control[,5] >= 60] <- 1
  # 職業
  # control[,3][control[,3] != 1] <- 0
  # control[,4][control[,4] != 1] <- 0
  control[,6][control[,6] != 1] <- 0
  control[,7][control[,7] != 3] <- 0
  
  control[,8][control[,8] < 4] <- 0
  control[,8][control[,8] == 4] <- 5
  control[,9][control[,9] < 4] <- 0
  control[,9][control[,9] == 4] <- 5
  control[,10][control[,10] > 2] <- 0
  control[,10][control[,10] == 2] <- 1

  # control[,8] <- abs(5 - (control[,8] - 1))
  # control[,9] <- abs(5 - (control[,9] - 1))

  for (i in 1:ncol(control)) {
    colnames(control)[i] <- paste("d", i, sep="")
  }
  
  control$d1 <- factor(control$d1)
  control$d2 <- factor(control$d2)
  control$d3 <- factor(control$d3)
  control$d4 <- factor(control$d4)
  control$d5 <- factor(control$d5)
  control$d6 <- factor(control$d6)
  control$d7 <- factor(control$d7)
  control$d8 <- factor(control$d8)
  control$d9 <- factor(control$d9)
  control$d10 <- factor(control$d10)
  
  
  return(makedummies(control))
  # return(cbind(makedummies(dummy),control[,8:9]))
}

getBasicData <- function(mat) {
  mean <- round(colMeans(mat),3)
  min <- round(apply(mat, 2, min),3)
  max <- round(apply(mat, 2, max),3)
  sd <- round(apply(mat, 2, sd),3)
  basic_data <- data.frame(mean=mean,min=min,max=max,sd=sd)
  return(basic_data)
}