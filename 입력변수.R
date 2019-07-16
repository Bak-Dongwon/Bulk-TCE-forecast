
#RSI#

RSI <- function(x, lag=14, wh){
  rsi <- x[wh,]
  
  if(wh <= lag){
    rsi[wh <= lag,] <- NA
  }else{
    #대상이 되는 범위#
    st <- wh-lag
    end <- wh
    range <- st:end
    #대상이 되는 데이터#
    target_x <- x[range,]
    
    #증가 감소된 수치#
    diff_target_x <- diff(target_x,lag=1)[-1,]
    
    up <- colSums(abs(diff_target_x)*(sign(diff_target_x)==1))
    down <- colSums(abs(diff_target_x)*(sign(diff_target_x)==-1))
    
    rsi <- up/(up+down)
  }
  return(rsi)
}


#모멘텀#

Momentum <- function(x, lag = 4, wh){
  
  momentum <- diff(x, lag = lag)[wh,]
  
  return(momentum)
}


#변화율(ROC)#

ROC <- function(x, lag = 4, wh){
  
  roc <- x[wh,]
  roc[wh <= lag,] <- NA
  roc[wh > lag,] <- x[wh[wh > lag],] / as.matrix(x[(wh-lag)[wh > lag],])
  
  return(roc)
}


# 이동평균 차이 #

MA_rate <- function(x, short = 4, long = 12, wh){
  ma_rate <- x[wh,]
  
  if(wh < long){
    ma_rate[,] <- NA
  }else{
    #대상이 되는 범위#
    st_short <- wh-short + 1
    end_short <- wh
    range_short <- st_short : end_short
    
    st_long <- wh-long + 1
    end_long <- wh
    range_long <- st_long : end_long
    
    #대상이 되는 데이터#
    target_x <- x[range_short,]
    MA_short <- colMeans(target_x)
    
    target_x <- x[range_long,]
    MA_long <- colMeans(target_x)  
    
    #장단기 이동평균 차이#
    ma_rate <- MA_short - MA_long
  }
  return(ma_rate)
}


# 스토캐스틱 #

Stochastic <- function(x, lag = 14, ma_lag = 3, wh, value = "K"){
  stochastic <- rep(NA, ncol(x)*2)
  
  if(wh >= lag + ma_lag - 1){
    #대상이 되는 범위#
    st_K <- wh -lag - ma_lag + 2
    end_K <- wh
    range_K <- st_K : end_K
    
    #대상이 되는 데이터#
    target_x <- x[range_K,]
    
    # 기간내 최대 최소값 #
    max_K <- as.matrix(rollMax(target_x, lag))
    min_K <- as.matrix(rollMin(target_x, lag))
    
    K <- (x[(st_K + lag - 1) : end_K,] - min_K) / (max_K - min_K)
    D <- colMeans(K)
    
    stochastic <- c(K[ma_lag,], D)
  }
  
  return(stochastic)
}

# 윌리엄스 %R #

Williams_R <- function(x, lag = 14, wh){
  williams_r <- rep(NA, ncol(x)*1)
  
  if(wh >= lag){
    #대상이 되는 범위#
    st_R <- wh -lag + 1
    end_R <- wh
    range_R <- st_R : end_R
    
    #대상이 되는 데이터#
    target_x <- x[range_R,]
    
    # -(최대 - 종가) / (최대 - 최소)
    
    williams_r <- -(colMaxs(target_x) - target_x[lag,])/(colMaxs(target_x) - colMins(target_x))
  }
  
  return(williams_r)
}


# MACD(moving average convergence divergence) #
MACD <- function(x, short = 12, long = 26, lag_signal = 9, wh){
  
  # EMA 차이 #
  EMA_diff <- apply(x, 2, function(x) EMA(x, n = short)) -
    apply(x, 2, function(x) EMA(x, n = long))
  
  macd <- EMA_diff[wh,]
  
  signal <- apply(EMA_diff, 2, function(x) EMA(x, lag_signal))[wh,]
  
  oscilator <- macd - signal
  
  return(c(macd, signal, oscilator))
}


