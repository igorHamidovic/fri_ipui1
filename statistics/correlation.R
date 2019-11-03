source("preprocessing/btc_data_stream.R")
source("preprocessing/eth_data_stream.R")

incremental.cor <- function() {
  btc_stream.init()
  eth_stream.init()
  counter <- 1
  sum.products <- 0
  sum.el1 <- 0
  sum.el2 <- 0
  sum.el1square <- 0
  sum.el2square <- 0
  cor.vector <- c()
  
  while (!is.null(current_btc <- btc_stream.next_value()) & !is.null(current_eth <- eth_stream.next_value())) {
    if (counter==1)
      cor.vector[counter] <- NaN
    else {
      sum.products <- sum.products + (current_btc * current_eth)
      sum.el1 <- sum.el1 + current_btc
      sum.el2 <- sum.el2 + current_eth
      sum.el1square <- sum.el1square + current_btc ^ 2
      sum.el2square <- sum.el2square + current_eth ^ 2
      cor.vector[counter] <- (sum.products - (sum.el1 * sum.el2) / counter) / (sqrt(sum.el1square - (sum.el1^2 / counter)) * sqrt(sum.el2square - (sum.el2^2 / counter)))
    }
    counter <- counter + 1
  }
  return(cor.vector)
}

incremental.beta.bth_eth <- function() {
  btc_stream.init()
  eth_stream.init()
  counter <- 1
  sum.btc <- 0
  sum.eth <- 0
  sum.els <- 0
  sum.variance <- 0
  beta.vector <- c()
  while (!is.null(current_btc <- btc_stream.next_value()) & !is.null(current_eth <- eth_stream.next_value())) {
    sum.btc <- sum.btc + current_btc
    sum.eth <- sum.eth + current_eth
    mean.btc <- sum.btc / counter
    mean.eth <- sum.eth / counter
    sum.variance <- sum.variance + (current_eth - mean.eth)^2
    if (counter==1)
      beta.vector[counter] <- NaN
    else {
      sum.els <- sum.els + (current_btc * current_eth)
      beta.vector[counter] <- (((1/counter)*sum.els) - (mean.btc*mean.eth)) / sum.variance
    }
    counter <- counter + 1
  }
  return(beta.vector)
}

incremental.beta.eth_btc <- function() {
  btc_stream.init()
  eth_stream.init()
  counter <- 1
  sum.btc <- 0
  sum.eth <- 0
  sum.els <- 0
  sum.variance <- 0
  beta.vector <- c()
  while (!is.null(current_btc <- btc_stream.next_value()) & !is.null(current_eth <- eth_stream.next_value())) {
    sum.btc <- sum.btc + current_btc
    sum.eth <- sum.eth + current_eth
    mean.btc <- sum.btc / counter
    mean.eth <- sum.eth / counter
    sum.variance <- sum.variance + (current_btc - mean.btc)^2
    if (counter==1)
      beta.vector[counter] <- NaN
    else {
      sum.els <- sum.els + (current_btc * current_eth)
      beta.vector[counter] <- (((1/counter)*sum.els) - (mean.btc*mean.eth)) / sum.variance
    }
    counter <- counter + 1
  }
  return(beta.vector)
}

plot.cors <- function() {
  old.par <- par(mfrow=c(3,1), oma=c(0,0,0,0), mar=c(1,4,1,1))
  on.exit(par(old.par))
  plot(btc_data_stream$size, type="l", ylab="stream1")
  plot(eth_data_stream$size, type="l", ylab="stream2")
  plot(incremental.cor(), type="l", ylab="correlation")
  plot(incremental.beta.bth_eth(), type = "l", ylab="beta")
  plot(incremental.beta.eth_btc(), type = "l", ylab="beta")
}