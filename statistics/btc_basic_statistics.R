source("preprocessing/btc_data_stream.R")

btc.incremental.mean <- function() {
  counter <- 1
  means.vector <- c()
  btc_stream.init()
  while(!is.null(current <- btc_stream.next_value())) {
    if(counter == 1) {
      means.vector[counter] <- current
    }
    else {
      means.vector[counter] <- ((counter - 1) * means.vector[counter - 1] + current) / counter
    }
    counter <- counter + 1
  }
  return(means.vector)
}

btc.incremental.sd <- function() {
  btc_stream.init()
  counter <- 1
  sum.squares <- 0
  sum.all <- 0
  sd.vector <- c()
  while(!is.null(current <- btc_stream.next_value())) {
    sum.squares <- sum.squares + current ^ 2
    sum.all <- sum.all + current
    sd.vector[counter] <- sqrt((sum.squares-(sum.all^2/counter))/(counter - 1))
    counter <- counter + 1
  }
  return(sd.vector)
}

plot.btc.non_window_statistics <- function() {
  plot(x=btc_data_stream$timestamp, y=btc_data_stream$size, main="BTC", xlab="timestamp", ylab="Block size")
  means <- btc.incremental.mean()
  lines(x=means, col="blue")
  sd <- btc.incremental.sd()
  lines(x=sd, col="red")
  legend("topright", legend=c("mean", "standard deviation"), col=c("blue", "red"), lty=1, cex=0.9)
}