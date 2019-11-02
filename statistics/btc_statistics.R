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

btc.acf <- function() {
  btc_stream.init()
  correlation.period <- 24
  counter <- 1
  available_values <- c()
  mean.value <- 0
  acf.variance.sum <- 0
  acf.cor.sum <- 0
  acf.vector <- c()
  acf.cor.func <- function(mean.value, available_values) {
    acf.cor.sum <- 0
    start_index <- length(available_values) - correlation.period
    for(i in start_index:length(available_values)) {
      acf.cor.sum <- acf.cor.sum + ((available_values[counter] - mean.value) * (available_values[i] - mean.value))
    }
    return(acf.cor.sum)
  }
  while(!is.null(value <- btc_stream.next_value())) {
    available_values[counter] <- value
    if(counter > correlation.period) {
      mean.value <- sum(available_values) / length(available_values)
      acf.variance.sum <- acf.variance.sum + (available_values[counter] - mean.value)^2
      acf.cor.sum <- acf.cor.func(mean.value, available_values)
      acf.vector[counter] <- acf.cor.sum / acf.variance.sum
    }
    else {
      acf.vector[counter] <- NaN
    }
    counter <- counter + 1
  }
  return(acf.vector)
}

plot.btc.non_window_statistics <- function() {
  plot(x=btc_data_stream$timestamp, y=btc_data_stream$size, main="BTC", xlab="timestamp", ylab="Block size")
  means <- btc.incremental.mean()
  lines(x=means, col="blue")
  sd <- btc.incremental.sd()
  plot(x=btc_data_stream$timestamp, y=sd, col="red", type = "l", xlab="timestamp", ylab="standard deviation")
  acf <- btc.acf()
  plot(acf, col="red", type = "l", ylab="autocorrelation")
}