source("preprocessing/eth_data_stream.R")

eth.incremental.mean <- function() {
  counter <- 1
  means.vector <- c()
  eth_stream.init()
  while(!is.null(current <- eth_stream.next_value())) {
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

eth.incremental.sd <- function() {
  eth_stream.init()
  counter <- 1
  sum.squares <- 0
  sum.all <- 0
  sd.vector <- c()
  while(!is.null(current <- eth_stream.next_value())) {
    sum.squares <- sum.squares + current ^ 2
    sum.all <- sum.all + current
    sd.vector[counter] <- sqrt((sum.squares-(sum.all^2/counter))/(counter - 1))
    counter <- counter + 1
  }
  return(sd.vector)
}

eth.incremental.acf <- function() {
  eth_stream.init()
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
  while(!is.null(value <- eth_stream.next_value())) {
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

plot.eth.non_window_statistics <- function() {
  plot(x=eth_data_stream$timestamp, y=eth_data_stream$size, main="ETH", xlab="timestamp", ylab="Block size")
  means <- eth.incremental.mean()
  lines(x=means, col="blue")
  sd <- eth.incremental.sd()
  plot(x=eth_data_stream$timestamp, y=sd, col="red", type = "l", xlab="timestamp", ylab="standard deviation")
  acf <- eth.incremental.acf()
  plot(acf, col="red", type = "l", ylab="autocorrelation")
}