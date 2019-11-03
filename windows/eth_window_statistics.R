source("preprocessing/eth_data_stream.R")
source("queue.R")

eth.window.adwin.statistics <- function(win.size, delta.confidence) {
  eth_stream.init()
  counter <- 1
  window.sizes <- c()
  window.means <- c()
  window.sd <- c()
  window.acf <- c()
  mintMinWinLength <- 3
  
  window <- queue.init()
  while(!is.null(current <- eth_stream.next_value()) && (counter <= win.size)) {
    window <- queue.insert(window, current)
    counter <- counter + 1
  }
  
  while(!is.null(current)) {
    window <- queue.insert(window, current)
    
    repeat {
      split.means.equal <- TRUE
      max_size <- length(window)-1
      s <- 1
      while(s <= max_size) {
        window.lenght <- length(window)
        split1.indexes <- 1:s
        split2.indexes <- (s+1):window.lenght
        
        m <- 1 / ((1 / length(split1.indexes)) + (1 / length(split2.indexes)))
        dd <- log10(2 * log10(window.lenght) / delta.confidence)
        eps <- sqrt(2 * m * var(window) * dd) + 2/3 * dd * m
        mean1 <- mean(window[split1.indexes])
        mean2 <- mean(window[split2.indexes])
        mean_delta = abs(mean1 - mean2)
        s <- s + 1
        if(mean_delta > eps) {
          split.means.equal <- FALSE
          break
        }
      }
      if(split.means.equal) break
      window <- queue.delete(window)
    }
    window.sizes <- c(window.sizes, length(window))
    window.means <- c(window.means, mean(window))
    window.sd <- c(window.sd, sd(window))
    window.acf <- c(window.acf, acf(window, plot = FALSE, lag.max = 1)[1]$acf[1])
    counter <- counter + 1
    current <- eth_stream.next_value()
  }
  return(list(size=window.sizes, means=window.means, sd=window.sd, acf=window.acf))
}

plot.eth.window.statistics <- function(win.size, delta) {
  old.par <- par(mfrow=c(2,1), oma=c(0,0,0,0), mar=c(1,4,1,1))
  on.exit(par(old.par))
  plot(x=eth_data_stream$timestamp, y=eth_data_stream$size, main="ETH", xlab="timestamp", ylab="Block size")
  wa <- eth.window.adwin.statistics(win.size, delta)
  lines(c(rep(NaN, win.size), wa$means), col="blue")
  plot(wa$size, col="red", type = "l")
  plot(wa$sd, main="ETH", xlab="timestamp", ylab="Standard deviation", type = "l")
  plot(wa$acf, main="ETH", xlab="timestamp", ylab="Autocorrelation", type = "l")
}