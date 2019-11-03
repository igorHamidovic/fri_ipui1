# provides basic function on a queue
# required for simulating a FIFO window on a stream

queue.init <- function() {
  return(c())  
}

queue.insert <- function(q, element) {
  c(q, element)  
}

queue.delete <- function(q) {
  q[2:length(q)]  
}

queue.move <- function(q, element) {
  queue.insert(q, element)
  queue.delete(q)
  return(q)  
}

