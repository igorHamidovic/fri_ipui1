eth_dataset_path = "Datasets/eth.csv"

eth_data_stream = read.csv(eth_dataset_path)

remove(eth_dataset_path)

eth_stram.pointer <- 0

eth_stream.init <- function() {
  eth_stram.pointer <<- 0
}

eth_stream.next_value <- function() {
  eth_stram.pointer <<- eth_stram.pointer + 1
  if(eth_stram.pointer > length(eth_data_stream$size)) {
    return(NULL)
  }
  else {
    return(eth_data_stream$size[eth_stram.pointer])
  }
}