btc_dataset_path = "Datasets/btc.csv"

btc_data_stream = read.csv(btc_dataset_path)

remove(btc_dataset_path)

btc_stram.pointer <- 0

btc_stream.init <- function() {
  btc_stram.pointer <<- 0
}

btc_stream.next_value <- function() {
  btc_stram.pointer <<- btc_stram.pointer + 1
  if(btc_stram.pointer > length(btc_data_stream$size)) {
    return(NULL)
  }
  else {
    return(btc_data_stream$size[btc_stram.pointer])
  }
}