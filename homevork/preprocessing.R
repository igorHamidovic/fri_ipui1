data_source.directory = "/home/sebastihol/Documents/Fax Slovenija/Predmeti/Teme iz vestacke inteligencije 1/fri_ipui1/Datasets"
data_source.files = c("vm_cpu_readings-file-1-of-195.csv", "vm_cpu_readings-file-2-of-125.csv", "vm_cpu_readings-file-3-of-125.csv", 
                       "vm_cpu_readings-file-4-of-125.csv", "vm_cpu_readings-file-5-of-125.csv", "vm_cpu_readings-file-6-of-125.csv", 
                       "vm_cpu_readings-file-7-of-125.csv", "vm_cpu_readings-file-8-of-195.csv", "vm_cpu_readings-file-9-of-195.csv", 
                       "vm_cpu_readings-file-10-of-195.csv")

path = sprintf("%s/%s", data_source.directory, data_source.files[1])
  new_dataset = read.csv(path, header = TRUE)
new_dataset$min_cpn <- NULL
new_dataset$max_cpn <- NULL
new_dataset$avg_cpn = as.numeric(format(round(new_dataset$avg_cpn, 2)))

vi_ids = unique(new_dataset$vm_id)
test = new_dataset[new_dataset$vm_id == vi_ids[5000],]


data_source.append_dataset <- function(path, dataset) {
  new_dataset <- read.csv(path, header = FALSE)
  
  if(is.null(dataset)) {
    dataset <- new_dataset
  }
  else {
    append(dataset, new_dataset, after = length(dataset))
  }
  return(dataset)
}

data_source.dataset <- c()

for(i in 1:length(data_source.files)) {
  path <- sprintf("%s/%s", data_source.directory, data_source.files[i])
  data_source.dataset <- data_source.append_dataset(path, data_source.dataset)
}
