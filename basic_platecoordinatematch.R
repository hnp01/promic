input_file <- "C:/Users/phamh2/Documents/R/Examples/2.csv"
output_file <- "C:/Users/phamh2/Documents/R/Examples/21.csv"
data <- read_csv(input_file)

x = letters[1:9]

for (i in data){
#  print (x[((i-1) %% 9)+1])
  result = paste0 ((x[((i-1) %% 9)+1]), as.integer((i-1)/9 + 1)) 
  print (result)
}

e = as.data.frame(toupper(result))
#options(max.print=7000)

coordinate = cbind(data, e)

if (file.exists(output_file)) {
  stop("Output already exist")
} else {
  write_csv(coordinate, output_file)
}

##for (i in data){print (i)}
##e = as.data.frame(i)
##view (e)

#i = seq(1, 9, by =1)
#while ((i-1) %% 9 < 9) {
#  print(x[i])
#}

#i = 0
#while (i < 81) {
#  print(x[(i) %% 9 +1])
#  i = i+1
#}
