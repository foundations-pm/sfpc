
> filenames <- sort(list.files(path="Datasets/weightloss"))
> filenames
[1] "baseline.csv" "weights1.csv" "weights2.csv" "weights3.csv" 
"weights4.csv" "weights5.csv" "weights6.csv" "weights7.csv" 
"weights8.csv" "weights9.csv"

#list.files() lists all the files currently stored in the
#working directory, but we can add in a different path or
#subfolder instead. The sort function sorts them in
#alphabetical order.

for (i in 1:9) {
  l[[i]] <-
    read.csv(paste("Datasets/weightloss/", filenames[i], sep=""))
}

#read.csv reads each file into R and saves each as a separate
#item in a list that we have chosen to name ‘l’. Each data frame 
#hasn’t been given a name, they are just numbered items and can 
#be accessed if required like so:
 
 l[[1]]

 #By running this code, we can see the weight measurements from 
#the first individual (originally stored in the spreadsheet 
#called weights1.csv)
 
install.packages("plyr")
require(plyr)

#Next, we need to use a function called rbind.fill that 
#can bind together the weights of all individuals. This 
#function is stored in the package plyr. We introduced this 
#function earlier in section 5.

weights.row <- rbind.fill(l)

#The advantage of using this function instead of the rbind, is
#that it allows us to combine together the rows of many data
#frames stored in a list. Using rbind would mean only being able 
#to combine two data frames at a time.
