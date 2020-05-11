#setwd("josh")
library(data.table)

data <- read.csv("GSE70766.csv")
fl <- read.csv("list.csv")
new <- read.csv("new1.csv")

data$ï..tracking_id <- toupper(data$ï..tracking_id)
i <- 1

ge <- fl[,1]

cat <- fl[,2]




com <- function(gene){
  

  write1 <- data[data$ï..tracking_id == gene,]
  print(write1)
  track <- data[data$ï..tracking_id == gene,1]
  Villus_1 <- data[data$ï..tracking_id == gene,2]
  Villus1 <- data[data$ï..tracking_id == gene,3]
  Villus_3 <- data[data$ï..tracking_id == gene,4]
  Lgr5_1 <- data[data$ï..tracking_id == gene,5]
  Lgr5_2 <- data[data$ï..tracking_id == gene,6]
  Lgr5_3 <- data[data$ï..tracking_id == gene,7]
  
  info <- c(Villus_1,Villus1,Villus_3,Lgr5_1,Lgr5_2,Lgr5_3)
  print(info)
  std <- c(Villus_1,Villus1,Villus_3)
  z_score <- ((log2(Villus_1) - (log2((Villus_3 + Villus1 + Villus_1)/3)) ) ) / (sqrt(sum((std-mean(std))^2/(length(std)-1))))
  #print("the z---score is :::::")
  #print(z_score)
  
  meanfold <- ((((Villus1 + Villus_3 + Villus_1)/3))/(((Lgr5_3 +Lgr5_2 + Lgr5_1)/3)))
  meandfold_change <- log2(meanfold)
  #print("--------------------------------------------")
  #print("the meanfold change is:")
  #print(meandfold_change)
  
  newdata <- data.frame("trackong id" = track,"Villus_1" = Villus_1, "villus1/villus2" = Villus1, "villus_3" = Villus_3, "Lgr5_1" = Lgr5_1, "Lgr5_2" = Lgr5_2, "Lgr5_3" = Lgr5_3, stringsAsFactors = FALSE, "Category" = a1[i], "zscore" = z_score, "mean_fold_change" = meandfold_change)
  write.csv(newdata,'new1.csv')
   
  write.table(newdata, file = "table.csv")
  print(track)
}


y <- fl[,1]

y2 <- cat

y3 <- fl[,3]

a <- array(y,dim = c(1,length(y),1))

a1 <- array(y2,dim = c(1,length(y),1))

a3 <- array(y3,dim = c(1,length(y),1))






mat <- matrix(a,nrow = 204,ncol = 1)   

work <- c(a)

# print(work)
# 
# com(a[12])
# 
# vector1 <- c(com(a[12]),a1[12])
# dataf <- data.frame(vector1, stringsAsFactors = FALSE)
# write.csv(dataf,'new1.csv')
# vector2 <- c(com(a[11]),a1[11])
# dataf2 <- data.frame(vector2, stringsAsFactors = FALSE)
# 


# write.csv(dataf2,'new1.csv')



# 
# while (i < length(a)) {0
#     
#   vector1 <- c(com(a[i]),a1[i])
#   print(vector1)
#   dataf <- data.frame(com(a[i]), stringsAsFactors = FALSE)
#   write.csv(dataf,'new1.csv')
#   i = i+1
# 
#   }


# 
# #   
#  for (i in 1:length(a)){
#   write.csv(com(a[i]), file = paste0("Filtered", a[i], ".csv"))
# }


