################################################################################################################
## This program iterates over the specified datasets, and computes the RMSE of the approximation formulas
################################################################################################################

#set to True if you want to enable filtering of K > 200, else set to False
data_filter_200 <- F


readFromfileName = "topX.csv.original.zip"

if(data_filter_200){
  model_file_name <- "./models/models_fitting.remove_top_25_percent_and_values_gt_200.rda"
  models_performance_file_name <- "./models/models_performance.remove_top_25_percent_and_values_gt_200.csv"
  filter_name <- "DS2"
}else{
  model_file_name <- "./models/models_fitting.remove_top_25_percent.rda"
  models_performance_file_name <- "./models/models_performance.remove_top_25_percent.csv"
  filter_name <- "DS1"
}

load(model_file_name)
#####################################################################################################################
#####                          define approximation formulas                                                    #####
#####################################################################################################################

# The "flexible" model a(X, N) * K^b(X,N)
# K == topic count
# X == the top-X 
# N == number of documents
model_two_param_simple <- function(K, X, N){
  
  val <- data.frame(X = X, N = N)
  a <- predict(dat.lm.intercept, val) # 1.171 + 0.649 * log(X) - 0.116 * log(N)
  b <- predict(dat.lm.slope, val)  # -0.841 + 0.002 * X + 0.00003 * N
  
  exp(a  + b * log(K))
}

model_two_param_complex <- function(K, X, N){
  
  val <- data.frame(X = X, N = N)
  a <- predict(dat.lm.intercept.complex, val) 
  b <- predict(dat.lm.slope.complex, val) 
  
  exp(a  + b * log(K))
}

# The "constrained" model X^-b(X, N) * K^b(X,N)
# K == topic count
# X == the top-X 
# N == number of documents

model_one_param_simple <- function(K, X, N){
  val <- data.frame(X = X, N = N)
  b <- predict(dat.lm.b, val)  #-8.408351E-01 + 2.110667E-03 * X + 3.397901E-05 * N
  
  X^-b * K^b
}

model_one_param_complex <- function(K, X, N){
  val <- data.frame(X = X, N = N)
  b <- predict(dat.lm.b.complex, val)
  
  X^-b * K^b
}


#####################################################################################################################
#####                                           main                                                            #####
#####################################################################################################################

dat <- read.csv(unz(readFromfileName, "topX.csv.original"), header = T, sep = ",", row.names = NULL)
validGroups <- unique(paste(dat$timeframe_type, dat$timeframe, dat$dataset_name, sep = " "))

for(iG in 1:length(validGroups)) {
  s <- strsplit(validGroups[iG], " ")
  sdf <- as.data.frame(s)
  p1 <- as.character(sdf[1,]) ## timeframe_type
  p2 <- as.character(sdf[2,]) ## timeframe
  p3 <- as.character(sdf[3,]) ## dataset_name
  
  cat("p1=", p1, " p2=", p2, " p3=", p3, " iG=", iG, "\n")
  
  
  pdf(paste(paste("./figures/fit_model_one_param_complex_", p3, p1, p2, filter_name, sep = "_"), ".pdf", sep = ""))
  line_counter <- 1
  topX_values_to_plot <- c(5,10,15,50)
  for(itopXX in topX_values_to_plot) {
    #get the data to plot
    ds <- dat[dat$timeframe_type ==  p1 & dat$timeframe == p2 & dat$dataset_name == p3 & dat$topXX == itopXX,]

    if(data_filter_200){ #if True -- keep only values of K <= 200
      ds <- ds[ds$topicCount <= 200, ]
    }
    ds <- ds[ds$topicCount < 0.75 * max(ds$documentCount), ] 
    

    F.model_one_param_complex <- model_one_param_complex(ds$topicCount, ds$topXX[1], ds$documentCount[1])
    
    #let's plot the actual data now
    if(line_counter == 1){ #if the first line to draw -- use plot function and setup the figure
      plot(
        ds$topicCount, ds$postFraction,
        log = "xy",
        main = paste("Dataset:", p3, p1, p2 ),
        sub = paste("Filter name:", filter_name),
        xlab = "K", ylab = "F",
        col = line_counter,
        type = "l"
      )
    }else{ #else use lines function
      lines(
        ds$topicCount, ds$postFraction,
        col = line_counter,
        type = "l"
        
      )
    }
    
    #and add the pine for prediction
    lines(
      ds$topicCount, F.model_one_param_complex,
      col = line_counter,
      type = "l",
      lty = 2
    )
    
    line_counter <- line_counter + 1
  }
  #add grid and legend to the plot
  grid (lty = "dotted")
  legend("bottomleft", 
         legend = paste("X =", topX_values_to_plot),
         col = c(1:length(topX_values_to_plot)), lty = 1)
  dev.off()

}
