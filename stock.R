library(forecast)
library(fpp)
library(tools)

filepath = "/home/harshith/Documents/CSE587/Assignments/Dataset/Small"
setwd(filepath)
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
numfiles = length(listcsv)
MAEVAL1 = matrix(NA,numfiles,1,byrow = TRUE)
MAEVAL2 = matrix(NA,numfiles,1,byrow = TRUE)
MAEVAL3 = matrix(NA,numfiles,1,byrow = TRUE)
STOCK = matrix(NA,numfiles,1,byrow = TRUE)

for (k in 1:numfiles){
  filename = listcsv[k]
  count <- length(readLines(filename))
  if(count == 755)  {
    if(file.info(filename)[1]>0) {
      
      # read one csv file into variable (DO NOT EDIT)
      textData=read.csv(file=filename, header=T)
      
      stockname = noquote(file_path_sans_ext(filename))
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE1 = matrix(NA,1,length(testData))
      MAE2 = matrix(NA,1,length(testData))
      MAE3 = matrix(NA,1,length(testData))
      
      # apply ARIMA model (DO NOT EDIT)
      fitData1 = auto.arima(trainData)
      fitData2 = tslm(trainData~trend)
      fitData3 = HoltWinters(trainData,gamma=FALSE)
      
      # apply forecast(DO NOT EDIT)
      forecastData1 = forecast(fitData1, h=length(testData))
      forecastData2 = forecast(fitData2, h=length(testData))
      forecastData3 = forecast(fitData3, h=length(testData))
      
      
      # calculate Mean Absolute Error 
      for(i in 1:length(testData))
      {
        MAE1[1,i] = abs(forecastData1$mean[i] - testData[i])
        MAE2[1,i] = abs(forecastData2$mean[i] - testData[i])
        MAE3[1,i] = abs(forecastData3$mean[i] - testData[i])
      }
      
      # this is the result you need for stock AAPL
      #print (stockname)
      #print (sum(MAE1[1,1:10]))
      #print (sum(MAE2[1,1:10]))
      #print (sum(MAE3[1,1:10]))
      MAEVAL1[k,] = (sum(MAE1[1,1:10]))
      MAEVAL2[k,] = (sum(MAE2[1,1:10]))
      MAEVAL3[k,] = (sum(MAE3[1,1:10]))
      STOCK[k,] = stockname
      
    }
  }
}
stockdf1 = data.frame(STOCK,MAEVAL1)
sortmae1 <- stockdf1[order(stockdf1$MAEVAL),]
output1 = sortmae1[c(1:10),]
stockdf2 = data.frame(STOCK,MAEVAL2)
sortmae2 <- stockdf2[order(stockdf2$MAEVAL),]
output2 = sortmae2[c(1:10),]
stockdf3 = data.frame(STOCK,MAEVAL3)
sortmae3 <- stockdf3[order(stockdf3$MAEVAL),]
output3 = sortmae3[c(1:10),]
print ("****ARIMA MINIMUM TEN****")
print (output1)
print ("****LINEAR REGRESSION MINIMUM TEN****")
print (output2)
print ("****HOLTWINTERS MINIMUM TEN****")
print (output3)

outpath = "/gpfs/courses/cse587/spring2015/students/harshith/hw2"
setwd(outpath)
plot.new()
jpeg('arima.jpg')
plot(output1[1:10,2], main="ARIMA",sub="", xlab="STOCK NAME", ylab="Mean Absolute Error",xaxt='n') 
lines(output1[1:10,2],lw = 2, col = "red") 
axis(1,at=1:10,labels=output1[1:10,1])
dev.off()
plot.new()
jpeg('lr.jpg')
plot(output2[1:10,2], main="LINEAR REGRESSION",sub="", xlab="STOCK NAME", ylab="Mean Absolute Error",xaxt='n') 
lines(output2[1:10,2],lw = 2, col = "red") 
axis(1,at=1:10,labels=output2[1:10,1])
dev.off()
plot.new()
jpeg('hw.jpg') 
plot(output3[1:10,2], main="HOLTWINTERS",sub="", xlab="STOCK NAME", ylab="Mean Absolute Error",xaxt='n') 
lines(output3[1:10,2],lw = 2, col = "red") 
axis(1,at=1:10,labels=output3[1:10,1])
dev.off()
