setwd("C:/Users/amara/OneDrive - Sri Lanka Institute of Information Technology/SLIIT/2Y1S/Probaility and Statistics/Labs/Lab 04/Lab 04-20250822")
data <-read.table("Exercise.txt",header = TRUE , sep = ",")
fix (data)
attach(data)



#2.Indentify Variables 
#Branch ---- Categorical (Nominal)
#Sales_X1       ---- Numeric (Ratio Scale)
#Advertising_X2 ---- Numeric (Ratio Scale)
#Years_X3       ---- Numeric (Ratio Scale)

#part 02 A
boxplot(data$Sales_X1,main = "Box plot for Sales ", outline = TRUE ,outpch = 8 ,horizontal = TRUE)


summary(data$Advertising_X2)
fivenum(data$Advertising_X2)
IQR(data$Advertising_X2)

find_outliers <-function(x){
  Q1 <- quantile(x,0.25)
  Q3 <- quantile(x,0.75)
  
  IQR_val  <- Q3 -Q1
  lower <-  Q1 - 1.5* IQR_val
  upper <-  Q3 + 1.5* IQR_val
  
  outliers <- x[x<lower | x > upper]
  return(outliers)
}
find_outliers(data$Years_X3)