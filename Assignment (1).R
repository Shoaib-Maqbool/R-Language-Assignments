#Assessment 3

#Task 1,1
#File saved in the working directory

#Task 1.2
#Reading the txt file
EnergyData = as.matrix(read.table("Energy19.txt"))

#Task 1.3
#Creating a sample
SampleEnergyData = EnergyData[sample(1:671,300),c(1:6)]

#Task 1.4
#Visualizing different relations
plot(SampleEnergyData[,1],SampleEnergyData[,6], xlab= "Tempreture in Kitchen Area in celcius",ylab = "Energy Use in appliances", main = "Tempreture in Kitchen with respect to Energy Use Applicances")

plot(SampleEnergyData[,2],SampleEnergyData[,6], xlab= "Humidity in Kitchen in %",ylab = "Energy Use in appliances", main = "Humidity in Kitchen with respect to Energy Use Applicances") 

plot(SampleEnergyData[,3],SampleEnergyData[,6], xlab= "Tempreture Outside in Celcius",ylab = "Energy Use in appliances", main = "Tempreture Outside with respect to Energy Use Applicances")

plot(SampleEnergyData[,4],SampleEnergyData[,6], xlab= "Humidity Outside in %",ylab = "Energy Use in appliances", main = "Humidity Outside with respect to Energy Use Applicances")

plot(SampleEnergyData[,5],SampleEnergyData[,6], xlab= "Visibility in km",ylab = "Energy Use in appliances", main = "Visibility with respect to Energy Use Applicances") 

hist(SampleEnergyData[,1], xlab = "Tempreture in Kitchen Area", main = "Historgam on X1")

hist(SampleEnergyData[,2], xlab = "Humidity in Kitchen Area", main = "Historgam on X2")

hist(SampleEnergyData[,3], xlab = "Tempreture Outside", main = "Historgam on X3")

hist(SampleEnergyData[,4], xlab = "Humidity Outside", main = "Historgam on X4")

hist(SampleEnergyData[,5], xlab = "Visibility", main = "Historgam on X5")

hist(SampleEnergyData[,6], xlab = "Energy Use of Appliances", main = "Historgam on Y")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Task 2.1
#Applying Linear Scanling funtion
TransformData = function(x){
a = min(x)
b = max(x)
c = (x-a)/(b-a)
c
}

#Scaling & Normalizing X2
TD2 = TransformData(SampleEnergyData[,2])
TX2 = TD2^0.8
hist(TX2,xlab="Humidity in Kitchen", main = "Transformed & Normailized Data")

#Scaling & Normalizing X3
TD3 = TransformData(SampleEnergyData[,3])
TX3 = TD3^0.99
hist(TX3, xlab="Tempreture Outside", main = "Transformed & Normailized Data")

#Scaling & Normalizing X4
TD4 = TransformData(SampleEnergyData[,4])
TX4 = TD4^0.99
hist(TX4, xlab="Humidity Outside", main = "Transformed & Normailized Data")

#Scaling & Normalizing X5
TD5 = TransformData(SampleEnergyData[,5])
TX5 = TD5^0.641
hist(TX5, xlab="Visibility", main = "Transformed & Normailized Data")

#Scaling & Normalizing Y
TDY = TransformData(SampleEnergyData[,6])
TY = TDY^0.59
hist(TY, xlab="Energy use of Appliances", main = "Transformed & Normailized Data")

#Binding
TransformedData = cbind(TX2,TX3,TX4,TX5,TY)
summary(TransformedData)
write.table(TransformedData,"Shoaib-transformed.txt")

#----------------------------------------------------------------------------------#

# Task3.1

source("AggWaFit718.R")

# Task 3.2
#Performing Different aggregation functions
#WAM
fit.QAM(TransformedData,"Weighted Airthmatic Mean Output.txt","Weighted Airthmatic Mean Status.txt")

#OWA
fit.OWA(TransformedData,"Ordered Weighting Average Output.txt","Ordered Weighting Average Status.txt")
 
#Choquet Integral
fit.choquet(TransformedData,"Choquet integral Output.txt","Choquet Integral Status.txt")

#WPM 2
fit.QAM(TransformedData,"Weighted Power mean 2 Output.txt","Weighted Power Mean 2 Status.txt",g = QM,g.inv = invQM)

#WPM 0.5
fit.QAM(TransformedData,"Weighted Power mean 0.5 Output.txt","Weighted Power Mean 0.5 Status.txt",g = PM05,g.inv = invPM05)
