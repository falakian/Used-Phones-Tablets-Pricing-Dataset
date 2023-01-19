install.packages("psych")
install.packages("ggplot2")
install.packages("plotrix")
library(epiDisplay)
library(plotrix)
library(modeest)
data <- read.csv("D:\\salary statistics\\salary\\used_device_data.csv")

tableBrand<-table(data$device_brand)

tab1(data$device_brand, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای برند ها  ", ylab = "فراوانی", col=c("red"), border = "black")

tab1(data$device_brand, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای برند ها", bar.values = "percent", ylab="درصد فراوانی", col=c("red"), border = "black")

pie(tableBrand)

###

tab1(data$os, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای سیستم عامل ها  ", ylab = "فراوانی", col=c("red"), border = "black")

tab1(data$os, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای سیستم عامل ها  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

tableOs<-table(data$os)

pie(tableOs)

##

tab1(data$four_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ", ylab = "فراوانی", col=c("red"), border = "black")

tab1(data$four_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

table4G<-table(data$four_g)

pie(table4G)

##

tab1(data$five_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ", ylab = "فراوانی", col=c("red"), border = "black")

tab1(data$five_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

table5G<-table(data$five_g)

pie(table5G)

###

# شاخص های مرکزگرا
Average <- mean(data$screen_size , na.rm = T) # میانگین
Median <- median(data$screen_size , na.rm = T) # میانه
Mode <- mfv(data$screen_size)

# شاخص های پراکندگی
minimum <- min(data$screen_size , na.rm = T)
maximum <- max(data$screen_size , na.rm = T)
range <- range(data$screen_size , na.rm = T)
standardDeviation <- sd(data$screen_size , na.rm = T) # انحراف معیار

variance <- var(data$screen_size , na.rm = T) # واریانس

cat ("داده های مربوط به سایز اسکرین", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

hist(data$screen_size , breaks=10, main="نمودار مستطیلی سایز اسکرین", ylab = "فراوانی", xlab = "اسکرین", col=c("red"), border = "black")

boxplot(data$screen_size,horizontal = TRUE,  main="نمودار جعبه ای مربوط به اسکرین", xlab = "اسکرین", col=c("red"), border = "black")

###

# شاخص های مرکزگرا
Average <- mean(data$rear_camera_mp , na.rm = T) # میانگین
Median <- median(data$rear_camera_mp , na.rm = T) # میانه
Mode <- mfv(data$rear_camera_mp)

# شاخص های پراکندگی
minimum <- min(data$rear_camera_mp , na.rm = T)
maximum <- max(data$rear_camera_mp , na.rm = T)
range <- range(data$rear_camera_mp , na.rm = T)
standardDeviation <- sd(data$rear_camera_mp , na.rm = T) # انحراف معیار

variance <- var(data$rear_camera_mp , na.rm = T) # واریانس

cat ("داده های مربوط به وضوح دوربین عقب بر حسب مگاپیکسل", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

hist(data$rear_camera_mp , breaks=10, main="نمودار مستطیلی وضوح دوربین عقب بر حسب مگاپیکسل", ylab = "فراوانی", xlab = "مگاپیکسل", col=c("red"), border = "black")

boxplot(data$rear_camera_mp,horizontal = TRUE,  main="نمودار جعبه ای مربوط به وضوح دوربین عقب بر حسب مگاپیکسل", xlab = "مگاپیکسل", col=c("red"), border = "black")

###

# شاخص های مرکزگرا
Average <- mean(data$front_camera_mp , na.rm = T) # میانگین
Median <- median(data$front_camera_mp , na.rm = T) # میانه
Mode <- mfv(data$front_camera_mp)

# شاخص های پراکندگی
minimum <- min(data$front_camera_mp , na.rm = T)
maximum <- max(data$front_camera_mp , na.rm = T)
range <- range(data$front_camera_mp , na.rm = T)
standardDeviation <- sd(data$front_camera_mp , na.rm = T) # انحراف معیار

variance <- var(data$front_camera_mp , na.rm = T) # واریانس

cat ("داده های مربوط به وضوح دوربین جلو بر حسب مگاپیکسل", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

hist(data$front_camera_mp , breaks=10, main="نمودار مستطیلی وضوح دوربین جلو بر حسب مگاپیکسل", ylab = "فراوانی", xlab = "مگاپیکسل", col=c("red"), border = "black")

boxplot(data$front_camera_mp,horizontal = TRUE,  main="نمودار جعبه ای مربوط به وضوح دوربین جلو بر حسب مگاپیکسل", xlab = "مگاپیکسل", col=c("red"), border = "black")

###

# شاخص های مرکزگرا
Average <- mean(data$internal_memory , na.rm = T) # میانگین
Median <- median(data$internal_memory , na.rm = T) # میانه
Mode <- mfv(data$internal_memory)

# شاخص های پراکندگی
minimum <- min(data$internal_memory , na.rm = T)
maximum <- max(data$internal_memory , na.rm = T)
range <- range(data$internal_memory , na.rm = T)
standardDeviation <- sd(data$internal_memory , na.rm = T) # انحراف معیار

variance <- var(data$internal_memory , na.rm = T) # واریانس

cat ("داده های مربوط به مقدار حافظه داخلی بر حسب گیگابایت", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

hist(data$internal_memory , breaks=10, main="نمودار مستطیلی مقدار حافظه داخلی بر حسب گیگابایت", ylab = "فراوانی", xlab = "گیگابایت", col=c("red"), border = "black")

boxplot(data$internal_memory,horizontal = TRUE,  main="نمودار جعبه ای مربوط به مقدار حافظه داخلی بر حسب گیگابایت", xlab = "گیگابایت", col=c("red"), border = "black")

####

# شاخص های مرکزگرا
Average <- mean(data$ram , na.rm = T) # میانگین
Median <- median(data$ram , na.rm = T) # میانه
Mode <- mfv(data$ram)

# شاخص های پراکندگی
minimum <- min(data$ram , na.rm = T)
maximum <- max(data$ram , na.rm = T)
range <- range(data$ram , na.rm = T)
standardDeviation <- sd(data$ram , na.rm = T) # انحراف معیار

variance <- var(data$ram , na.rm = T) # واریانس

cat ("داده های مربوط به مقدار رم به گیگابایت", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

hist(data$ram , breaks=10, main="نمودار مستطیلی مقدار رم به گیگابایت", ylab = "فراوانی", xlab = "گیگابایت", col=c("red"), border = "black")

boxplot(data$ram,horizontal = TRUE,  main="نمودار جعبه ای مربوط به مقدار رم به گیگابایت", xlab = "گیگابایت", col=c("red"), border = "black")

####

# شاخص های مرکزگرا
Average <- mean(data$battery , na.rm = T) # میانگین
Median <- median(data$battery , na.rm = T) # میانه
Mode <- mfv(data$battery )

# شاخص های پراکندگی
minimum <- min(data$battery , na.rm = T)
maximum <- max(data$battery , na.rm = T)
range <- range(data$battery , na.rm = T)
standardDeviation <- sd(data$battery , na.rm = T) # انحراف معیار

variance <- var(data$battery , na.rm = T) # واریانس

cat ("داده های مربوط به ظرفیت انرژی باتری دستگاه بر حسب میلی آمپر ساعت", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

hist(data$battery , breaks=10, main="نمودار مستطیلی ظرفیت انرژی باتری دستگاه بر حسب میلی آمپر ساعت", ylab = "فراوانی", xlab = "میلی آمپر ساعت", col=c("red"), border = "black" )

boxplot(data$battery ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به ظرفیت انرژی باتری دستگاه بر حسب میلی آمپر ساعت", xlab = "میلی آمپر ساعت", col=c("red"), border = "black")

####
# کوواریانس و ضریب وابستگی بین سایز اسکرین و ظرفیت باتری
xy<- data$screen_size* data$battery
Averagexy <- mean(xy , na.rm = T) # میانگین

Averagex <- mean(data$screen_size , na.rm = T) # میانگین
Averagey <- mean(data$battery , na.rm = T) # میانگین

covariance <- (Averagexy -(Averagex*Averagey))

#covariance <- cov(data$screen_size, data$battery)

# Coefficient of Correlation ضریب وابستگی
CoefCorr <- (covariance / ((sd(data$screen_size , na.rm = T)) * sd(data$battery , na.rm = T))) 

cat("کوواریانس بین سایز اسکرین و ظرفیت باتری : ", covariance, "\nضریب وابستگی بین سایز اسکرین و ظرفیت باتری : ", CoefCorr)
