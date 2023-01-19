
#نصب کردن پکیج های مورد استفاده 
# شاید به اینترنت نیاز باشد
install.packages("psych")
install.packages("ggplot2")
install.packages("plotrix")
library(epiDisplay)
library(plotrix)
library(modeest)
 # خواندن دیتای برنامه
# آدرس فایل مورد نظر را آدرس فایل در کاکپیوتر خودتان بزارید
data <- read.csv("D:\\statistics\\Used-Phones-Tablets-Pricing-Dataset\\used_device_data.csv")


## برای متغییر های کیفی 

tableBrand<-table(data$device_brand)
# نمودار میله ای برای برند ها
tab1(data$device_brand, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای برند ها  ", ylab = "فراوانی", col=c("red"), border = "black")
#  نمودار میله ای برای برند ها با درصد فراوانی

tab1(data$device_brand, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای برند ها", bar.values = "percent", ylab="درصد فراوانی", col=c("red"), border = "black")

#  نمودار دایره ای فراوانی برند ها
pie(tableBrand)


# نمودار میله ای برای سیستم عامل ها

tab1(data$os, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای سیستم عامل ها  ", ylab = "فراوانی", col=c("red"), border = "black")
# نمودار میله ای برای سیستم عامل ها با درصد فراوانی

tab1(data$os, sort.group = "decreasing", cum.percent = TRUE, main="نمودار میله ای سیستم عامل ها  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

tableOs<-table(data$os)
 # نمودار دایره ای فراوانی سیستم عامل ها
pie(tableOs)


# نمودار میله ای برای دسترسی به فور جی

tab1(data$four_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ", ylab = "فراوانی", col=c("red"), border = "black")

# نمودار میله ای برای دسترسی به فور جی با درصد فراوانی

tab1(data$four_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

table4G<-table(data$four_g)
# نمودار دایره ای فراوانی دسترسی به فور جی

pie(table4G)


# نمودار میله ای برای دسترسی به فایو جی

tab1(data$five_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ", ylab = "فراوانی", col=c("red"), border = "black")

# نمودار میله ای برای دسترسی به فایو جی با درصد فراوانی

tab1(data$five_g, sort.group = "decreasing", cum.percent = TRUE, main=" 4G  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

table5G<-table(data$five_g)

# نمودار دایره ای فراوانی دسترسی به فایو جی

pie(table5G)

### برای متغییر های پیوسته

## برای سایز اسکرین

# شاخص های مرکزگرا

Average <- mean(data$screen_size , na.rm = T) # میانگین

Median <- median(data$screen_size , na.rm = T) # میانه

Mode <- mfv(data$screen_size)

# شاخص های پراکندگی

minimum <- min(data$screen_size , na.rm = T) #کم ترین

maximum <- max(data$screen_size , na.rm = T)# بیشترین

range <- range(data$screen_size , na.rm = T)# بازه

standardDeviation <- sd(data$screen_size , na.rm = T) # انحراف معیار

variance <- var(data$screen_size , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به سایز اسکرین", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای سایز اسکرین

hist(data$screen_size , breaks=10, main="نمودار مستطیلی سایز اسکرین", ylab = "فراوانی", xlab = "اسکرین", col=c("red"), border = "black")

# نمودار جعبه ای برای سایز اسکرین

boxplot(data$screen_size,horizontal = TRUE,  main="نمودار جعبه ای مربوط به اسکرین", xlab = "اسکرین", col=c("red"), border = "black")

##برای وضوح دوربین عقب 

# شاخص های مرکزگرا

Average <- mean(data$rear_camera_mp , na.rm = T) # میانگین

Median <- median(data$rear_camera_mp , na.rm = T) # میانه

Mode <- mfv(data$rear_camera_mp)

# شاخص های پراکندگی

minimum <- min(data$rear_camera_mp , na.rm = T) #کم ترین

maximum <- max(data$rear_camera_mp , na.rm = T) # بیشترین

range <- range(data$rear_camera_mp , na.rm = T)# بازه

standardDeviation <- sd(data$rear_camera_mp , na.rm = T) # انحراف معیار

variance <- var(data$rear_camera_mp , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به وضوح دوربین عقب بر حسب مگاپیکسل", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای وضوح دوربین عقب

hist(data$rear_camera_mp , breaks=10, main="نمودار مستطیلی وضوح دوربین عقب بر حسب مگاپیکسل", ylab = "فراوانی", xlab = "مگاپیکسل", col=c("red"), border = "black")

# نمودار جعبه ای برای وضوح دوربین عقب

boxplot(data$rear_camera_mp,horizontal = TRUE,  main="نمودار جعبه ای مربوط به وضوح دوربین عقب بر حسب مگاپیکسل", xlab = "مگاپیکسل", col=c("red"), border = "black")

## برای دوربین جلو

# شاخص های مرکزگرا
Average <- mean(data$front_camera_mp , na.rm = T) # میانگین
Median <- median(data$front_camera_mp , na.rm = T) # میانه
Mode <- mfv(data$front_camera_mp)

# شاخص های پراکندگی
minimum <- min(data$front_camera_mp , na.rm = T)#کم ترین

maximum <- max(data$front_camera_mp , na.rm = T)# بیشترین

range <- range(data$front_camera_mp , na.rm = T)# بازه

standardDeviation <- sd(data$front_camera_mp , na.rm = T) # انحراف معیار

variance <- var(data$front_camera_mp , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به وضوح دوربین جلو بر حسب مگاپیکسل", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای وضوخ دوربین جلو

hist(data$front_camera_mp , breaks=10, main="نمودار مستطیلی وضوح دوربین جلو بر حسب مگاپیکسل", ylab = "فراوانی", xlab = "مگاپیکسل", col=c("red"), border = "black")

# نمودار جعبه ای برای دوربین جلو

boxplot(data$front_camera_mp,horizontal = TRUE,  main="نمودار جعبه ای مربوط به وضوح دوربین جلو بر حسب مگاپیکسل", xlab = "مگاپیکسل", col=c("red"), border = "black")

## برای مقدار حافظه داخلی

# شاخص های مرکزگرا
Average <- mean(data$internal_memory , na.rm = T) # میانگین
Median <- median(data$internal_memory , na.rm = T) # میانه
Mode <- mfv(data$internal_memory)

# شاخص های پراکندگی
minimum <- min(data$internal_memory , na.rm = T)#کم ترین

maximum <- max(data$internal_memory , na.rm = T)# بیشترین

range <- range(data$internal_memory , na.rm = T) # بازه

standardDeviation <- sd(data$internal_memory , na.rm = T) # انحراف معیار

variance <- var(data$internal_memory , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به مقدار حافظه داخلی بر حسب گیگابایت", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای مقدار حافظه داخلی

hist(data$internal_memory , breaks=10, main="نمودار مستطیلی مقدار حافظه داخلی بر حسب گیگابایت", ylab = "فراوانی", xlab = "گیگابایت", col=c("red"), border = "black")

# نمودار جعبه ای برای مقدار حافظه داخلی

boxplot(data$internal_memory,horizontal = TRUE,  main="نمودار جعبه ای مربوط به مقدار حافظه داخلی بر حسب گیگابایت", xlab = "گیگابایت", col=c("red"), border = "black")

## برای مقدار رم

# شاخص های مرکزگرا
Average <- mean(data$ram , na.rm = T) # میانگین
Median <- median(data$ram , na.rm = T) # میانه
Mode <- mfv(data$ram)

# شاخص های پراکندگی
minimum <- min(data$ram , na.rm = T) #کم ترین

maximum <- max(data$ram , na.rm = T)# بیشترین

range <- range(data$ram , na.rm = T)# بازه

standardDeviation <- sd(data$ram , na.rm = T) # انحراف معیار

variance <- var(data$ram , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به مقدار رم به گیگابایت", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای مقدار رم

hist(data$ram , breaks=10, main="نمودار مستطیلی مقدار رم به گیگابایت", ylab = "فراوانی", xlab = "گیگابایت", col=c("red"), border = "black")

# نمودار جعبه ای برای مقدار رم

boxplot(data$ram,horizontal = TRUE,  main="نمودار جعبه ای مربوط به مقدار رم به گیگابایت", xlab = "گیگابایت", col=c("red"), border = "black")

## برای ظرفیت باتری

# شاخص های مرکزگرا
Average <- mean(data$battery , na.rm = T) # میانگین
Median <- median(data$battery , na.rm = T) # میانه
Mode <- mfv(data$battery )

# شاخص های پراکندگی
minimum <- min(data$battery , na.rm = T)#کم ترین

maximum <- max(data$battery , na.rm = T)# بیشترین

range <- range(data$battery , na.rm = T)# بازه

standardDeviation <- sd(data$battery , na.rm = T) # انحراف معیار

variance <- var(data$battery , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به ظرفیت انرژی باتری دستگاه بر حسب میلی آمپر ساعت", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای ظرفیت باتری

hist(data$battery , breaks=10, main="نمودار مستطیلی ظرفیت انرژی باتری دستگاه بر حسب میلی آمپر ساعت", ylab = "فراوانی", xlab = "میلی آمپر ساعت", col=c("red"), border = "black" )

# نمودار جعبه ای برای ظرفیت باتری

boxplot(data$battery ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به ظرفیت انرژی باتری دستگاه بر حسب میلی آمپر ساعت", xlab = "میلی آمپر ساعت", col=c("red"), border = "black")

###########################################################
#covariance <- cov(data$screen_size, data$battery)
# چون داده ی گمشده دارم با استفاده از فرمول به دست اوردیم

# کوواریانس و ضریب وابستگی بین سایز اسکرین و ظرفیت باتری

xy<- data$screen_size* data$battery

Averagexy <- mean(xy , na.rm = T) # امید ریاضی ایکس و وای که برابر میانگین است

Averagex <- mean(data$screen_size , na.rm = T)  # امید ریاضی ایکس که برابر میانگین است

Averagey <- mean(data$battery , na.rm = T) #  امید ریاضی وای که برابر میانگین است


covariance <- (Averagexy -(Averagex*Averagey)) # کواریانس سایز اسکرین و ظرفیت باتری

#  ضریب وابستگی
Corr <- (covariance / ((sd(data$screen_size , na.rm = T)) * sd(data$battery , na.rm = T))) 

# چاپ کردن مقدار کواریانس و ضریب وابستگی

cat("کوواریانس بین سایز اسکرین و ظرفیت باتری : ", covariance, "\nضریب وابستگی بین سایز اسکرین و ظرفیت باتری : ", Corr)
