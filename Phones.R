
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

tab1(data$five_g, sort.group = "decreasing", cum.percent = TRUE, main=" 5G  ", ylab = "فراوانی", col=c("red"), border = "black")

# نمودار میله ای برای دسترسی به فایو جی با درصد فراوانی

tab1(data$five_g, sort.group = "decreasing", cum.percent = TRUE, main=" 5G  ",bar.values = "percent", ylab = "فراوانی", col=c("red"), border = "black")

table5G<-table(data$five_g)

# نمودار دایره ای فراوانی دسترسی به فایو جی

pie(table5G)

### برای متغییر های پیوسته

## برای سایز اسکرین

# شاخص های مرکزگرا

summary(data$screen_size) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
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

summary(data$rear_camera_mp) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
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

summary(data$front_camera_mp) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
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

summary(data$internal_memory) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
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

summary(data$ram) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
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

summary(data$battery) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
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

## برای وزن دستگاه بر حسب گرم

# شاخص های مرکزگرا

summary(data$weight) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
Average <- mean(data$weight , na.rm = T) # میانگین
Median <- median(data$weight , na.rm = T) # میانه
Mode <- mfv(data$weight )

# شاخص های پراکندگی
minimum <- min(data$weight , na.rm = T)#کم ترین

maximum <- max(data$weight , na.rm = T)# بیشترین

range <- range(data$weight , na.rm = T)# بازه

standardDeviation <- sd(data$weight , na.rm = T) # انحراف معیار

variance <- var(data$weight , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به وزن دستگاه بر حسب گرم", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای وزن دستگاه بر حسب گرم

hist(data$weight , breaks=10, main="نمودار مستطیلی وزن دستگاه بر حسب گرم", ylab = "فراوانی", xlab = "گرم", col=c("red"), border = "black" )

# نمودار جعبه ای برای وزن دستگاه بر حسب گرم

boxplot(data$weight ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به وزن دستگاه بر حسب گرم", xlab = "گرم", col=c("red"), border = "black")

## برای سالی که مدل دستگاه عرضه شد

# شاخص های مرکزگرا

summary(data$release_year) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
Average <- mean(data$release_year , na.rm = T) # میانگین
Median <- median(data$release_year , na.rm = T) # میانه
Mode <- mfv(data$release_year )

# شاخص های پراکندگی
minimum <- min(data$release_year , na.rm = T)#کم ترین

maximum <- max(data$release_year , na.rm = T)# بیشترین

range <- range(data$release_year , na.rm = T)# بازه

standardDeviation <- sd(data$release_year , na.rm = T) # انحراف معیار

variance <- var(data$release_year , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به سالی که مدل دستگاه عرضه شد", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای سالی که مدل دستگاه عرضه شد

hist(data$release_year , breaks=10, main="نمودار مستطیلی سالی که مدل دستگاه عرضه شد", ylab = "فراوانی", xlab = "سال", col=c("red"), border = "black" )

# نمودار جعبه ای برای سالی که مدل دستگاه عرضه شد

boxplot(data$release_year ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به سالی که مدل دستگاه عرضه شد", xlab = "سال", col=c("red"), border = "black")

## برای تعداد روزهایی که دستگاه استفاده شده/بازسازی شده استفاده شده است

# شاخص های مرکزگرا

summary(data$days_used) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
Average <- mean(data$days_used , na.rm = T) # میانگین
Median <- median(data$days_used , na.rm = T) # میانه
Mode <- mfv(data$days_used )

# شاخص های پراکندگی
minimum <- min(data$days_used , na.rm = T)#کم ترین

maximum <- max(data$days_used , na.rm = T)# بیشترین

range <- range(data$days_used , na.rm = T)# بازه

standardDeviation <- sd(data$days_used , na.rm = T) # انحراف معیار

variance <- var(data$days_used , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به تعداد روزهایی که دستگاه استفاده شده/بازسازی شده استفاده شده است", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده برای تعداد روزهایی که دستگاه استفاده شده/بازسازی شده استفاده شده است

hist(data$days_used , breaks=10, main="نمودار مستطیلی تعداد روزهایی که دستگاه استفاده شده/بازسازی شده استفاده شده است", ylab = "فراوانی", xlab = "تعداد", col=c("red"), border = "black" )

# نمودار جعبه ای برای تعداد روزهایی که دستگاه استفاده شده/بازسازی شده استفاده شده است

boxplot(data$days_used ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به تعداد روزهایی که دستگاه استفاده شده/بازسازی شده استفاده شده است", xlab = "تعداد", col=c("red"), border = "black")

## قیمت عادی یک دستگاه جدید از همان مدل

# شاخص های مرکزگرا

summary(data$normalized_new_price) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
Average <- mean(data$normalized_new_price , na.rm = T) # میانگین
Median <- median(data$normalized_new_price , na.rm = T) # میانه
Mode <- mfv(data$normalized_new_price )

# شاخص های پراکندگی
minimum <- min(data$normalized_new_price , na.rm = T)#کم ترین

maximum <- max(data$normalized_new_price , na.rm = T)# بیشترین

range <- range(data$normalized_new_price , na.rm = T)# بازه

standardDeviation <- sd(data$normalized_new_price , na.rm = T) # انحراف معیار

variance <- var(data$normalized_new_price , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به قیمت عادی یک دستگاه جدید از همان مدل", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده قیمت عادی یک دستگاه جدید از همان مدل

hist(data$normalized_new_price , breaks=10, main="نمودار مستطیلی قیمت عادی یک دستگاه جدید از همان مدل", ylab = "فراوانی", xlab = "قیمت", col=c("red"), border = "black" )

# نمودار جعبه ای قیمت عادی یک دستگاه جدید از همان مدل

boxplot(data$normalized_new_price ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به قیمت عادی یک دستگاه جدید از همان مدل", xlab = "قیمت", col=c("red"), border = "black")

## قیمت عادی دستگاه استفاده شده/بازسازی شده

# شاخص های مرکزگرا

summary(data$normalized_used_price) #شاخص های مرکز گرا 


# شاخص های مرکزگرا به صورت دستی
Average <- mean(data$normalized_used_price  , na.rm = T) # میانگین
Median <- median(data$normalized_used_price  , na.rm = T) # میانه
Mode <- mfv(data$normalized_used_price  )

# شاخص های پراکندگی
minimum <- min(data$normalized_used_price  , na.rm = T)#کم ترین

maximum <- max(data$normalized_used_price  , na.rm = T)# بیشترین

range <- range(data$normalized_used_price  , na.rm = T)# بازه

standardDeviation <- sd(data$normalized_used_price  , na.rm = T) # انحراف معیار

variance <- var(data$normalized_used_price  , na.rm = T) # واریانس

# چاپ کردن شاخص های مرکزگرا و شاخص های پراکندگی

cat ("داده های مربوط به قیمت عادی دستگاه استفاده شده/بازسازی شده", "\nمیانگین : ", Average, "\nمیانه : ", Median, "\nنما : ", Mode, "\nمینیمم : ", minimum, "\nماکسیمم : ", maximum, "\nرنج : ", range, "\nواریانس : ", variance, "\nانحراف معیار : ", standardDeviation)

# نمودار مستطیلی با 10 رده قیمت عادی دستگاه استفاده شده/بازسازی شده

hist(data$normalized_used_price  , breaks=10, main="نمودار مستطیلی قیمت عادی دستگاه استفاده شده/بازسازی شده", ylab = "فراوانی", xlab = "قیمت", col=c("red"), border = "black" )

# نمودار جعبه ای قیمت عادی دستگاه استفاده شده/بازسازی شده

boxplot(data$normalized_used_price  ,horizontal = TRUE,  main="نمودار جعبه ای مربوط به قیمت عادی دستگاه استفاده شده/بازسازی شده", xlab = "قیمت", col=c("red"), border = "black")

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

cat("کوواریانس بین سایز اسکرین و ظرفیت باتری : ", covariance, "\nضریب همبستگی بین سایز اسکرین و ظرفیت باتری : ", Corr)
