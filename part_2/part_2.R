mydata = read.csv("C:\\Users\\montazeri\\Desktop\\amar\\used_device_data.csv")
#محاسبه بازه 95 درصدی برای میانگین
#محاسبه میانگین،تعداد اعضا و انحراف معیار
tmp = mydata$battery
baze_mian.mean = mean(tmp,na.rm = T)
baze_mian.num = length(tmp)
baze_mian.sd = sd(tmp,na.rm = T)
baze_mian.var = var(tmp,na.rm = T)
baze_mian.se = baze_mian.sd/sqrt(baze_mian.num)
baze_mian.se
alpha = 0.05
degrees.freedom = baze_mian.num - 1
#محاسبه بازه 95 درصدی میانگین جمعیت
#محاسبه بازه 95 درصدی با استفاده از توزیع نرمال
error = qnorm(1-(alpha/2))*baze_mian.se
lower.bound = baze_mian.mean - error
upper.bound = baze_mian.mean + error
#چاپ بازه 95 درصدی
print(c(lower.bound,upper.bound))
#T محاسبه بازه 95 درصدی با استفاده از توزیع
error.t = qt(1-(alpha/2),degrees.freedom)*baze_mian.se
lower.t.bound = baze_mian.mean - error.t
upper.t.bound = baze_mian.mean + error.t
#چاپ بازه 95 درصدی
print(c(lower.t.bound,upper.t.bound))
#محاسبه بازه 95 درصدی واریانس جمعیت 
#محاسبه بازه 95 درصدی با استفاده از توزیع مربع کای
error.chi.lower = degrees.freedom*baze_mian.var/qchisq(1-(alpha/2),degrees.freedom)
error.chi.upper = degrees.freedom*baze_mian.var/qchisq(alpha/2,degrees.freedom)
#چاپ بازه 95 درصدی
print(c(error.chi.lower,error.chi.upper))
#انجام آزمون آماری
komaki = mydata$weight
n = length(komaki)
avg = mean(komaki,na.rm = T)
sd = sd(komaki,na.rm = T)
alpha1 = 0.05
mu0 = avg + 10
zt = (avg-mu0)/(sd/sqrt(n))
zt1 = qt((1-alpha1),n-1,lower.tail = T)
zt
zt1
if (zt > zt1){
  cat("فرض صفرم رد می شود")
} else{
  cat("فرض صفرم رد نمی شود")
}
