M1<-matrix(1:20,4,5,byrow=FALSE)
M2<-matrix(1:20,4,5,byrow=TRUE)
M3<-M1+M2
M4<-M3[,c(1:2,4:5)]
M4

death <- read.csv("/home/eugeneliu/RProjects/a.csv",header = TRUE)
plot(death$Age,death$Male_death,
     main="Age ~ Male_death",
     xlab = "Age",
     ylab="death")
library(vcd)
mytable<-xtabs(~Age+Male_death,data=death)
chisq.test(mytable)

a_result<-c(2,4,3,2,4,7,7,2,5,4)
b_result<-c(5,6,8,5,10,7,12,6,6)
c_result<-c(7,11,6,6,7,9,5,10,6,3,10)
a_mean_result<-mean(a_result)
b_mean_result<-mean(b_result)
c_mean_result<-mean(c_result)
all_mean_result<-c(a_mean_result,b_mean_result,c_mean_result)
var(all_mean_result)


library(data.table)
set.seed(100L)
dt = data.table(x = sample(1e5L, 1e7L, TRUE), y = sample(1e5L),z=log(sample(1e7L)))
dt[x==7045]
dt[y==75130]



df<-read.csv("/home/eugeneliu/RProjects/b.csv",header = FALSE)
colnames(df)<-c("date","item_id","cate_id","cate_level_id","brand_id","supplier_id","pv_ipv","cart_uv","collect_uv","cart_ipv")
df
feature<-df[,c("date","item_id","cate_id","cart_ipv")]
myvars <- c("date","item_id","cate_id","cart_ipv")
aggregate(feature[myvars],by=list(cate_id=feature$cate_id,min))
