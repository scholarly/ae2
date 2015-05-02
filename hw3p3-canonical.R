# This is the cononical answer for homework 3 part 3.
# The reason my answers differed from these is that
# the imputation did not produce the same results for me.
# I should have just used loans_imputed.csv

baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"

getdata = function(local){
  if(!file.exists(local)){
    library(downloader)
    remote = paste0(baseurl,local)
    print(remote)
    download(remote,local)
  }
  read.csv(local)
}

loans = getdata("loans.csv")


# 1.1
mean(loans$not.fully.paid)

# 1.2
sapply(loans,function(x){sum(is.na(x))})

# 1.4
if(FALSE){
  library(mice)
  vars.for.imputation = setdiff(names(loans), "not.fully.paid")
  set.seed(144)
  imputed = complete(mice(loans[vars.for.imputation]))
  loans[vars.for.imputation] = imputed

  ## the imputation did not yield identical results
  #sapply(vars.for.imputation,function(x){sum(iloans[,x]!=loans[,x])})
}else{
  loans = getdata("loans_imputed.csv")
}



# 2.1
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid,SplitRatio=0.7)
train = subset(loans,spl)
test = subset(loans,!spl)

mod = glm(not.fully.paid~., data=train, family="binomial")

s = summary(mod)
s$coefficients[s$coefficients[,4]<0.05,]

# 2.2
xx = s$coefficients[["fico","Estimate"]]
oaob = xx*(700-710)
print(oaob)
print(exp(oaob))

# 2.3
test$predicted.risk = predict(mod, newdata=test, type="response")
conf = table(test$not.fully.paid, test$predicted.risk > 0.5)
print(conf)
sum(diag(conf))/nrow(test)

bl.pred = rep(mean(train$not.fully.paid)>=.5,nrow(test))
bl.conf = table(test$not.fully.paid,bl.pred)
bl.conf[1]/nrow(test)

# 2.4
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

# 3.1
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

# 3.2
pred.bivariate = predict(bivariate, newdata=test, type="response")
max(pred.bivariate)

# 3.3
as.numeric(performance(prediction(pred.bivariate, test$not.fully.paid), "auc")@y.values)

# 4.1
10*exp(0.06*3)


# 5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

max(test$profit)*10

# 6.1
highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)

mean(highInterest$not.fully.paid)


# 6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)
