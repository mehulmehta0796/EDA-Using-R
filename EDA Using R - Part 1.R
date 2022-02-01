library (faraway)
pima <- read.table("pima.data", header=T) # read the data into R
head(pima)  # take a look
tail(pima)
summary(pima)  # some numerical summaries 
sort(pima$glucose)
sort(pima$diastolic)
pima$diastolic[pima$diastolic == 0] <- NA # set zero values in the variable blood to "NA", where "==" means "equal" in R
pima$glucose[pima$glucose == 0] <- NA 	# set zero values in the variable glucose to "NA"
pima$triceps[pima$triceps == 0] <- NA 	# set zero values in the variable triceps to "NA"
pima$insulin[pima$insulin == 0] <- NA 	# set zero values in the variable insulin to "NA"
pima$bmi[pima$bmi == 0] <- NA 		# set zero values in the variable bmi to "NA"
pima$test <- factor(pima$test) 		# assign the variable test as a factor in R
levels(pima$test) 			# check how variable test is coded now
levels(pima$test) <- c("negative", "positive") 	# assign descriptive labels to variable test
levels(pima$test) 				# check how variable test is coded now

summary(pima$test) 			# take a look
hist(pima$diastolic, col=rainbow(10)) 		# draw histogram of variable diastolic blood
plot(density(pima$diastolic, na.rm=TRUE))

newpima<-data.frame(pima[,-9])# extracting only numerical variables
head(newpima)
summary(newpima)

par(mfrow=c(2,4))
for(i in 1:ncol(newpima)){
  boxplot(newpima[,i],main=names(newpima)[i])
  abline(h=apply(newpima,2,mean,na.rm=TRUE)[i],col="red")
}
par(mfrow=c(2,2))
plot(glucose ~ test, pima)  # it draws a side-by-side box plot because the variable test is a qualitative variable
plot(diastolic ~ test, pima)  # it draws a side-by-side box plot because the variable test is a qualitative variable
plot(triceps ~ test, pima)  # it draws a side-by-side box plot because the variable test is a qualitative variable
plot(insulin ~ test, pima)  # it draws a side-by-side box plot because the variable test is a qualitative variable


par(mfrow=c(2,4))
for(i in 1:ncol(newpima)){
  plot(newpima[,i]~pima$test,main=names(newpima)[i])
}

par(mfrow=c(2,4))
for(i in 1:ncol(newpima)){
  hist(newpima[,i],main=names(newpima)[i],prob=T)
  lines(density(pima[,i], na.rm=TRUE))
}

findoutlier = function(x){
  row.names(pima)[x>quantile(x, probs=0.75, na.rm=TRUE) + 1.5*IQR(x) | x<quantile(x, probs=0.25, na.rm=TRUE) - 1.5*IQR(x)]
}

findoutlier(pima$pregnant)

par(mfrow=c(1,1))
boxplot(pima$pregnant)