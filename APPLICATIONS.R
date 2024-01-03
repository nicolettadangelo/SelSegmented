library(stopp)
??stopp

data("valenciacrimes")
data("greececatalog")
library(segmented)

str(valenciacrimes)
tab.expanse2 <- as.data.frame(table(valenciacrimes$week_day, valenciacrimes$crime_hour))
colnames(tab.expanse2) <- c("week_day", "crime_hour", "crimescount")
str(tab.expanse2)
tail(tab.expanse2)
plot(as.numeric(tab.expanse2$crime_hour), tab.expanse2$crimescount)

x <- as.numeric(tab.expanse2$crime_hour)
y <- tab.expanse2$crimescount
o <- glm(y ~ x, family = poisson)

summary(o)

o1<-try(segmented(o,~x,npsi=1,silent=TRUE,control=seg.control(n.boot=30,display=FALSE)))
o2<-try(segmented(o,~x,npsi=2,silent=TRUE,control=seg.control(n.boot=30,display=FALSE)))
o3<-try(segmented(o,~x,npsi=3,silent=TRUE,control=seg.control(n.boot=30,display=FALSE)))

n <- length(x)

c(AIC(o), AIC(o1), AIC(o2), AIC(o3))
c(BIC(o), BIC(o1), BIC(o2), BIC(o3))
c(AIC(o, k = log(n) * log(log(n))),
  AIC(o1, k = log(n) * log(log(n))),
  AIC(o2, k = log(n) * log(log(n))),
  AIC(o3, k = log(n) * log(log(n))))

which.min(c(AIC(o), AIC(o1), AIC(o2), AIC(o3)))#K = 3
which.min(c(BIC(o), BIC(o1), BIC(o2), BIC(o3)))#K = 3
which.min(c(AIC(o, k = log(n) * log(log(n))),
            AIC(o1, k = log(n) * log(log(n))),
            AIC(o2, k = log(n) * log(log(n))),
            AIC(o3, k = log(n) * log(log(n)))))#K = 3

#davies K = 2
0.05 / 3
davies.test(o)$p.value
davies.test(o1)$p.value
davies.test(o2)$p.value
davies.test(o3)$p.value

#score K = 2
pscore.test(o, n.break = 1)$p.value 
pscore.test(o1, n.break = 1, more.break = TRUE)$p.value
pscore.test(o2, n.break = 1, more.break = TRUE)$p.value
pscore.test(o3, n.break = 1, more.break = TRUE)$p.value

# pdf("criminini.pdf", height = 6, width = 10)
plot(x, y, xlab = " ", ylab = " ")
mtext(c("crime hour", "number of crimes"), side = c(1, 2), line = 2.5, cex = 1.5)
points(x, predict(o2, type = "response"), col = 2, type = "l", lwd = 2)
points(o2$psi[, 2], predict(o2, type = "response",
                            newdata = data.frame(x = o2$psi[, 2])),
       pch = 19, col = 2, cex = 1.5)
# dev.off()


summary(o2)
coef(o2)



###################################



land <- read.csv2("Land.csv", h = F, sep = ",")
str(land)
land$V2 <- as.numeric(land$V2)
plot(land$V1, land$V2, type = "l")



# land
x <- land$V1
y <- land$V2
o <- lm(y ~ x)


summary(o)
library(segmented)

o1<-try(segmented(o,~x,npsi=1,silent=TRUE,control=seg.control(n.boot=30,display=FALSE)))
o2<-try(segmented(o,~x,npsi=2,silent=TRUE,control=seg.control(n.boot=30,display=FALSE)))
o3<-try(segmented(o,~x,npsi=3,silent=TRUE,control=seg.control(n.boot=30,display=FALSE)))

n <- length(x)

c(AIC(o), AIC(o1), AIC(o2), AIC(o3))#3
c(BIC(o), BIC(o1), BIC(o2), BIC(o3))#2
c(AIC(o, k = log(n) * log(log(n))),
  AIC(o1, k = log(n) * log(log(n))),
  AIC(o2, k = log(n) * log(log(n))),
  AIC(o3, k = log(n) * log(log(n))))#1

which.min(c(AIC(o), AIC(o1), AIC(o2), AIC(o3)))#K = 3
which.min(c(BIC(o), BIC(o1), BIC(o2), BIC(o3)))#K = 3
which.min(c(AIC(o, k = log(n) * log(log(n))),
            AIC(o1, k = log(n) * log(log(n))),
            AIC(o2, k = log(n) * log(log(n))),
            AIC(o3, k = log(n) * log(log(n)))))#K = 3

#davies K = 1
0.05 / 3
davies.test(o)$p.value
davies.test(o1)$p.value
davies.test(o2)$p.value
davies.test(o3)$p.value

#score K = 1
pscore.test(o, n.break = 1)$p.value 
pscore.test(o1, n.break = 1, more.break = TRUE)$p.value
pscore.test(o2, n.break = 1, more.break = TRUE)$p.value
pscore.test(o3, n.break = 1, more.break = TRUE)$p.value

# pdf("land.pdf", height = 6, width = 10)
plot(x, y, xlab = " ", ylab = " ", type = "l")
mtext(c("Year", "Temperature anomalies"), side = c(1, 2), line = 2.5, cex = 1.5)
points(x, predict(o1, type = "response"), col = 2, type = "l", lwd = 2)
points(o1$psi[, 2], predict(o1, type = "response",
                            newdata = data.frame(x = o1$psi[, 2])),
       pch = 19, col = 2, cex = 1.5)
# dev.off()
# o1$psi[, 2] #1979
summary(o1)
o1$psi


