salaries <- read.table("U:\\608\\Data\\ProfessorSalaries.txt", header=TRUE)


m1 <- lm (salaries$ThirdQuartile ~ salaries$Experience)
par(mfrow=c(2,2))
plot(m1)



m2 <- lm(salaries$ThirdQuartile ~ salaries$Experience, weight=(salaries$SampleSize))
par(mfrow=c(2,2))
plot(m2)

