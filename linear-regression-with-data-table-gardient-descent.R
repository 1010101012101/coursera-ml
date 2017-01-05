require(data.table)
setwd('c:\\TIM\\PERSO\\coursera-ml\\')
t = fread('ex1data1.txt')
setnames(t, 'V1', 'x')
setnames(t, 'V2', 'y')

# =================================
# linear regression using gradient descent
# =================================
# model is:
# yhat ~ y, with:
# 				yhat = theta0 + theta1 * x
# =================================

# cost function (RMSE)
rmse <- function(x,y)
{
	m = length(x)
	sum((y - x)^2)/(2 * m)
}

# https://www.coursera.org/learn/machine-learning/lecture/kCvQc/gradient-descent-for-linear-regression
gradientDescent <- function(data=t, iterations = 1000, alpha = 0.02)
# alpha = learning rate
{
	# initialization of the parameters we are looking for, before starting gradient descent
	theta0 = 0
	theta1 = 0

	m = nrow(t) # number of observations

	# now loop over iterations
	for(i in 1:iterations)
	{
		t[, yhat 		:= theta0 + theta1 * x]

		# print current cost to show progress
		if(i %% 100 == 0) print( rmse(t$yhat, t$y) )
		
		t[, gradient0 	:= (yhat - y) * 1 / m]
		t[, gradient1 	:= (yhat - y) * x / m]

		theta0 = theta0 - alpha * t[, sum(gradient0)]
		theta1 = theta1 - alpha * t[, sum(gradient1)]

	}
	return(c(theta0, theta1))
}


gradientDescent(data = t, iterations = 1000)
