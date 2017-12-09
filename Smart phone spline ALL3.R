

##########################################
########## Read in and plot data 
##########################################

# Read in data
progdata = read.table('F:\\Teaching\\ZUFE\\Data Science-rev2\\Towhidul lectures\\sp_all.txt')
names(progdata) = c("all","cno","econ","geo","year","sp")
head(progdata)

#install.packages(pkgs='ggplot2',dependencies=TRUE)

# plot the entire data
library(ggplot2)
ggplot(data=progdata, aes(x=year,y=sp, group=cno)) +
	geom_point() +
	geom_line() + 
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18)) 




##########################################
########## Polynomials 
##########################################

# Extract first cycle data only for illustration
data1 = progdata[progdata$cno==36,]
attach(data1)	

# Fitting polynomials to the first cycle data
# Dependent variable: logp (log progesterone level)
# Predictor variable: day (day since ovulation)

degree = 3 # degree of polynomial (highest power)
fit = lm(sp~poly(year,degree),data=data1) # fitted model
coef(fit) # coefficients

# Evaluate the fitted curve for plotting
time = seq(1,9,by=1) # time points to evaluate the fitted model
newdata = data.frame(year=time,fity=predict(fit,newdata=data.frame(year=time))) # time points and polynomial evaluated at the time points

# Plot data and estimated polynomial
ggplot(data=data1,aes(x=year,y=sp)) +
	geom_point(size=3) + 
	geom_line(size=1) +
	geom_line(data=newdata,aes(x=year,y=fity),color="red",size=1) + 
	ggtitle(paste('Polynomial of degree ',degree)) +
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))




##########################################
########## Piecewise Polynomials
##########################################

#install.packages("fda",dependencies=TRUE)
library(fda) # I used fda package to fit piecewise polynomials, which might not be the simplest way to do...

# Create polynomial basis object and fit piecewise polynomial
degree = 2 ; # degree of polynomial
myrange = c(1,9) # range of the predictor variable
myknots0 = c(1,3,6,9) # knot locations, the first element should be equal to the first element in myrange and the last element should be equal to the second element in myrange.

# The following two lines are required to put more knots to obtain piecewise (discontinuous) polynomials
myknotsm = myknots0[2:(length(myknots0)-1)] 
myknots = c(myknots0[1],sort(rep(myknotsm,degree+1)),myknots0[length(myknots0)]) # knots to be used in the analysis

mybasisobj = create.bspline.basis(range=myrange,norder=degree+1,breaks=myknots) # create b-spline basis functions to fit piecewise polynomials
myfdobj = smooth.basis(argvals=data1$year,y=data1$sp,mybasisobj) # fitted piecewise polynomials

coef(myfdobj)

# Evaluate the fitted curve for plotting
time = seq(myrange[1],myrange[2],by=1) # time points to evaluate the fitted model
newdata = data.frame(year=time,fity=eval.fd(evalarg=time,fdobj=myfdobj$fd)) # time points and polynomial evaluated at the time points

# Plot data and fitted piecewise polynomial
ggplot(data=data1,aes(x=year,y=sp)) +
	geom_point(size=3) +
	geom_line(size=1) +
	geom_line(data=newdata,aes(x=time,y=fity),color="red",size=1) + 
	geom_vline(xintercept = myknots,size=1,color="white") +
	geom_vline(xintercept = myknots,size=1,color="black",linetype="dashed") +
	ggtitle(paste('Piecewise Polynomial of degree ',degree)) +
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))




###############################################
########## Continuous Piecewise Polynomials
###############################################

library(fda) # I used fda package to fit piecewise polynomials, which might not be the simplest way to do...

# Create polynomial basis object and fit piecewise polynomial
degree = 2; # degree of polynomial
myrange = c(1,9) # range of the predictor variable
myknots0 = c(1,3,6,9) # knot locations, the first element should be equal to the first element in myrange and the last element should be equal to the second element in myrange.

# The following two lines are required to put more knots to obtain continuous piecewise polynomials
myknotsm = myknots0[2:(length(myknots0)-1)] 
myknots = c(myknots0[1],sort(rep(myknotsm,degree)),myknots0[length(myknots0)]) # knots to be used in the analysis

mybasisobj = create.bspline.basis(range=myrange,norder=degree+1,breaks=myknots) # create b-spline basis functions to fit piecewise polynomials
myfdobj = smooth.basis(argvals=data1$year,y=data1$sp,mybasisobj) # fitted piecewise polynomials
coef(myfdobj)

# Evaluate the fitted curve for plotting
time = seq(myrange[1],myrange[2],by=1) # time points to evaluate the fitted model
newdata = data.frame(year=time,fity=eval.fd(evalarg=time,fdobj=myfdobj$fd)) # time points and polynomial evaluated at the time points

# Plot data and fitted continuous piecewise polynomial
ggplot(data=data1,aes(x=year,y=sp)) +
	geom_point(size=3) +
	geom_line(size=1) +
	geom_line(data=newdata,aes(x=time,y=fity),color="red",size=1) + 
	geom_vline(xintercept = myknots,size=1,color="white") +
	geom_vline(xintercept = myknots,size=1,color="black",linetype="dashed") +
	ggtitle(paste('Piecewise Polynomial of degree ',degree)) +
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))



###############################################
########## Splines
###############################################

library(fda)

# Create spline basis object
degree = 3 # degree of polynomial
myrange = c(1,9) # range of the predictor variable
myknots = c(1,3,6,9) # knot locations, the first element should be equal to the first element in myrange and the last element should be equal to the second element in myrange.

mybasisobj = create.bspline.basis(range=myrange,norder=degree+1,breaks=myknots) # create b-spline basis functions to fit splines
myfdobj = smooth.basis(argvals=data1$year,y=data1$sp,mybasisobj) # fitted spline
coef(myfdobj)

# Evaluate the fitted curve
time = seq(myrange[1],myrange[2],by=1) # time points to evaluate the fitted model
newdata = data.frame(year=time,fity=eval.fd(evalarg=time,fdobj=myfdobj$fd)) # time points and polynomial evaluated at the time points

# Plot data and fitted spline
ggplot(data=data1,aes(x=year,y=sp)) +
	geom_point(size=3) +
	geom_line(size=1) +
	geom_line(data=newdata,aes(year=time,y=fity),color="red",size=1) + 
	geom_vline(xintercept = myknots,size=1,color="white") +
	geom_vline(xintercept = myknots,size=1,color="black",linetype="dashed") +
	ggtitle(paste('Spline of degree ',degree)) +
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))



###############################################
#### Smoothing splines with cross-validation
###############################################

# Create train and test data sets
traindata = progdata[progdata$cno==36,] # the first cycle data
testdata = progdata[progdata$cno==88,] # the second cycle data
attach(traindata) 

# Create spline basis object
mydegree = 3 # degree of polynomial
myrange = c(1,9)  # range of the predictor variable
myknots = unique(traindata$year) # knots are located at unique predictor variable values
mybasisobj = create.bspline.basis(range=myrange,norder=mydegree+1,breaks=myknots) # create b-spline basis functions to fit smoothing splines

lambdas = 10^seq(-2,3,by=0.5) # a grid of penalty parameter (lambda) to be tested
perr = c() # predictor error

# Cross-valiation starts
for (i in 1:length(lambdas)){

	# Create functional parameter object
	mylambda = lambdas[i] 
	myfdparobj = fdPar(fdobj=mybasisobj,Lfdobj=2,lambda=mylambda) 
	# fdobj: specify the basis function object
	# Lfobj: specify the penalty term, 2 means you will use the (integrated squared) second derivative as the penalty term
	# lambda: specify the penalty parameter value to use

	# Fit smoothing spline 
	myfdobj = smooth.basis(argvals=year,y=sp,fdParobj=myfdparobj,fdnames=list("year","cno","sp"))

	# Evaluate smoothing spline at the time points where the test data were obtained 
	predy = eval.fd(evalarg=testdata$year,fdobj=myfdobj$fd)

	# Calculate prediction error
	perr[i] = sum((testdata$sp - predy)^2) # sum of squared differece between observed and predicted values in the test data

} # cross-validation ends

# Plot cross-validation result
crossval = data.frame(log10lambda=log10(lambdas),perr=perr)
ggplot(data=crossval, aes(x=log10lambda,y=perr)) +
	geom_point(size=3) + 
	geom_line(size=1) +
	ggtitle('Prediction Error') + 
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))




###############################################
#### Smoothing splines with GCV
###############################################

# Create train and test data sets
traindata = progdata[progdata$cno==36,] # train country
testdata = progdata[progdata$cno==88,] # test country
attach(traindata) 

# Create spline basis object
mydegree = 3 # degree of polynomial
myrange = c(1,9)  # range of the predictor variable
myknots = unique(traindata$year) # knots are located at unique predictor variable values
mybasisobj = create.bspline.basis(range=myrange,norder=mydegree+1,breaks=myknots) # create b-spline basis functions to fit smoothing splines

lambdas = 10^seq(-2,3,by=0.5) # a grid of penalty parameter (lambda) to be tested
gcv = c() # GCV value

# Cross-valiation starts
for (i in 1:length(lambdas)){
	
	# Create functional parameter object
	mylambda = lambdas[i]
	myfdparobj = fdPar(fdobj=mybasisobj,Lfdobj=2,lambda=mylambda)
	# fdobj: specify the basis function object
	# Lfobj: specify the penalty term, 2 means you will use the (integrated squared) second derivative as the penalty term
	# lambda: specify the penalty parameter value to use

	# Fit smoothing spline
	myfdobj = smooth.basis(argvals=year,y=sp,fdParobj=myfdparobj,fdnames=list("year","cno","sp"))

	# Save GCV value
	gcv[i] = myfdobj$gcv

} # cross-validation ends

# Plot GCV result
crossval = data.frame(log10lambda=log10(lambdas),perr=gcv)
ggplot(data=crossval, aes(x=log10lambda,y=perr)) +
	geom_point(size=3) + 
	geom_line(size=1) +
	ggtitle('GCV measure') +
	theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))



#################################################################################
#### Fitting smoothing splines to Smartphone data using GCV
#################################################################################
nonconceptive = progdata[progdata$econ ==4,]
conceptive = progdata[progdata$econ == 1,]
mydata = nonconceptive # if you want to analyze conceptive data, change this to mydata=conceptive
groupname = 'Nonconceptive Cycles' 
attach(mydata)

# Create spline basis object
mydegree = 2 # degree of polynomial
myrange = c(1,9) # range of the predictor variable
myknots = unique(mydata$year) # knots are located at unique predictor variable values
mybasisobj = create.bspline.basis(range=myrange,norder=mydegree+1,breaks=myknots) # create b-spline basis functions to fit smoothing splines

lambdas = 10^seq(-2,3,by=0.5) # a grid of penalty parameter (lambda) to be tested
gcv = c() # GCV value

# cross-validation starts
for (i in 1:length(lambdas)){
  
  # Create functional parameter object
  mylambda = lambdas[i]
  myfdparobj = fdPar(fdobj=mybasisobj,Lfdobj=2,lambda=mylambda)
  
  # Fit smoothing spline 
  myfdobj = smooth.basis(argvals=year,y=sp,fdParobj=myfdparobj,fdnames=list("year","cno","logp"))
  
  # Save GCV
  gcv[i] = myfdobj$gcv
} # cross-validation ends

mylambda = lambdas[gcv==min(gcv)] # optimal value of lambda
myfdparobj = fdPar(fdobj=mybasisobj,Lfdobj=2,lambda=mylambda) # create fdparobj again using the optimal lambda value
myfdobj = smooth.basis(argvals=day,y=logp,fdParobj=myfdparobj,fdnames=list("year","cno","sp")) # fit smoothing spline again under the optimal lambda value
coef(myfdobj)

# Evaluate the fitted curve
time = seq(myrange[1],myrange[2],by=1) # time points to evaluate the fitted model
newdata = data.frame(year=time,fity=eval.fd(evalarg=time,fdobj=myfdobj$fd)) # time points and polynomial evaluated at the time points

# Plot data and fitted smoothing spline
ggplot(data=mydata,aes(x=year,y=sp)) +
  geom_point(size=3) +
  geom_line(size=1,aes(group=cno)) +
  geom_line(data=newdata,aes(year=time,y=fity), color="red",size=2) +
  ggtitle(groupname) +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18),title=element_text(size=18))

