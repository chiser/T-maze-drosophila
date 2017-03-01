

y= function (a,x,k){
  a*x^(-k)
}

xCoordinate<- 1:100

powerlaw = vector("numeric", length=100)

for (x in xCoordinate){
  
  powerlaw[x]<-y(2,x,2)
  
}

plot(powerlaw)
plot(log(powerlaw)~log(xCoordinate))

############## This is by making an iteration over the exponent k. You get a line just in a log-normal plot
### if a<0 it tends from negative to zero, if a<0 tends from positive to zero
### if x<0 it has a neg and positive power law and for x>0 only one (0 not possible). If x=+1/-1 isn´t a power law rather a constant. 
### For 0<x<1 the curve increase at the end (inverted). For increasing (when x>1) or decreasing (when 0<x<1) x, the power law curve is steeper
### k just changes the scale I think, but the shape stays so
###  power-law x^{-a} has a well-defined mean over x \in [1,\infty] only if a > 2 , and it has a finite variance only if a >3; 

############# By making an iteration over the value x is how you get the straight line in a log-log plot (real powerlaw)
## The higher k is the steepest the slope is





