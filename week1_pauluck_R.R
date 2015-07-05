#######################################################
## Puneet Auluck - Sunday 7/5/15
#######################################################

#1 Write a loop that calculates 12-factorial.
# 12!=12X11X10X9X8X7X6X5X4X3X2X1
# The last multiplcation of 1 is not necessary
# To save a step, this will be skipped and looped from 12:2
y<-1
for (x in 12:2)
  {
  y<-y*x; 
  }
y


#2 Show how to create a numeric vector that contains the sequence 
#  from 20 to 50 by 5.
numVector<-5*c(4:10)
numVector

#3 Show how to take a trio of input numbers a, b and c and
#  implement the quadratic equation
quadEQ<-function(a,b,c){

 xvalues<-c(0,0)
 
 sqroot<-(b^2)-(4*a*c);
 base<-2*a
 sqrtAbs<-round(sqrt(abs(sqroot)),digit=2)
 topNeg<-(-b-sqrtAbs)
 topPos<-(-b+sqrtAbs)
 
 if (a==0) { 
   xvalues<-c(NA,NA)
   }
 else if (sqroot==0){
   xvalues<-c(-b/2*a,NA)
 }
 #handle imaginary numbers:
 else if (b==0) {
   x1<-paste(topNeg/base,"i", sep="")
   x2<-paste(topPos/base,"i", sep="")
  xvalues<-c(x1,x2)
 } 
 else if (sqroot<0){
   x1<-paste(-b/base,"-",sqrtAbs/base,"i ", sep="")
   x2<-paste(-b/base,"+",sqrtAbs/base,"i", sep="")
 
   xvalues<-c(x1, x2)
  
 }
 #the input values a, b and c are valid
 else {
   xvalues<-c(topNeg/base, topPos/base) 
 }
 names(xvalues)<-c("x1","x2")
 return(xvalues[c("x1","x2")])
}

#test for a=0
print(quadEQ(a=0, b=1, c=10))

#test for sqroot=0
print(quadEQ(a=1, b=2, c=1))

#test for b=0 (imaginary)
print(quadEQ(a=1, b=0, c=10))

#test for sqroot<0 (imaginary)
print(quadEQ(a=1, b=1, c=10))

#valid quadratic equation
print(quadEQ(a=2, b=50, c=5))