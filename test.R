
curve(dchisq(x,2),0,10)
abline(v=qchisq(0.95,2))
curve(dchisq(x,10),add=T,col="green",lwd=2)
1-pchisq(qchisq(0.95,2),10)
pchisq(qchisq(0.95,2),2)


curve(dnorm(x,0,15),-100,100)
curve(dnorm(x,10,15),add=T,col="green",lwd=2)
abline(v=qnorm(c(0.025,0.975),0,15))
pnorm(qnorm(0.975,0,15),10,15)-pnorm(qnorm(0.025,0,15),10,15)
1-(pnorm(qnorm(0.975,0,15),10,15)-pnorm(qnorm(0.025,0,15),10,15))
