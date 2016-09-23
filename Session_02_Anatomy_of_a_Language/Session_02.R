killcurve <-function (object, xx)
{
  object <- object[,-1];
  mu <- sapply(object, mean);
  sd <- sapply(object, sd);
  #browser()
  plot(xx,mu,xlab='Dose',ylab='',las=1);
  lines(xx,mu);
  #colnames(object)<- x
  #object <- subset(object, select = -c(E))
  #plot(names(p),p,xlab = "Dose", ylab= "Signal")
  #lines(names(p),p)
}
