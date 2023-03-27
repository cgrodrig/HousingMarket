normalqqplot <- function(x){
  alpha <- 0.05   # set alpha here
  n <- length(x)
  pplot <- (1:n)/(n + 1)
  plow <- qbeta(alpha/2,(1:n),n-(1:n)+1)
  pupp <- qbeta(1-alpha/2,(1:n),n-(1:n)+1)
  qqnorm <- qnorm(pplot)
  qx_low <- qnorm(plow)
  qx_upp <- qnorm(pupp)
  e = (x - mean(x))/sd(x)
  e_sort <- sort(e)
  index_e = order(e)
  figout <- ggplot(data=NULL,aes(x=qqnorm, y=e_sort)) + geom_point() 
  figout <- figout + geom_line(y = qx_low) + geom_line(y = qx_upp)+geom_line(y = qqnorm) 
  figout <- figout + xlab("Standard normal quantiles")+ylab("Standardized residuals")
  figout  <- figout  + geom_text(aes(label = ifelse(e_sort > qx_upp, index_e, "")), hjust = -0.5, vjust=0.4)
  figout  <- figout  + geom_text(aes(label = ifelse(e_sort < qx_low, index_e, "")), hjust = -0.5, vjust=0.4)
  figout
  return(figout)
}

tqqplot <- function(x,df){
  alpha <- 0.05  # set alpha here
  n <- length(x)
  pplot <- (1:n)/(n + 1)
  plow <- qbeta(alpha/2,(1:n),n-(1:n)+1)
  pupp <- qbeta(1-alpha/2,(1:n),n-(1:n)+1)
  qqt <- qt(pplot,df)
  qqt_low <- qt(plow,df)
  qqt_upp <- qt(pupp,df)
  e_sort <- sort(x)
  index_e = order(x)
  figout <- ggplot(data=NULL,aes(x=qqt, y=e_sort)) + geom_point() 
  figout <- figout + geom_line(y = qqt_low) + geom_line(y = qqt_upp)+geom_line(y = qqt) 
  figout <- figout + xlab("Standard t quantiles")+ylab("Studentized residuals")
  figout  <- figout  + geom_text(aes(label = ifelse(e_sort > qqt_upp, index_e, "")), hjust = -0.5, vjust=0.4)
  figout  <- figout  + geom_text(aes(label = ifelse(e_sort < qqt_low, index_e, "")), hjust = -0.5, vjust=0.4)
  figout
  return(figout)
}


