###################################################
########### PLOTS FUNCTION
plots.lm<-function(x, bins=NULL, binwidth=NULL, lags=1, scatterplots=F){
  require(ggplot2)
  require(cowplot)
  require(dplyr)
  require(gridExtra)
  
  #QQ PLOT
  res<-as.data.frame(residuals(x))
  colnames(res)<-"resid"
  p <- ggplot(res, aes(sample = resid)) + stat_qq() + stat_qq_line() + labs(title="A q-q plot for residuals")
  
  
  #HISTOGRAM
  if(is.null(bins) && is.null(binwidth)){bins=floor(sqrt(nrow(res)))}
  h<-qplot(res$resid, geom="histogram", main = "Histogram for residuals", xlab="Residuals", ylab="Count", bins=bins, binwidth=binwidth)
  
  
  ##AUTOCORELATION
  #Function for lagging the residuals
  multilag <- function(df, n) {
    varname <- sprintf("resid_lag%i",n)
    mutate(df, !!varname := lag(resid,n))
  }
  
  #autocorrelation plot function
   plot_data_func<-function(data, lag_col){
    ggplot(res, aes(x=resid,y=res[,lag_col])) + geom_point()+ labs(title=sprintf("Resid autocorr - lag %i",l), x = "i residual", y = sprintf("i-%i residual",l))
  }
  
  myplots<-list(p,h)
  for (l in 1:lags){
    res<-res %>% multilag(l)
    varname<-sprintf("resid_lag%i",l)
    myplots[l+2]<- lapply(varname, plot_data_func, data = res)
  }
  

  #SCATTERPLOT
  if (scatterplots==T){
    plots<-length(myplots)
    
    var_res<-cbind(x$model,resid=x$residuals)
    
    #scatterplot function
    scatter_plot<-function(data, var_col, resid=resid){
      ggplot(data,aes(x=data[,var_col], y=resid))+geom_point()+labs(title=sprintf("Scatterplot - %s ~ residual",colnames(data)[var_col]))
      }
  
  
    for (k in 1:(ncol(var_res)-1)){
      myplots[plots+k]<-lapply(k, scatter_plot, data=var_res)
      }
  }
  
  
  
  #COMBINED PLOT
  n <- length(myplots)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(myplots, ncol=nCol))
  
  
}


#################################################
########### EXAMPLE

library(datasets)
data("iris")
colnames(iris) <- c("sl", "sw", "pl", "pw", "s")
iris$s <- as.numeric(iris$s)
model1 <- lm(s ~ sl + sw + pl + pw, data = iris)
class(model1)


plots.lm(model1)
plots.lm(model1,lags = 5, scatterplots = T)




