###
###  Time Series (geom_line)
###
###-----------------------------------

library(ggplot2)


#--- if you have ts object


data(LakeHuron)

is.ts(LakeHuron)

Y <- LakeHuron
  Y.df <- data.frame(t=time(Y), series=as.matrix(Y))
  ggplot( data=Y.df, aes(x=t, y=series)) + geom_line()

  ggplot( data=Y.df, aes(x=t, y=series)) + geom_line() + geom_point()


  ggplot( data=H, aes(x=t, y=Y)) + geom_line(size=2", linetype="dashed")
    #  linetype="dashed",   "dotted",  "dotdash", 
    #           "longdash", "twodash", etc. 

Y <- diff(LakeHuron)
  Y.df <- data.frame(t=time(Y), series=as.matrix(Y))
  ggplot( data=Y.df, aes(x=t, y=series)) + geom_line()



# type="h" plot
Y <- diff(LakeHuron)
  Y.df <- data.frame(t=time(Y), series=as.matrix(Y))
  ggplot( data=Y.df, aes(x=t, xend=t, y=0, yend=series)) + geom_segment(size=2)






#--- if you have xts object


library(quantmod)

getSymbols(c("SPY", "QQQ"))

Y <- Ad(SPY)
  colnames(Y) <- "series"
  Y.df <- data.frame(date=as.Date(index(Y), format="%Y-%m-%d"), 
                        series=as.matrix(Y))  
  ggplot( data=Y.df, aes(x=date, y=series)) + geom_line(size=1)


Y <- Ad(QQQ)
  colnames(Y) <- "series"
  Y.df <- data.frame(date=as.Date(index(Y), format="%Y-%m-%d"), 
                        series=as.matrix(Y))  
  ggplot( data=Y.df, aes(x=date, y=series)) + geom_line(size=1)



# plotting two xts together
Y1 <- log(Ad(SPY));  colnames(Y1) <- "Y1"
Y2 <- log(Ad(QQQ));  colnames(Y2) <- "Y2"
  Y.df <- data.frame(date=as.Date(index(Y), format="%Y-%m-%d"), 
                     Y1=as.matrix(cbind(Y1,Y2)) )  
  ggplot( data=Y.df, ) + 
       geom_line(aes(x=date, y=Y1-log(2)), size=1, color="red")  + 
       geom_line(aes(x=date, y=Y2       ), size=1, color="blue") + 
       geom_hline(yintercept=215) + 
       geom_vline(xintercept=as.Date("2015-01-02", format="%Y-%m-%d")) +
       coord_cartesian(ylim=c(log(80),log(300)), 
                       xlim=c(as.Date("2015-01-01", format="%Y-%m-%d"),
                              as.Date("2019-12-01", format="%Y-%m-%d"))) 
       
























