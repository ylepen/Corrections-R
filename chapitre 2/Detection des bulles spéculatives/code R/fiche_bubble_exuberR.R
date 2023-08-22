install.packages("exuber")
library(exuber)

library(MultipleBubbles)

install_exuberdata()

x<-sp_data

plot(x,type='l',col='blue')


radf_x <-radf(x)

radf_x$sadf

radf_x$sadf

mc_critical_values <- radf_mc_cv(n=length(x), seed = 145)

summary(radf_x,mc_critical_values)

library("ggplot2")

autoplot_x_gsadf<-autoplot(radf_x)+labs(title=NULL)

autoplot_x_sadf<-autoplot(radf_x,option="sadf",cv=mc_critical_values)+labs(title=NULL)

autoplot_x_sadf

datestamp(radf_x,option="sadf",cv=mc_critical_values)
