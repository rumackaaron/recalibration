library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

entropy = function(x, nbins=100) {
  x = x[!is.na(x)]
  x = pmax(pmin(x,1-1/(2*nbins)),1/(2*nbins))
  y = hist(x, breaks=seq(0,1,length.out=nbins+1), plot=FALSE)
  p = nbins*y$counts/length(x)
  return(-sum(p*log(p),na.rm=T)/nbins)
}

quantiles = readRDS("../flusight-natreg-spline-run/swgtfc.forecast.quantiles.rds")
quantiles1 = readRDS("../flusight-natreg-spline-run/swgtf.calibrated.quantiles.rds")
arre = array(0,dim=c(7,27,4))
arre[,,1:3] = apply(quantiles,4:6,var,na.rm=T)
arre[,,4] = apply(quantiles1,4:5,var,na.rm=T)
dimnames(arre) = list(Target=dimnames(quantiles)[[4]],Forecaster=dimnames(quantiles)[[5]],Calibration=c(dimnames(quantiles)[[6]],"Ensemble"))
df = plyr::adply(arre,.margins=1:3)
df[["Calibration"]] = as.character(df[["Calibration"]])
df[["Calibration"]][df[["Calibration"]] == "np"] = "Nonparametric"
df[["Calibration"]][df[["Calibration"]] == "beta"] = "Parametric"
df[["Calibration"]][df[["Calibration"]] == "none"] = "None"
df[["Calibration"]] = factor(df[["Calibration"]],levels=c("Nonparametric","Parametric","Ensemble","None"))
df = as_tibble(df)
df = df %>%
  pivot_wider(names_from=Calibration,values_from=V1)

ggplot(df %>% filter(str_sub(Target,start=3) == "wk ahead")) +
  geom_segment(aes(y=None,yend=Ensemble,color=Target),x=0,xend=1,
    arrow = arrow(length=unit(0.01,"npc"))) +
  geom_abline(intercept=1/12,slope=0) +
  geom_vline(xintercept=0,alpha=0) +
  geom_vline(xintercept=1,alpha=0) +
  scale_x_continuous(breaks=c(0,1),labels=function(b) {c("Original","Recalibrated")[b+1] } ) +
  labs(color="Target",x="",y="PIT Variance") +
  theme(aspect.ratio=0.75)

ggsave("fig11.png",width=8,height=6)
