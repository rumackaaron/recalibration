library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

evals = readRDS("../flusight-natreg-training-run/swgtfcrn.forecast.evaluations.rds")
evals = apply(pmax(evals,-10),c(4,6,8),mean,na.rm=T)
evals = plyr::adply(evals,.margins=1:3)
evals[["Calibration"]] = as.character(evals[["Calibration"]])
evals[["Calibration"]][evals[["Calibration"]] == "none"] = "None"
evals[["Calibration"]][evals[["Calibration"]] == "np"] = "Nonparametric"
evals[["Calibration"]][evals[["Calibration"]] == "beta"] = "Parametric"
evals[["Num"]] = as.numeric(evals[["Num"]])

evals = pivot_wider(evals,names_from=Calibration,values_from=V1)
evals = evals %>%
  group_by(Num,Target) %>%
  summarize(Nonparametric=Nonparametric-None,Parametric=Parametric-None)

ggplot(evals) +
  geom_line(aes(x=Num,y=Nonparametric,color=Target,linetype="Nonparametric")) +
  geom_point(aes(x=Num,y=Nonparametric,color=Target,shape="Nonparametric")) +
  geom_line(aes(x=Num,y=Parametric,color=Target,linetype="Parametric")) +
  geom_point(aes(x=Num,y=Parametric,color=Target,shape="Parametric")) +
  labs(color="Target",linetype="Calibration",y="Improvement in Mean Log Score",x="Training Seasons") +
  guides(shape="none") +
  scale_x_continuous(breaks=c(1,2,3,4),labels=c(1,2,4,8)) +
  theme(aspect.ratio=0.75)

ggsave("fig10.png",width=8,height=6)
