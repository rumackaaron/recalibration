library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

evals = readRDS("../flusight-natreg-retro-run/swgtfc.forecast.evaluations.rds")[,,,4:7,,]
evals = apply(pmax(evals,-10),c(1,4,6),mean,na.rm=T)
evals = plyr::adply(evals,.margins=1:3)
evals[["Calibration"]] = as.character(evals[["Calibration"]])
evals[["Calibration"]][evals[["Calibration"]] == "none"] = "None"
evals[["Calibration"]][evals[["Calibration"]] == "np"] = "Nonparametric"
evals[["Calibration"]][evals[["Calibration"]] == "beta"] = "Parametric"

evals = pivot_wider(evals,names_from=Calibration,values_from=V1) %>%
  mutate(Season = as.integer(str_sub(Season,1,4)))
evals = evals %>%
  group_by(Season,Target) %>%
  summarize(Nonparametric=Nonparametric-None,Parametric=Parametric-None)

evals[["Nonparametric"]][evals[["Season"]] == 2010] = 0
evals[["Parametric"]][evals[["Season"]] == 2010] = 0

ggplot(evals) +
  geom_line(aes(x=Season,y=Nonparametric,color=Target,linetype="Nonparametric")) +
  geom_point(aes(x=Season,y=Nonparametric,color=Target,shape="Nonparametric")) +
  geom_line(aes(x=Season,y=Parametric,color=Target,linetype="Parametric")) +
  geom_point(aes(x=Season,y=Parametric,color=Target,shape="Parametric")) +
  labs(color="Target",linetype="Calibration",y="Improvement in Mean Log Score",x="Season") +
  guides(shape="none") +
  theme(aspect.ratio=0.75)

ggsave("fig9.png",width=8,height=6)
