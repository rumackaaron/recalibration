library(ggplot2)
library(stringr)

window_evals = readRDS("../flusight-natreg-windows-run/swgtfvc.forecast.evaluations.rds")[,,,4:7,,,]
window_evals = apply(pmax(window_evals,-10),c(4,6,7),mean,na.rm=T)
window_evals = plyr::adply(window_evals,.margins=1:3)
window_evals[["Window"]] =
  as.integer(str_sub(window_evals[["Window"]],start=3))
window_evals[["Calibration"]] = as.character(window_evals[["Calibration"]])
window_evals[["Calibration"]][window_evals[["Calibration"]] == "none"] = "None"
window_evals[["Calibration"]][window_evals[["Calibration"]] == "np"] = "Nonparametric"
window_evals[["Calibration"]][window_evals[["Calibration"]] == "beta"] = "Parametric"


ggplot(window_evals) +
  geom_line(aes(x=Window,y=V1,color=Target,linetype=Calibration)) +
  geom_point(aes(x=Window,y=V1,color=Target,shape=Calibration)) +
  labs(color="Target",linetype="Calibration",y="Mean Log Score",x="Window Size") +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  theme(aspect.ratio=0.75)

ggsave("fig3.png",width=8,height=6)
