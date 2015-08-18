#this script is for plotting a time series of fecal pellets from different experimens, four different bacteria, and fitting a curve to it in the shape of FP_count=1/time

#data accepted comes in the shape of
#Mouse  Counts	Time	Bacteria	Exp
#2014	8.16E+11	5	HA127	1
#2013	7.17E+11	5	HA127	1
#2012	6.86E+11	5	HA127	1

FP<-read.table("9.03.15/fig_intestinal_transit/in_vivo_1-3.csv", header=T, sep=";")
fp<-na.omit(FP)
fp
require(ggplot2)

attach(fp)

custom<-c("red4","skyblue","blue","red") 
 

a<-ggplot() + 
  coord_cartesian() +
  scale_x_continuous(breaks=seq(0,270,20)) +
  scale_y_log10(breaks=c(1e+03,1e+04,1e+05,1e+06,1e+07,1e+08,1e+09,1e+10,1e+11,1e+12,1e+13)) +
  scale_colour_manual(values=custom)+
  theme_classic()+
  facet_wrap(~Facet+Bacteria) +
  layer(
    data=fp, 
    mapping=aes(x=Time, y=Counts, color=as.factor(Bacteria),shape=Bacteria),
    stat="identity", 
    stat_params=list(), 
    geom="point", 
    geom_params=list(), 
    position=position_jitter(width=0.4, height=0)
    
  )+
layer(
    data=fp,
    mapping=aes(x=Time,y=Counts),
    stat="smooth",
    stat_params=list(method="glm", formula=y~ I(1/x), size=1),
    geom="smooth",
    geom_params=list(color="black", alpha=0.3),
    position=position_identity()
  )+
  geom_hline(yintercept=200,color="black", linetype="dashed")
a
a+theme(legend.position="none")




