library(readxl)
library(ggplot2)

#### spirotetramat ####

# load data
data = read_excel("0031 R input.xlsx") # load in experimental data


# preprocessing of data
data$logit = log10(data$frequency/(1-data$frequency)) # this step performs the logit function on the experimental data
control = data[data$condition=="control",]
trt20 = data[data$condition=="moderate",]
trt24 = data[data$condition=="high",]


# control - spirotetramat
#for the control, we only fit selection coefficient to generation 2 onwards (due to an unexplained delay in the first 2 generations).
control = control[control$generation>1,] 

control.lm = lm(logit~generation,data=control)
coef = control.lm$coefficients

selection_coef = (10^coef[2])-1 # predicted s for control gen 2-7 = 1.28
# wR/wS = s + 1 , wS = 1
wR = (selection_coef+1)*1 # wR = 2.28

p = ggplot(data=control,aes(x=generation,y=logit))+
  geom_point(size=2)+
  geom_abline(intercept=coef[1],slope=coef[2],color="red",lty=2,linewidth=1)+
  annotate("text", x=3, y=0.5, label= "s = 1.28",size=6)+
  annotate("text", x=3, y = 0.3, label="wR = 2.28", size=6)+
  theme_classic()+
  labs(x="Generation",y="Logit resistance frequency")
print(p)

ggsave("0031_control.png", p, width =7.5, height=5, dpi=300)


# high dose - spirotetramat (24 ug/ml)
trt24 = trt24[trt24$frequency!=1,] #remove values which leads to infinities when logged
high.lm = lm(logit~generation,data=trt24)
coef = high.lm$coefficients

selection_coef = (10^coef[2])-1 # predicted s for high dose = 2.9
# wR/wS = s + 1 , wS = 1
wR = (selection_coef+1)*1

p = ggplot(data=trt24,aes(x=generation,y=logit))+
  geom_point(size=2)+
  geom_abline(intercept=coef[1],slope=coef[2],color="red",lty=2,linewidth=1)+
  annotate("text", x=1.5, y=1.9, label= "s = 2.90",size=6)+
  annotate("text", x=1.5, y = 1.5, label="wR = 3.90", size=6)+
  theme_classic()+
  labs(x="Generation",y="Logit resistance frequency")
print(p)

ggsave("0031_high_dose.png", p, width =7.5, height=5, dpi=300)


# moderate dose - spirotetramat
trt20 = trt20[trt20$frequency!=1,] #remove values which leads to infinities when logged
moderate.lm = lm(logit~generation,data=trt20)
coef = moderate.lm$coefficients

selection_coef = (10^coef[2])-1 # predicted s for moderate dose = 1.97
# wR/wS = s + 1 , wS = 1
wR = (selection_coef+1)*1

p = ggplot(data=trt20,aes(x=generation,y=logit))+
  geom_point(size=2)+
  geom_abline(intercept=coef[1],slope=coef[2],color="red",lty=2,linewidth=1)+
  annotate("text", x=1.5, y=1.9, label= "s = 1.97",size=6)+
  annotate("text", x=1.5, y = 1.5, label="wR = 2.97", size=6)+
  theme_classic()+
  labs(x="Generation",y="Logit resistance frequency")
print(p)

ggsave("0031_moderate_dose.png", p, width =7.5, height=5, dpi=300)

#### ivermectin ####

# load data
data = read_excel("0032 R input.xlsx")


# preprocessing of data
data$logit = log10(data$frequency/(1-data$frequency))
control = data[data$condition=="control",]
trt = data[data$condition=="treatment",]


# control - ivermectin
control = control[control$frequency!=0,] #remove values which leads to negative infinities when logged
control.lm = lm(logit~generation,data=control)
coef = control.lm$coefficients

selection_coef = (10^coef[2])-1 # predicted s for control = -0.66
# wR/wS = s + 1 , wS = 1
wR = (selection_coef+1)*1 # wR = 0.34

p = ggplot(data=control,aes(x=generation,y=logit))+
  geom_point(size=2)+
  geom_abline(intercept=coef[1],slope=coef[2],color="red",lty=2,linewidth=1)+
  annotate("text", x=3, y=-0.1, label= "s = -0.66",size=6)+
  annotate("text", x=3, y = -0.3, label="wR = 0.34", size=6)+
  theme_classic()+
  labs(x="Generation",y="Logit resistance frequency")
print(p)

ggsave("0032_control.png", p, width =7.5, height=5, dpi=300)

# treatment - ivermectin
trt = trt[trt$frequency!=1,] 
trt.lm = lm(logit~generation,data=trt)
coef = trt.lm$coefficients

selection_coef = (10^coef[2])-1 # predicted s for treatment = 2.25
# wR/wS = s + 1 , wS = 1
wR = (selection_coef+1)*1 # wR = 3.25

p = ggplot(data=trt,aes(x=generation,y=logit))+
  geom_point(size=2)+
  geom_abline(intercept=coef[1],slope=coef[2],color="red",lty=2,linewidth=1)+
  annotate("text", x=0.7, y=2.1, label= "s = 2.25",size=6)+
  annotate("text", x=0.7, y = 1.9, label="wR = 3.25", size=6)+
  theme_classic()+
  labs(x="Generation",y="Logit resistance frequency")
print(p)

ggsave("0032_treatment.png", p, width =7.5, height=5, dpi=300)

