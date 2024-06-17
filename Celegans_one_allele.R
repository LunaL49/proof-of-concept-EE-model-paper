library(viridis)
library(reshape2)

# Model setup
#  Constant population size
#  Stochastic genetic evolution 
#  Resistance allele present from start at set frequencies
#  C. elegans genetics with selfing hermaphrodites

# Key assumptions 
#  Non-overlapping generations 
#  Homogeneous population 
#  No de novo mutation

#### C. elegans resistance model, homozygote only, selfing reproduction ####

# MANUAL CHOICE: choose one of five setups
setup = "spirotetramat_control"
setup = "spirotetramat_moderate"
setup = "spirotetramat_high"
setup = "ivermectin_control"
setup = "ivermectin_treatment"

## The script from this point on can be run without any manual input/changes


## simulation parameters

# number of generations
if(grepl("spirotetramat", setup, fixed=TRUE)){
  t = 7
} else {
  t = 5
}
# number of simulation repeats
reps = 100
# population size
k = 2000 
# initial resistant allele frequency
if(grepl("spirotetramat", setup, fixed=TRUE)){
  fA0 = 0.05
} else {
  fA0 = 0.5
}


# setup of data to save from simulation runs
fA_reps = matrix(fA0,t+1,reps) 

for(i in 1:reps){
  for(j in 1:t){
    
    ## fitness values

    # resistant homozygote fitness
    if(setup=="spirotetramat_control"){ 
      # for spirotetramat control, the evolutionary advantage of the resistant strain only kicks in after gen 2
      if(j<3){
        wAA = 1
      } else {
        wAA = 2.28
      }
    } else if(setup=="spirotetramat_moderate"){
      wAA = 2.97
    } else if(setup=="spirotetramat_high"){
      wAA = 3.90
    } else if(setup=="ivermectin_control"){
      wAA = 0.34
    } else if(setup=="ivermectin_treatment"){
      wAA = 3.25
    }
    # non-resistant homozygote fitness, set to 1
    waa = 1
    
    if(j==1){ 
      n = k # population size
      genotypes = matrix(0,t+1,2)
      colnames(genotypes) = c("fAA","faa") 
      genotypes[1,] = c(fA0,1-fA0) # mutation starts in homozygotes
    }
    # genotype frequencies
    fAA = genotypes[j,1]
    faa = genotypes[j,2]
    
    # survival frequencies #this is where the fitness cost shows up in the model
    sAA = wAA*fAA
    saa = waa*faa
    
    # zygote frequencies
    zAA = sAA 
    zaa = saa

    # proportional zygote frequencies
    wm = zAA + zaa
    pAA = zAA/wm
    paa = zaa/wm
    
    # stochastic sampling
    pset = c(pAA,paa)
    nn = k # constant population size
    qset = rmultinom(1,nn,prob=pset)
    pAA = qset[1]/nn
    paa = qset[2]/nn
    
    # allele frequencies
    pA = pAA
    pa = paa
    
    # save allele frequencies to main matrix
    fA_reps[j+1,i] = pA 
    
    # update genotype frequencies
    genotypes[1+j,1] = pAA
    genotypes[1+j,2] = paa
    
  }
}

# plot allele frequencies across generations
plot(seq(0,t),fA_reps[,1],type="l",col=viridis(reps)[1],lwd=4,
     ylim=c(0,1),xaxs="i",las=1,main="allele A frequency",xlab="Time",ylab="Frequency",font.lab=2)
for(i in 1:reps){
  points(seq(0,t),fA_reps[,i],type="l",col=viridis(reps)[i],lwd=4)
}

fA_reps = data.frame(fA_reps)
fA_reps = melt(fA_reps)
fA_reps$generation = seq(0,t)

data = read_excel("0031 R input.xlsx")
control_sp = data[data$condition=="control",]
trt20 = data[data$condition=="moderate",]
trt24 = data[data$condition=="high",]

data = read_excel("0032 R input.xlsx")
control_iv = data[data$condition=="control",]
trt = data[data$condition=="treatment",]

if(setup=="spirotetramat_control"){ 
  df = control_sp
} else if(setup=="spirotetramat_moderate"){
  df = trt20
} else if(setup=="spirotetramat_high"){
  df = trt24
} else if(setup=="ivermectin_control"){
  df = control_iv
} else if(setup=="ivermectin_treatment"){
  df = trt
}

p = ggplot(data = fA_reps, aes(x = generation,y=value,group=variable,colour=variable))+
  geom_line()+
  theme_classic()+
  theme(legend.position="none")+
  scale_colour_manual(values = unname(viridis(100)))+
  geom_point(inherit.aes = F, data = df, aes(x=generation,y=frequency),size=2)+
  labs(x="Generation",y="Resistance allele frequency")
print(p)

ggsave("0032_treatment_f.png", p, width =7.5, height=5, dpi=300)



