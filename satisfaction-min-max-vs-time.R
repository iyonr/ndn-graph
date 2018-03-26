#!/usr/bin/env Rscript

#args = commandArgs(TRUE)
#if (length(args) == 0) {
#  cat ("ERROR: Scenario parameters should be specified\n")
#  q(status = 1)
#}

prefixes = "fairness,satisfaction-accept,satisfaction-pushback"
topo     = "7018.r0"
evils    = "140"
runs     = "1,2,3,4,5,6,7,8,9,10"
folder   = "attackISP"
good     = "0"
producer = "gw"

suppressPackageStartupMessages (library(ggplot2))
suppressPackageStartupMessages (library(reshape2))
suppressPackageStartupMessages (library(doBy))
suppressPackageStartupMessages (library(plyr))
suppressPackageStartupMessages (library(scales))

source ("graph-style.R")

name = paste (sep="-", prefixes, "topo", topo, "evil", evils, "producer", producer)
filename = paste(sep="", "results/",folder,"/process/", name, "-all-data.dat")

if (file_test("-f", filename)) {
  cat ("Loading data from", filename, "\n")
  load (filename)
  
} else {
   data.all = data.frame ()
   for (evil in strsplit(evils,",")[[1]]) {
     for (prefix in strsplit(prefixes,",")[[1]]) {
       name = paste (sep="-", prefix, "topo", topo, "evil", evil, "good", good, "producer", producer)
       filename = paste(sep="", "results/",folder,"/process/", name, ".txt")
       cat ("Reading from", filename, "\n")

       load (filename)
       
       data.all <- rbind (data.all, data)
     }
   }

   name = paste (sep="-", prefixes, "topo", topo, "evil", evils, "producer", producer)
   filename = paste(sep="", "results/",folder,"/process/", name, "-all-data.dat")
   
   cat ("Saving data to", filename, "\n")
   save (data.all, file=filename)
}
data.all$Evil = factor(data.all$Evil)

name2 = paste (sep="-", topo, "good", good, "producer", producer)

data.all$Scenario = ordered (data.all$Scenario,
  c("fairness", "satisfaction-accept", "satisfaction-pushback"))
levels(data.all$Scenario) <- sub("^satisfaction-pushback$", "Satisfaction-based pushback", levels(data.all$Scenario))
levels(data.all$Scenario) <- sub("^satisfaction-accept$",   "Satisfaction-based Interest acceptance", levels(data.all$Scenario))
levels(data.all$Scenario) <- sub("^fairness$",              "Token bucket with per interface fairness", levels(data.all$Scenario))

cat (sep="", "Writing to ", paste(sep="","graphs/pdfs/", folder, "/",name2,".pdf"))
pdf (paste(sep="","graphs/pdfs/", folder, "/",name2,".pdf"), width=5, height=4)

minTime = 300
attackTime = 300

gdata = subset(data.all, minTime-100 <= Time & Time < minTime+attackTime+100)

g <- ggplot (gdata) +
  stat_summary(aes(x=Time-minTime, y=Ratio, color=Scenario), geom="line", fun.y=mean, size=0.4) +

  theme_custom () +
  xlab ("Waktu sejak Serangan dilancarkan (detik)") +
  ylab ("Min/Max Satisfaction Ratios") +
  scale_colour_brewer(palette="Set1") +
  scale_fill_brewer(palette="PuOr") +
  scale_y_continuous (limits=c(0,1), breaks=seq(0,1,0.1), labels=percent_format ()) +
  scale_x_continuous (limits=c(-100,500), breaks=seq(-100,500,50)) +
  ##  facet_wrap (~ Scenario, nrow=5, ncol=1) + #Makhluk ini yang bikin dia pecah2
  geom_vline(xintercept = attackTime) +
  theme (legend.key.size = unit(0.8, "lines"),
         legend.position="bottom", #c(1.0, 0.0),
         legend.justification=c(1,0),
         legend.background = element_rect (fill="white", colour="black", size=0.1))  

print (g)

x = dev.off ()

