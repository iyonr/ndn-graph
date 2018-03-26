#!/usr/bin/env Rscript

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
suppressPackageStartupMessages (library(RColorBrewer))

source ("graph-style.R")

name = paste (sep="-", prefixes, "topo", topo, "evil", evils, "producer", producer)
filename = paste(sep="", "results/",folder,"/process/", name, "-rtt-330ms-data.dat")

if (file_test("-f", filename)) {
  cat ("Loading data from", filename, "\n")
  load (filename)
  
} else {
  data.semua = data.frame ()
  for (evil in strsplit(evils,",")[[1]]) {
    for (prefix in strsplit(prefixes,",")[[1]]) {
      name = paste (sep="-", prefix, "all-data")
      filename = paste(sep="", "results/",folder,"/process/", name, ".dat")
      cat ("Reading from", filename, "\n")
      
      load (filename)
      
      data.semua <- rbind (data.semua, data.all)
    }
  }
  
  name = paste (sep="-", prefixes, "topo", topo, "evil", evils, "producer", producer)
  filename = paste(sep="", "results/",folder,"/process/", name, "-rtt-330ms-data.dat")
  
  cat ("Saving data to", filename, "\n")
  save (data.semua, file=filename)
}
data.semua$Evil = factor(data.semua$Evil)

name2 = paste (sep="-", topo, "good", good, "producer", producer)


data.semua$Scenario = ordered (data.semua$Scenario,
                             c("fairness-400", "satisfaction-accept-400", "satisfaction-pushback-400", "fairness-700", "satisfaction-accept-700", "satisfaction-pushback-700", "fairness-1100", "satisfaction-accept-1100", "satisfaction-pushback-1100", "fairness-1500", "satisfaction-accept-1500", "satisfaction-pushback-1500", "fairness-2000", "satisfaction-accept-2000", "satisfaction-pushback-2000"))
levels(data.semua$Scenario) <- sub("^satisfaction-pushback-2000$", "2000 bytes - S. Pushback", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-accept-2000$", "2000 bytes - S. Accept", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^fairness-2000$", "2000 bytes - Token bucket", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-pushback-1500$", "1500 bytes - S. Pushback", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-accept-1500$", "1500 bytes - S. Accept", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^fairness-1500$", "1500 bytes - Token bucket", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-pushback-1100$", "1100 bytes - S. Pushback", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-accept-1100$", "1100 bytes - S. Accept", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^fairness-1100$", "1100 bytes - Token bucket", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-pushback-700$", "700 bytes - S. Pushback", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-accept-700$", "700 bytes - S. Accept", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^fairness-700$", "700 bytes - Token bucket", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-pushback-400$", "400 bytes - S. Pushback", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^satisfaction-accept-400$", "400 bytes - S. Accept", levels(data.semua$Scenario))
levels(data.semua$Scenario) <- sub("^fairness-400$", "400 bytes - Token bucket", levels(data.semua$Scenario))


cat (sep="", "Writing to ", paste(sep="","graphs/pdfs/", folder, "/",name2,".pdf"))
pdf (paste(sep="","graphs/pdfs/", folder, "/",name2,".pdf"), width=5, height=4)

minTime = 300
attackTime = 300

gdata = subset(data.semua, minTime-100 <= Time & Time < minTime+attackTime+200)

getPalette  <- colorRampPalette(brewer.pal(12, "Set3"))
myPal <- getPalette(length(unique(data.semua$Scenario)))

g <- ggplot (gdata) +
  stat_summary(aes(x=Time-minTime, y=Ratio, color=Scenario), geom="line", fun.y=mean, size=0.4) +
  
  theme_custom () +
  labs (title = "Comparison of Multiple Interest Flooding Attack Countermeasures Algorithm\n Based on Data Payload Size", caption = "(Sumber: Hasil Pengolahan Data Primer)") +
  xlab ("Waktu sejak Serangan dilancarkan (detik)") +
  ylab ("Tingkat Kualitas Layanan (Satisfaction Ratios) Yang dirasakan oleh \"good\" Consumer") +
#  scale_colour_brewer(palette="Set1") +
  scale_fill_brewer(palette(myPal)) +
  scale_y_continuous (limits=c(0,1), breaks=seq(0,1,0.1), labels=percent_format ()) +
  scale_x_continuous (limits=c(-100,500), breaks=seq(-100,500,50)) +
  geom_vline(xintercept = attackTime) +
  theme (legend.key.size = unit(0.8, "lines"),
         legend.position="bottom", #c(1.0, 0.0),
#         legend.justification=c(1,0),
         legend.background = element_rect (fill="white", colour="black", size=0.1))  

print (g)

  x = dev.off ()
