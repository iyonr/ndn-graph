#!/usr/bin/env Rscript

prefixes = "satisfaction-pushback-400-rtt-150,satisfaction-pushback-400-rtt-250,satisfaction-pushback-400-rtt-330,satisfaction-pushback-400-rtt-400,satisfaction-pushback-400-rtt-500"
#prefixes = "fairness-payload-400-rtt-150,fairness-payload-400-rtt-250,fairness-payload-400-rtt-330,fairness-payload-400-rtt-400,fairness-payload-400-rtt-500"
#prefixes = "satisfaction-accept-payload-400,satisfaction-accept-payload-700,satisfaction-accept-payload-1100,satisfaction-accept-payload-1500,satisfaction-accept-payload-2000"
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
      name = paste (sep="-", prefix, "topo", topo, "evil", evil, "producer", producer, "data")
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
                             c("satisfaction-pushback-400-rtt-150", "satisfaction-pushback-400-rtt-250", "satisfaction-pushback-400-rtt-330", "satisfaction-pushback-400-rtt-400", "satisfaction-pushback-400-rtt-500"))
levels(data.all$Scenario) <- sub("^satisfaction-pushback-400-rtt-500$", "SPA Payload 400B, RTT 500ms", levels(data.all$Scenario))
levels(data.all$Scenario) <- sub("^satisfaction-pushback-400-rtt-400$", "SPA Payload 400B, RTT 400ms", levels(data.all$Scenario))
levels(data.all$Scenario) <- sub("^satisfaction-pushback-400-rtt-330$", "SPA Payload 400B, RTT 330ms", levels(data.all$Scenario))
levels(data.all$Scenario) <- sub("^satisfaction-pushback-400-rtt-250$", "SPA Payload 400B, RTT 250ms", levels(data.all$Scenario))
levels(data.all$Scenario) <- sub("^satisfaction-pushback-400-rtt-150$", "SPA Payload 400B, RTT 150ms", levels(data.all$Scenario))

# data.all$Scenario = ordered (data.all$Scenario,
#                             c("fairness-400-rtt-150", "fairness-400-rtt-250", "fairness-400-rtt-330", "fairness-400-rtt-400", "fairness-400-rtt-500"))
# levels(data.all$Scenario) <- sub("^fairness-400-rtt-500$", "TB Payload 400B, RTT 500ms", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^fairness-400-rtt-400$", "TB Payload 400B, RTT 400ms", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^fairness-400-rtt-330$", "TB Payload 400B, RTT 330ms", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^fairness-400-rtt-250$", "TB Payload 400B, RTT 250ms", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^fairness-400-rtt-150$", "TB Payload 400B, RTT 150ms", levels(data.all$Scenario))

#data.all$Scenario = ordered (data.all$Scenario,
#                             c("satisfaction-accept-400", "satisfaction-accept-700", "satisfaction-accept-1100", "satisfaction-accept-1500", "satisfaction-accept-2000"))
# levels(data.all$Scenario) <- sub("^satisfaction-accept-2000$", "Data Payload size 2000 bytes", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^satisfaction-accept-1500$", "Data Payload size 1500 bytes", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^satisfaction-accept-1100$", "Data Payload size 1100 bytes", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^satisfaction-accept-700$", "Data Payload size 700 bytes", levels(data.all$Scenario))
# levels(data.all$Scenario) <- sub("^satisfaction-accept-400$", "Data Payload size 400 bytes", levels(data.all$Scenario))

#cat (sep="", "Writing to ", paste(sep="","graphs/pdfs/", folder, "/",name2,".pdf"))
#pdf (paste(sep="","graphs/pdfs/", folder, "/",name2,".pdf"), width=5, height=4)

# minTime = 300
attackTime = 300

# gdata = subset(data.all, minTime-100 <= Time & Time < minTime+attackTime+200)

g <- ggplot (data.all) +
  stat_summary(aes(x=Time, y=Ratio, color=Scenario), geom="line", fun.y=mean, size=0.4) +
  
  theme_custom () +
  labs (title = "Comparison of Satisfaction Pushback Algorithm\n Based on Payload size 400 Bytes and Data Payload Size", caption = "(Sumber: Hasil Pengolahan Data Primer)") +
  xlab ("Waktu sejak Serangan dilancarkan (detik)") +
  ylab ("Tingkat Kualitas Layanan (Satisfaction Ratios) Yang dirasakan oleh \"good\" Consumer") +
  scale_colour_brewer(palette="Set1") +
  scale_fill_brewer(palette="PuOr") +
  scale_y_continuous (limits=c(0,1), breaks=seq(0,1,0.1), labels=percent_format ()) +
  scale_x_continuous (limits=c(0,900), breaks=seq(0,900,50)) +
  geom_vline(xintercept = attackTime) +
  geom_vline(xintercept = 600) +
  text()
  annotate("rect", xmin=300, xmax=600, ymin=-Inf, ymax=1, alpha=0.1, fill="red") +
#  geom_rect(aes(xmin = 300, xmax = 600, ymin = -Inf, ymax = 1), fill="red", alpha=0.4) +
  theme (legend.key.size = unit(0.8, "lines"),
         legend.position="bottom", #c(1.0, 0.0),
#         legend.justification=c(1,0),
         legend.background = element_rect (fill="white", colour="black", size=0.1))  

print (g)

#x = dev.off ()
