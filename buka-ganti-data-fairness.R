#!/usr/bin/env Rscript

prefixes = "fairness-payload-2000"
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
filename = paste(sep="", "results/",folder,"/process/", name, ".txt")

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
  
  levels(data.all$Scenario) <- sub("^fairness$", "fairness-2000", levels(data.all$Scenario))
  name = paste (sep="-", prefixes, "topo", topo, "evil", evils, "producer", producer)
  filename = paste(sep="", "results/",folder,"/process/", name, "-data.txt")
  
  cat ("Saving data to", filename, "\n")
  data = data.all
  save (data, file=filename)
}

