rm(list=ls())
library(igraph)
library(backbone)

#Import data
dat <- read.csv(url("https://osf.io/download/bgjvx/"), header = TRUE)
dat <- dat[which(!is.na(dat$speaker_id)),]  #Remove events with no speaker

#Data as biadjacency matrix
dat <- table(dat[,c("event_id", "speaker_name")])  
dat[dat>1] <- 1
dat <- t(dat)

#Exclude events with fewer than two speakers
dat <- dat[,colSums(dat)>1]

#Data as bipartite graph
dat <- graph_from_biadjacency_matrix(dat)

#Extract backbone of bipartite projection
bb <- backbone_from_bipartite(dat, alpha = 0.001, mtc = "fdr")

#Extract giant component
component <- components(bb)
bb <- induced_subgraph(bb, V(bb)[component$membership == 1])

#Plot
pdf("plot.pdf", width = 5, height = 5)
plot(bb,
     vertex.label = NA, vertex.size = 1, 
     edge.color = rgb(0,0,0,.1),
     main = "Speakers with a statistically\nsignificant (alpha < 0.001) number\n of event co-appearances")
dev.off()


