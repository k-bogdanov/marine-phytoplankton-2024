library(phyloseq)
library(microViz)
library(dplyr)
library(stringr)

#####
# import data
Sven_phyto <- readRDS("~/Dropbox/PhytoBact_CultivationExperiment/William_reanalysis/sergio_data/Phyloseq_object_16S.rds")

# remove unwanted samples
Sven_phyto = ps_filter(Sven_phyto, Culture_type != "DELETE")

# import revised metadata
new_sample_data <- read.csv(file = "~/mappingfile.csv", header = TRUE, fileEncoding = "UTF-8-BOM", check.names = FALSE, row.names = 1)

new_sample_data = new_sample_data %>% 
  mutate(BacterialStatus = if_else(str_detect(Culture_type, "Bacteria"), "Present", "Absent"))

new_sample_data$ExperimentalPhase = factor(new_sample_data$Cultivation_Stage,
                                           levels = c("Lag",
                                                      "Exponential",
                                                      "Stationary",
                                                      "Death"))

# update metadata
sample_data(Sven_phyto) <- new_sample_data
Sven_phyto_metadata <- data.frame(sample_data(Sven_phyto))


#####

# remove any samples with less than 100 sequences

pruned <- prune_samples(sample_sums(Sven_phyto) >= 100, Sven_phyto)
pruned

library(ggplot2)
library(ranacapa)

# calculate rarefaction curves, then plot and color by subsite
p <- ggrare(pruned, step = 1000, color = "Culture_type", se =
              FALSE,parallel = TRUE)+
  scale_y_log10() +
  scale_x_continuous(limits = c(1, 170000))
p


p + facet_wrap(~Culture_type)

# rarefy at 30k

Sven_phyto_rare <- rarefy_even_depth(Sven_phyto, 20000, rngseed=TRUE)
#####

library(microbiomeutilities)


tern_df<-prep_ternary(Sven_phyto_rare, group = "Phyto", abund.thres = 0.01, level = "Genus", prev.thres = 0.25)

library(ggtern)
# Replace empty with Other
tern_df$Phylum[tern_df$Phylum==""] <- "Other"

ggtern(data=tern_df, aes(x=None, y=Coccolithophore, z=Diatom)) + 
  geom_point(aes(color= Genus), 
             alpha=0.5, 
             show.legend=T, 
             size=3) +
  #scale_size(range=c(0, 6)) + 
  geom_mask() + 
  scale_colour_brewer(palette = "Paired") +
  theme_biome_utils()


#repeat with only significant ones
KW_tern_df<-prep_ternary(SPR_clean2, group = "Phyto", abund.thres = 0.01, level = "Genus", prev.thres = 0.1)

# Replace empty with Other
tern_df$Phylum[tern_df$Phylum==""] <- "Other"

ggtern(data=tern_df, aes(x=None, y=Coccolithophore, z=Diatom)) + 
  geom_point(aes(color= Order), 
             alpha=0.5, 
             show.legend=T, 
             size=3) +
  #scale_size(range=c(0, 6)) + 
  geom_mask() + 
  scale_colour_brewer(palette = "Paired") +
  theme_biome_utils()

#####
setwd("~/")
library(dysbiosisR)


physeq<-Sven_Phyto_iCAMP
stage="Exponential"
Phyto="Diatom"
dist2control<-function(stage,physeq,Phyto){
  samples2keep<-sample_names(physeq)[physeq@sam_data$Cultivation_Stage==stage]
  samples2keep_treat<-sample_names(physeq)[physeq@sam_data$Phyto %in% c("None",Phyto)]
  samples2keep<-intersect(samples2keep,samples2keep_treat)
  tempphy<-prune_samples(samples2keep,physeq)
  dist.mat <- phyloseq::distance(tempphy, "bray")
  dysbiosis_2 <- euclideanDistCentroids(tempphy,
                                        dist_mat = dist.mat,
                                        use_squared = TRUE,
                                        group_col = "BacterialStatus",
                                        control_label = "Present",
                                        case_label = "Absent")
  dysbiosis_2
}
dist2control(stage="Lag",physeq=Sven_phyto_rare,Phyto="Coccolithophore")

library(iCAMP)

#phyloseq::phy_tree(Sven_phyto_rare)<-tree_sven
Sven_phyto_rare_dia_bac<-prune_samples(sample_names(Sven_Phyto_iCAMP)[Sven_Phyto_iCAMP@sam_data$Phyto=="Diatom" &
                                                                        Sven_Phyto_iCAMP@sam_data$BacterialStatus=="Present"], Sven_Phyto_iCAMP)
Sven_Phyto_iCAMP_dia_nobac<-prune_samples(sample_names(Sven_Phyto_iCAMP)[Sven_Phyto_iCAMP@sam_data$Phyto=="Diatom" &
                                                                           Sven_Phyto_iCAMP@sam_data$BacterialStatus=="Absent"], Sven_Phyto_iCAMP)
Sven_Phyto_iCAMP_coc_bac<-prune_samples(sample_names(Sven_Phyto_iCAMP)[Sven_Phyto_iCAMP@sam_data$Phyto=="Coccolithophore" &
                                                                         Sven_Phyto_iCAMP@sam_data$BacterialStatus=="Present"], Sven_Phyto_iCAMP)
Sven_Phyto_iCAMP_coc_nobac<-prune_samples(sample_names(Sven_Phyto_iCAMP)[Sven_Phyto_iCAMP@sam_data$Phyto=="Coccolithophore" &
                                                                           Sven_Phyto_iCAMP@sam_data$BacterialStatus=="Absent"], Sven_Phyto_iCAMP)

Sven_Phyto_iCAMP_bac<-prune_samples(sample_names(Sven_Phyto_iCAMP)[Sven_Phyto_iCAMP@sam_data$Phyto=="None" &
                                                                     Sven_Phyto_iCAMP@sam_data$BacterialStatus=="Present"], Sven_Phyto_iCAMP)

#coc_nobac analysis
unlink("iCAMP.iCAMP.Confidence.detail.rda")
unlink("iCAMP.process.CbMPDiCBraya.csv")
unlink("path.rda")
unlink("pd.bin")
unlink("pd.desc")
unlink("pd.taxon.name.csv")
physeq<-Sven_Phyto_iCAMP_coc_nobac
physeq<-prune_taxa(names(taxa_sums(physeq)[taxa_sums(physeq)>0]),physeq)
comm=physeq@otu_table@.Data
tree=physeq@phy_tree
meta.group=data.frame(meta.com=physeq@sam_data$Cultivation_Stage)
rownames(meta.group)=rownames(comm)
setwd("~/Dropbox/PhytoBact_CultivationExperiment/William_reanalysis/")

dir.create("./coc_nobac1")

wd0="./coc_nobac1" # please change to the folder you want to save the pd.big output.
save.wd="./coc_nobac1/"

nworker=4 # parallel computing thread number
rand.time=1000 # usually use 1000 for real data.
bin.size.limit=20 # for real data, usually use a proper number
#download.file("https://github.com/DaliangNing/iCAMP1/blob/master/RPackage/AllVersions/iCAMP_1.6.5.zip",destfile = "iCAMP_1.6.5.zip")
#remotes::install_local("./iCAMP_1.6.5.zip")
library(iCAMP)

Sven_phyto_rare_coc_nobac_icamp=iCAMP::icamp.cm(comm=comm, tree=tree, meta.group=meta.group,
                                                pd.wd="./coc_nobac1", rand=rand.time, nworker=nworker,
                                                bin.size.limit=bin.size.limit,phylo.metric="bMNTD")

Sven_phyto_rare_coc_nobac_icamp$CbMNTDiCBraya$S1_Stage<-Sven_Phyto_iCAMP_coc_nobac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_coc_nobac_icamp$CbMNTDiCBraya$sample1,sample_names(Sven_Phyto_iCAMP_coc_nobac))]
Sven_phyto_rare_coc_nobac_icamp$CbMNTDiCBraya$S2_Stage<-Sven_Phyto_iCAMP_coc_nobac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_coc_nobac_icamp$CbMNTDiCBraya$sample2,sample_names(Sven_Phyto_iCAMP_coc_nobac))]
Sven_phyto_rare_coc_nobac_icamp_x<-Sven_phyto_rare_coc_nobac_icamp$CbMNTDiCBraya
Sven_phyto_rare_coc_nobac_icamp_x<-Sven_phyto_rare_coc_nobac_icamp_x[Sven_phyto_rare_coc_nobac_icamp_x$S1_Stage==Sven_phyto_rare_coc_nobac_icamp_x$S2_Stage,]
Sven_phyto_rare_coc_nobac_icamp_x<-reshape2::melt(Sven_phyto_rare_coc_nobac_icamp_x)
Sven_phyto_rare_coc_nobac_icamp_x$S2_Stage<-NULL
Sven_phyto_rare_coc_nobac_icamp_x<-aggregate(x=Sven_phyto_rare_coc_nobac_icamp_x$value,
                                             by=list(Sven_phyto_rare_coc_nobac_icamp_x$S1_Stage,Sven_phyto_rare_coc_nobac_icamp_x$variable),
                                             FUN=mean)
Sven_phyto_rare_coc_nobac_icamp_x$CustOrd<-factor(Sven_phyto_rare_coc_nobac_icamp_x$Group.1,levels=c("Lag","Exponential","Stationary","Death"))
Sven_phyto_rare_coc_nobac_icamp_x$Group.2<-gsub(".", " ", Sven_phyto_rare_coc_nobac_icamp_x$Group.2,fixed = TRUE)
Sven_phyto_rare_coc_nobac_icamp_x$Group.2 = factor(Sven_phyto_rare_coc_nobac_icamp_x$Group.2, levels = c('Heterogeneous Selection','Homogeneous Selection', 'Dispersal Limitation', 'Homogenizing Dispersal', 'Drift and Others'))

iCAMP.col <- c("Heterogeneous Selection" = "#332288",
               "Homogeneous Selection" = "#23C3E4",
               "Dispersal Limitation" = "red",
               "Homogenizing Dispersal" = "#F25F14FF",
               "Drift and Others" = "#EBD239")



ggplot(Sven_phyto_rare_coc_nobac_icamp_x) +
  aes(x = CustOrd, y = x, fill = Group.2) +
  geom_col() +
  scale_fill_manual("Ecological Process",values = iCAMP.col)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 0.9, size=12),
        axis.text.y = element_text(colour = "black", size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.title =element_text(size =12, face="bold"),
        legend.text = element_text(size = 12),
        legend.position="right",
        legend.key.size = unit(0.5, "cm"),
        #strip.text.x = element_text(size=20, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),                                                                    
        strip.background = element_rect(colour="black"))+
  labs(y="Relative contribution")


#coc_bac analysis
setwd("~/")
dir.create("./coc_bac1")
setwd("./coc_bac1")
wd0="coc_bac1" # please change to the folder you want to save the pd.big output.
save.wd="./coc_bac1"

unlink("iCAMP.iCAMP.Confidence.detail.rda")
unlink("iCAMP.process.CbMPDiCBraya.csv")
unlink("path.rda")
unlink("pd.bin")
unlink("pd.desc")
unlink("pd.taxon.name.csv")
physeq<-Sven_Phyto_iCAMP_coc_bac
physeq<-prune_taxa(names(taxa_sums(physeq)[taxa_sums(physeq)>0]),physeq)
comm=physeq@otu_table@.Data
tree=physeq@phy_tree
meta.group=data.frame(meta.com=physeq@sam_data$Cultivation_Stage)
rownames(meta.group)=rownames(comm)
#nworker=1 # parallel computing thread number
#rand.time=1000 # usually use 1000 for real data.
#bin.size.limit=50 # for real data, usually use a proper number
Sven_phyto_rare_coc_bac_icamp=icamp.cm(comm=comm, tree=tree, meta.group=meta.group,
                                       pd.wd="./coc_bac1", rand=rand.time, nworker=nworker,
                                       bin.size.limit=bin.size.limit,phylo.metric="bMNTD")
Sven_phyto_rare_coc_bac_icamp$CbMNTDiCBraya$S1_Stage<-Sven_Phyto_iCAMP_coc_bac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_coc_bac_icamp$CbMNTDiCBraya$sample1,sample_names(Sven_Phyto_iCAMP_coc_bac))]
Sven_phyto_rare_coc_bac_icamp$CbMNTDiCBraya$S2_Stage<-Sven_Phyto_iCAMP_coc_bac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_coc_bac_icamp$CbMNTDiCBraya$sample2,sample_names(Sven_Phyto_iCAMP_coc_bac))]
Sven_phyto_rare_coc_bac_icamp_x<-Sven_phyto_rare_coc_bac_icamp$CbMNTDiCBraya
Sven_phyto_rare_coc_bac_icamp_x<-Sven_phyto_rare_coc_bac_icamp_x[Sven_phyto_rare_coc_bac_icamp_x$S1_Stage==Sven_phyto_rare_coc_bac_icamp_x$S2_Stage,]
Sven_phyto_rare_coc_bac_icamp_x<-reshape2::melt(Sven_phyto_rare_coc_bac_icamp_x)
Sven_phyto_rare_coc_bac_icamp_x$S2_Stage<-NULL
Sven_phyto_rare_coc_bac_icamp_x<-aggregate(x=Sven_phyto_rare_coc_bac_icamp_x$value,
                                           by=list(Sven_phyto_rare_coc_bac_icamp_x$S1_Stage,Sven_phyto_rare_coc_bac_icamp_x$variable),
                                           FUN=mean)
Sven_phyto_rare_coc_bac_icamp_x$CustOrd<-factor(Sven_phyto_rare_coc_bac_icamp_x$Group.1,levels=c("Lag","Exponential","Stationary","Death"))

Sven_phyto_rare_coc_bac_icamp_x$Group.2<-gsub(".", " ", Sven_phyto_rare_coc_bac_icamp_x$Group.2,fixed = TRUE)
Sven_phyto_rare_coc_bac_icamp_x$Group.2 = factor(Sven_phyto_rare_coc_bac_icamp_x$Group.2, levels = c('Heterogeneous Selection','Homogeneous Selection', 'Dispersal Limitation', 'Homogenizing Dispersal', 'Drift and Others'))



ggplot(Sven_phyto_rare_coc_bac_icamp_x) +
  aes(x = CustOrd, y = x, fill = Group.2) +
  geom_col() +
  scale_fill_manual("Ecological Process",values = iCAMP.col)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 0.9, size=12),
        axis.text.y = element_text(colour = "black", size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.title =element_text(size =12, face="bold"),
        legend.text = element_text(size = 12),
        legend.position="right",
        legend.key.size = unit(0.5, "cm"),
        #strip.text.x = element_text(size=20, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),                                                                    
        strip.background = element_rect(colour="black"))+
  labs(y="Relative contribution")


#dia_bac
physeq<-Sven_phyto_rare_dia_bac
physeq<-prune_taxa(names(taxa_sums(physeq)[taxa_sums(physeq)>0]),physeq)
comm=physeq@otu_table@.Data
tree=physeq@phy_tree
meta.group=data.frame(meta.com=physeq@sam_data$Cultivation_Stage)
rownames(meta.group)=rownames(comm)
setwd("~")
dir.create("./dia_bac1")
setwd("./dia_bac1")
wd0="./dia_bac1"# please change to the folder you want to save the pd.big output.
save.wd="./dia_bac1"
#nworker=1 # parallel computing thread number
#rand.time=1000 # usually use 1000 for real data.
#bin.size.limit=50 # for real data, usually use a proper number
Sven_phyto_rare_dia_bac_icamp=icamp.cm(comm=comm, tree=tree, meta.group=meta.group,
                                       pd.wd="./dia_bac1", rand=rand.time, nworker=nworker,
                                       bin.size.limit=bin.size.limit,phylo.metric="bMNTD")
Sven_phyto_rare_dia_bac_icamp$CbMNTDiCBraya$S1_Stage<-Sven_phyto_rare_dia_bac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_dia_bac_icamp$CbMNTDiCBraya$sample1,sample_names(Sven_phyto_rare_dia_bac))]
Sven_phyto_rare_dia_bac_icamp$CbMNTDiCBraya$S2_Stage<-Sven_phyto_rare_dia_bac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_dia_bac_icamp$CbMNTDiCBraya$sample2,sample_names(Sven_phyto_rare_dia_bac))]
Sven_phyto_rare_dia_bac_icamp_x<-Sven_phyto_rare_dia_bac_icamp$CbMNTDiCBraya
Sven_phyto_rare_dia_bac_icamp_x<-Sven_phyto_rare_dia_bac_icamp_x[Sven_phyto_rare_dia_bac_icamp_x$S1_Stage==Sven_phyto_rare_dia_bac_icamp_x$S2_Stage,]
Sven_phyto_rare_dia_bac_icamp_x<-reshape2::melt(Sven_phyto_rare_dia_bac_icamp_x)
Sven_phyto_rare_dia_bac_icamp_x$S2_Stage<-NULL
Sven_phyto_rare_dia_bac_icamp_x<-aggregate(x=Sven_phyto_rare_dia_bac_icamp_x$value,
                                           by=list(Sven_phyto_rare_dia_bac_icamp_x$S1_Stage,Sven_phyto_rare_dia_bac_icamp_x$variable),
                                           FUN=mean)
Sven_phyto_rare_dia_bac_icamp_x$CustOrd<-factor(Sven_phyto_rare_dia_bac_icamp_x$Group.1,levels=c("Lag","Exponential","Stationary","Death"))


Sven_phyto_rare_dia_bac_icamp_x$Group.2<-gsub(".", " ", Sven_phyto_rare_dia_bac_icamp_x$Group.2,fixed = TRUE)
Sven_phyto_rare_dia_bac_icamp_x$Group.2 = factor(Sven_phyto_rare_dia_bac_icamp_x$Group.2, levels = c('Heterogeneous Selection','Homogeneous Selection', 'Dispersal Limitation', 'Homogenizing Dispersal', 'Drift and Others'))


ggplot(Sven_phyto_rare_dia_bac_icamp_x) +
  aes(x = CustOrd, y = x, fill = Group.2) +
  geom_col() +
  scale_fill_manual("Ecological Process",values = iCAMP.col)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 0.9, size=12),
        axis.text.y = element_text(colour = "black", size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.title =element_text(size =12, face="bold"),
        legend.text = element_text(size = 12),
        legend.position="right",
        legend.key.size = unit(0.5, "cm"),
        #strip.text.x = element_text(size=20, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),                                                                    
        strip.background = element_rect(colour="black"))+
  labs(y="Relative contribution")


#dia_nobac

physeq<-Sven_Phyto_iCAMP_dia_nobac
physeq<-prune_taxa(names(taxa_sums(physeq)[taxa_sums(physeq)>0]),physeq)
comm=physeq@otu_table@.Data
tree=physeq@phy_tree
meta.group=data.frame(meta.com=physeq@sam_data$Cultivation_Stage)
rownames(meta.group)=rownames(comm)
setwd("~/")
dir.create("./dia_nobac1")
setwd("./dia_nobac1")
wd0="dia_nobac1" # please change to the folder you want to save the pd.big output.
save.wd="./dia_nobac1"

Sven_phyto_rare_dia_nobac_icamp=icamp.cm(comm=comm, tree=tree, meta.group=meta.group,
                                         pd.wd="./dia_nobac1", rand=rand.time, nworker=nworker,
                                         bin.size.limit=bin.size.limit,phylo.metric="bMNTD")
Sven_phyto_rare_dia_nobac_icamp$CbMNTDiCBraya$S1_Stage<-Sven_Phyto_iCAMP_dia_nobac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_dia_nobac_icamp$CbMNTDiCBraya$sample1,sample_names(Sven_Phyto_iCAMP_dia_nobac))]
Sven_phyto_rare_dia_nobac_icamp$CbMNTDiCBraya$S2_Stage<-Sven_Phyto_iCAMP_dia_nobac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_dia_nobac_icamp$CbMNTDiCBraya$sample2,sample_names(Sven_Phyto_iCAMP_dia_nobac))]
Sven_phyto_rare_dia_nobac_icamp_x<-Sven_phyto_rare_dia_nobac_icamp$CbMNTDiCBraya
Sven_phyto_rare_dia_nobac_icamp_x<-Sven_phyto_rare_dia_nobac_icamp_x[Sven_phyto_rare_dia_nobac_icamp_x$S1_Stage==Sven_phyto_rare_dia_nobac_icamp_x$S2_Stage,]
Sven_phyto_rare_dia_nobac_icamp_x<-reshape2::melt(Sven_phyto_rare_dia_nobac_icamp_x)
Sven_phyto_rare_dia_nobac_icamp_x$S2_Stage<-NULL
Sven_phyto_rare_dia_nobac_icamp_x<-aggregate(x=Sven_phyto_rare_dia_nobac_icamp_x$value,
                                             by=list(Sven_phyto_rare_dia_nobac_icamp_x$S1_Stage,Sven_phyto_rare_dia_nobac_icamp_x$variable),
                                             FUN=mean)
Sven_phyto_rare_dia_nobac_icamp_x$CustOrd<-factor(Sven_phyto_rare_dia_nobac_icamp_x$Group.1,levels=c("Lag","Exponential","Stationary","Death"))



Sven_phyto_rare_dia_nobac_icamp_x$Group.2<-gsub(".", " ", Sven_phyto_rare_dia_nobac_icamp_x$Group.2,fixed = TRUE)
Sven_phyto_rare_dia_nobac_icamp_x$Group.2 = factor(Sven_phyto_rare_dia_nobac_icamp_x$Group.2, levels = c('Heterogeneous Selection','Homogeneous Selection', 'Dispersal Limitation', 'Homogenizing Dispersal', 'Drift and Others'))



ggplot(Sven_phyto_rare_dia_nobac_icamp_x) +
  aes(x = CustOrd, y = x, fill = Group.2) +
  geom_col() +
  scale_fill_manual("Ecological Process",values = iCAMP.col)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 0.9, size=12),
        axis.text.y = element_text(colour = "black", size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.title =element_text(size =12, face="bold"),
        legend.text = element_text(size = 12),
        legend.position="right",
        legend.key.size = unit(0.5, "cm"),
        #strip.text.x = element_text(size=20, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),                                                                    
        strip.background = element_rect(colour="black"))+
  labs(y="Relative contribution")

#bac


physeq<-Sven_Phyto_iCAMP_bac
physeq<-prune_taxa(names(taxa_sums(physeq)[taxa_sums(physeq)>0]),physeq)
comm=physeq@otu_table@.Data
tree=physeq@phy_tree
meta.group=data.frame(meta.com=physeq@sam_data$Cultivation_Stage)
rownames(meta.group)=rownames(comm)
setwd("~/")
dir.create("./bac1")
setwd("./bac1")

wd0="bac1"#getwd() # please change to the folder you want to save the pd.big output.
save.wd="./bac1"
Sven_phyto_rare_bac_icamp=icamp.cm(comm=comm, tree=tree, meta.group=meta.group,
                                   pd.wd="./bac1", rand=rand.time, nworker=nworker,
                                   bin.size.limit=bin.size.limit,phylo.metric="bMNTD")
Sven_phyto_rare_bac_icamp$CbMNTDiCBraya$S1_Stage<-Sven_Phyto_iCAMP_bac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_bac_icamp$CbMNTDiCBraya$sample1,sample_names(Sven_Phyto_iCAMP_bac))]
Sven_phyto_rare_bac_icamp$CbMNTDiCBraya$S2_Stage<-Sven_Phyto_iCAMP_bac@sam_data$Cultivation_Stage[match(Sven_phyto_rare_bac_icamp$CbMNTDiCBraya$sample2,sample_names(Sven_Phyto_iCAMP_bac))]
Sven_phyto_rare_bac_icamp_x<-Sven_phyto_rare_bac_icamp$CbMNTDiCBraya
Sven_phyto_rare_bac_icamp_x<-Sven_phyto_rare_bac_icamp_x[Sven_phyto_rare_bac_icamp_x$S1_Stage==Sven_phyto_rare_bac_icamp_x$S2_Stage,]
Sven_phyto_rare_bac_icamp_x<-reshape2::melt(Sven_phyto_rare_bac_icamp_x)
Sven_phyto_rare_bac_icamp_x$S2_Stage<-NULL
Sven_phyto_rare_bac_icamp_x<-aggregate(x=Sven_phyto_rare_bac_icamp_x$value,
                                       by=list(Sven_phyto_rare_bac_icamp_x$S1_Stage,Sven_phyto_rare_bac_icamp_x$variable),
                                       FUN=mean)
Sven_phyto_rare_bac_icamp_x$CustOrd<-factor(Sven_phyto_rare_bac_icamp_x$Group.1,levels=c("Lag","Exponential","Stationary","Death"))


Sven_phyto_rare_bac_icamp_x$Group.2<-gsub(".", " ", Sven_phyto_rare_bac_icamp_x$Group.2,fixed = TRUE)
Sven_phyto_rare_bac_icamp_x$Group.2 = factor(Sven_phyto_rare_bac_icamp_x$Group.2, levels = c('Heterogeneous Selection','Homogeneous Selection', 'Dispersal Limitation', 'Homogenizing Dispersal', 'Drift and Others'))



ggplot(Sven_phyto_rare_bac_icamp_x) +
  aes(x = CustOrd, y = x, fill = Group.2) +
  geom_col() +
  scale_fill_manual("Ecological Process",values = iCAMP.col)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 0.9, size=12),
        axis.text.y = element_text(colour = "black", size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.title =element_text(size =12, face="bold"),
        legend.text = element_text(size = 12),
        legend.position="right",
        legend.key.size = unit(0.5, "cm"),
        #strip.text.x = element_text(size=20, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),                                                                    
        strip.background = element_rect(colour="black"))+
  labs(y="Relative contribution")


Sven_phyto_rare_dia_nobac_icamp_x$Group="Diatom Only"
Sven_phyto_rare_dia_bac_icamp_x$Group="Diatom + Bacteria"
Sven_phyto_rare_coc_nobac_icamp_x$Group="Coccolithophore Only"
Sven_phyto_rare_coc_bac_icamp_x$Group="Coccolithophore + Bacteria"
Sven_phyto_rare_bac_icamp_x$Group="Bacteria Only"
Sven_phyto_rare_dia_nobac_icamp_x$
  Sven_phyto_rare_coc_nobac_icamp_x
sel_icamp_res<-rbind(Sven_phyto_rare_bac_icamp_x,Sven_phyto_rare_coc_bac_icamp_x,
                     Sven_phyto_rare_coc_nobac_icamp_x,Sven_phyto_rare_dia_bac_icamp_x,
                     Sven_phyto_rare_dia_nobac_icamp_x)
sel_icamp_res$CustOrd<-factor(sel_icamp_res$Group.1,levels=c("Lag","Exponential","Stationary","Death"))
sel_icamp_res$Group<-factor(sel_icamp_res$Group,levels=c("Coccolithophore Only","Diatom Only", "Bacteria Only","Coccolithophore + Bacteria" ,"Diatom + Bacteria"))


ggplot(sel_icamp_res) +
  aes(x = CustOrd, y = x, fill = Group.2) +
  geom_col() +
  scale_fill_manual("Ecological Process",values = iCAMP.col)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 0.9, size=13),
        axis.text.y = element_text(colour = "black", size=13),
        axis.title.y = element_text(face="bold",size=13),
        legend.title =element_text(size =13, face="bold"),
        legend.text = element_text(size = 13),
        legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.7, "cm"),
        strip.text.x = element_text(size=13, face="bold"),
        strip.text.y = element_text(size=13, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),                                                                    
        strip.background = element_rect(colour="black"))+
  labs(y="Relative contribution") +
  facet_wrap(vars(Group))

ggsave(".//iCAMP_composite.pdf",width=12,height=8,units ="in", device="pdf")