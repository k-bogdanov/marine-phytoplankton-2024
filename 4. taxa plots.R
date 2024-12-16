
library(plyr)
library(scales)

# clean graph and cluster by Culture_type

library(dplyr)
library(microViz)
library(fantaxtic)
library(speedyseq)
library(phylosmith)

Sven_phyto_rare <- merge_treatments(Sven_phyto_rare, c("Culture_type", "Cultivation_Stage"))

SPR_clean <- name_na_taxa(Sven_phyto_rare, na_label = "Unidentified <tax> (<rank>)")

SPR_clean_test<-tax_fix(SPR_clean)

SPR_clean_test

SPR_clean_test %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Phylum", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))

SPR_clean_test %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Order", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))
ggsave("",width=12,height=8,units ="in", device="pdf")


test<-SPR_clean_test %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Genus", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))
ggsave("",width=12,height=8,units ="in", device="pdf")

test<-data.frame(test[["data"]])


test<-as.data.frame(test)



Control_Coccoonly_Lag <- subset_samples(Sven_phyto_rare_Lag, Culture_type %in% c("BacterialControl", "CoccolithophoreOnly"))

Control_Coccobact_Lag <- subset_samples(Sven_phyto_rare_Lag, Culture_type %in% c("BacterialControl", "CoccolithophoreBacteria"))

Control_DiatomOnly_Lag <- subset_samples(Sven_phyto_rare_Lag, Culture_type %in% c("BacterialControl", "DiatomOnly"))

Control_Diatombact_Lag <- subset_samples(Sven_phyto_rare_Lag, Culture_type %in% c("BacterialControl", "DiatomBacteria"))


#no hits after p adjusted so used just p value
Control_Coccoonly_Lag_df<-Control_Coccoonly_Lag %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

library(rstatix)
# do tests
KW_taxa_Control_Coccoonly_Lag<-Control_Coccoonly_Lag_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Coccobact_Lag_df<-Control_Coccobact_Lag %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Coccobact_Lag<-Control_Coccobact_Lag_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_DiatomOnly_Lag_df<-Control_DiatomOnly_Lag %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_DiatomOnly_Lag<-Control_DiatomOnly_Lag_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Diatombact_Lag_df<-Control_Diatombact_Lag %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Diatombact_Lag<-Control_Diatombact_Lag_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)


Control_Coccoonly_Exponential <- subset_samples(Sven_phyto_rare_Exponential, Culture_type %in% c("BacterialControl", "CoccolithophoreOnly"))

Control_Coccobact_Exponential <- subset_samples(Sven_phyto_rare_Exponential, Culture_type %in% c("BacterialControl", "CoccolithophoreBacteria"))

Control_DiatomOnly_Exponential <- subset_samples(Sven_phyto_rare_Exponential, Culture_type %in% c("BacterialControl", "DiatomOnly"))

Control_Diatombact_Exponential <- subset_samples(Sven_phyto_rare_Exponential, Culture_type %in% c("BacterialControl", "DiatomBacteria"))


#no hits after p adjusted so used just p value
Control_Coccoonly_Exponential_df<-Control_Coccoonly_Exponential %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

library(rstatix)
# do tests
KW_taxa_Control_Coccoonly_Exponential<-Control_Coccoonly_Exponential_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Coccobact_Exponential_df<-Control_Coccobact_Exponential %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Coccobact_Exponential<-Control_Coccobact_Exponential_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_DiatomOnly_Exponential_df<-Control_DiatomOnly_Exponential %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_DiatomOnly_Exponential<-Control_DiatomOnly_Exponential_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Diatombact_Exponential_df<-Control_Diatombact_Exponential %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Diatombact_Exponential<-Control_Diatombact_Exponential_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)


Control_Coccoonly_Stationary <- subset_samples(Sven_phyto_rare_Stationary, Culture_type %in% c("BacterialControl", "CoccolithophoreOnly"))

Control_Coccobact_Stationary <- subset_samples(Sven_phyto_rare_Stationary, Culture_type %in% c("BacterialControl", "CoccolithophoreBacteria"))

Control_DiatomOnly_Stationary <- subset_samples(Sven_phyto_rare_Stationary, Culture_type %in% c("BacterialControl", "DiatomOnly"))

Control_Diatombact_Stationary <- subset_samples(Sven_phyto_rare_Stationary, Culture_type %in% c("BacterialControl", "DiatomBacteria"))


#no hits after p adjusted so used just p value
Control_Coccoonly_Stationary_df<-Control_Coccoonly_Stationary %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

library(rstatix)
# do tests
KW_taxa_Control_Coccoonly_Stationary<-Control_Coccoonly_Stationary_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Coccobact_Stationary_df<-Control_Coccobact_Stationary %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Coccobact_Stationary<-Control_Coccobact_Stationary_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_DiatomOnly_Stationary_df<-Control_DiatomOnly_Stationary %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_DiatomOnly_Stationary<-Control_DiatomOnly_Stationary_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Diatombact_Stationary_df<-Control_Diatombact_Stationary %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Diatombact_Stationary<-Control_Diatombact_Stationary_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)


Control_Coccoonly_Death <- subset_samples(Sven_phyto_rare_Death, Culture_type %in% c("BacterialControl", "CoccolithophoreOnly"))

Control_Coccobact_Death <- subset_samples(Sven_phyto_rare_Death, Culture_type %in% c("BacterialControl", "CoccolithophoreBacteria"))

Control_DiatomOnly_Death <- subset_samples(Sven_phyto_rare_Death, Culture_type %in% c("BacterialControl", "DiatomOnly"))

Control_Diatombact_Death <- subset_samples(Sven_phyto_rare_Death, Culture_type %in% c("BacterialControl", "DiatomBacteria"))


#no hits after p adjusted so used just p value
Control_Coccoonly_Death_df<-Control_Coccoonly_Death %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

library(rstatix)
# do tests
KW_taxa_Control_Coccoonly_Death<-Control_Coccoonly_Death_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Coccobact_Death_df<-Control_Coccobact_Death %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Coccobact_Death<-Control_Coccobact_Death_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_DiatomOnly_Death_df<-Control_DiatomOnly_Death %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_DiatomOnly_Death<-Control_DiatomOnly_Death_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)

#no hits after p adjusted so used just p value
Control_Diatombact_Death_df<-Control_Diatombact_Death %>% 
  transform_sample_counts(function(x) {x/sum(x)} ) %>%
  psmelt() 

# do tests
KW_taxa_Control_Diatombact_Death<-Control_Diatombact_Death_df%>% 
  group_by(OTU) %>% 
  kruskal_test(Abundance ~ Culture_type) %>% 
  adjust_pvalue(method="fdr")%>% 
  filter(p < 0.05)


#combine significant taxa across all tests
KW_taxa_All <- rbind(KW_taxa_Control_Coccoonly_Lag,KW_taxa_Control_Coccobact_Lag, KW_taxa_Control_DiatomOnly_Lag, KW_taxa_Control_Diatombact_Lag,KW_taxa_Control_Coccoonly_Exponential,KW_taxa_Control_Coccobact_Exponential, KW_taxa_Control_DiatomOnly_Exponential, KW_taxa_Control_Diatombact_Exponential,KW_taxa_Control_Coccoonly_Stationary,KW_taxa_Control_Coccobact_Stationary, KW_taxa_Control_DiatomOnly_Stationary, KW_taxa_Control_Diatombact_Stationary,KW_taxa_Control_Coccoonly_Death,KW_taxa_Control_Coccobact_Death, KW_taxa_Control_DiatomOnly_Death, KW_taxa_Control_Diatombact_Death,KW_taxa_Control_Coccoonly_Death,KW_taxa_Control_Coccobact_Death, KW_taxa_Control_DiatomOnly_Death, KW_taxa_Control_Diatombact_Death)

#remove redundant names and subset ps object
KW_taxa_All_names<-unique(KW_taxa_All$OTU)

KW_SPR_clean2 = prune_taxa(KW_taxa_All_names, SPR_clean) 

KW_SPR_clean2 <- name_na_taxa(KW_SPR_clean2, na_label = "Unidentified <tax> (<rank>)")

#KW_SPR_clean2<-tax_fix(KW_SPR_clean2)


KW_SPR_clean2 %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Phylum", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))


KW_SPR_clean2 %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Order", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))




KW_SPR_clean2 %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Family", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))




KW_SPR_clean2 %>%
  ps_arrange(
    Cultivation_Stage = factor(Cultivation_Stage, levels = c("Death","Stationary","Exponential", "Lag")))%>%
  ps_mutate(
    Culture_type = factor(Culture_type, levels = c("CoccolithophoreOnly","DiatomOnly", "BacterialControl","CoccolithophoreBacteria" ,"DiatomBacteria"))) %>% 
  comp_barplot("Genus", n_taxa = 15, merge_other = FALSE, label = "Cultivation_Stage", sample_order = "asis") +
  facet_wrap(vars(Culture_type), scales = "free") + # scales = "free" is IMPORTANT!
  coord_flip() +
  theme(axis.ticks.y = element_blank(), strip.text = element_text(face = "bold"))
