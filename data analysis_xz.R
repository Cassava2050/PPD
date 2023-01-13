rm(list =ls())
library(readxl)
library(tidyverse)
library(lme4)
library(Matrix)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(lmerTest)
library(asreml)
library(openxlsx)
library(agriutilities)
source("utilities_tidy.R")
source("https://raw.githubusercontent.com/darizasu/work/master/scripts/ggCor.R") #ggCor function correlations



folder = "D:\\OneDrive - CGIAR\\Data Analysis\\PPD\\data_v2\\"

# create master list ----------------------------------------------------------
master_data = list()  # master list

# data load from Jorge --------------------------------------------------------
PPD <- read_excel(paste0(folder, "PPD-V4-English actualizado 1.xlsx"),
                  sheet = "Sheet2", na = ".") 

# become factor root, root slice and root color -------------------------------
PPD$Root = as.factor(PPD$Root)
PPD$Root_slice = as.factor(PPD$Root_slice)
PPD$Root_color = as.factor(PPD$Root_color)

# structure of data -----------------------------------------------------------
str(PPD)
dim(PPD)
# recode factors roots, slices, root color ------------------------------------

PPD <- PPD %>% mutate(Root = recode_factor(PPD$Root, `1` = "root_1",  `2` = "root_2",
                                           `3` = "root_3", `4` = "root_4",
                                           `5` = "root_5", `6` = "root_6",
                                           `7` = "root_7", `8` = "root_8",
                                           `9` = "root_9", `10` = "root_10"))

PPD <- PPD %>% mutate(Root_slice = recode_factor(PPD$Root_slice, `1` = "slice_1",
                                                 `2` = "slice_2", `3` = "slice_3",
                                                 `4` = "slice_4", `5` = "slice_5",
                                                 `6` = "slice_6", `7` = "slice_7"))

PPD <- PPD %>% mutate(Root_color = recode_factor(PPD$Root_color, `blanca` = "white",
                                                 `amarilla` = "yellow"))

# change name of lab trials into cassava base trials (# attached trial names equivalence table )

trial_names_cb <-
  tibble(cb = c("201280PDGN1_ciat",
              "201397PDGN1_ciat",
              "201398BCGN1_ciat",#
              "201497BCGN1_ovej", #
              "201494BCGN1_libe", #
              "201495PDGN1_ciat",
              "201496BCGN1_ciat", #
              "2015100BCGN1_ciat", #
              "201599PDGN1_ciat",
              "201678PDGN1_ciat",
              "201679PDGN1_ciat",
              "201680BCGN1_ciat", #
              "201681BCGN1_ciat", #
              "201728BCGN1_ciat",
              "201729PDGN1_ciat",
              "201780PDGN1_ciat",
              "201833BCGN1_ciat", #
              "201835PDGN1_ciat",
              "201920BCGN1_ciat", #
              "201923BCGN1_ciat", #
              "201922PDGN1_ciat",
              "201495PDGN1_ciat",
              "201599PDGN1_ciat",
              "201678PDGN1_ciat",
              "201680BCGN1_ciat", #
              "201679PDGN1_ciat"
  ), 
       trial_lab = c("CIAT 13-21_SEPTIEMBRE 2012", 
                     "CIAT14-12_OCTUBRE2013", 
                     "CIAT14-15_NOVIEMBRE2013", 
                     "OVEJASSUCRE-14-21_MARZO2014", 
                     "VILLAVICENCIO-META-15-01_FEBRERO2014", 
                     "CIAT15-35_OCTUBRE2014", 
                     "CIAT15-37_OCTUBRE2014", 
                     "CIAT16-34_AGOSTO2015", 
                     "CIAT16-35_AGOSTO2015", 
                     "CIAT17-45_AGOSTO2016", 
                     "CIAT17-46_AGOSTO2016", 
                     "CIAT17-48_AGOSTO2016", 
                     "CIAT17-63_NOVIEMBRE2016",
                     "CIAT18-33_JULIO2017",
                     "CIAT18-49_JULIO2017",
                     "CIAT18-51_JULIO2017",
                     "CIAT19-29_JULIO2018",
                     "CIAT19-42_JULIO2018",
                     "CIAT20-24_JULIO2019",
                     "CIAT20-26_AGOSTO2019",
                     "CIAT20-34_JULIO2019",
                     "CIAT15-44_OCTUBRE2014",
                     "CIAT16-46_AGOSTO2015",
                     "CIAT17-52_JULIO2016",
                     "CIAT17-53_JULIO2016",
                     "CIAT17-56_AGOSTO2016"))

trial_names_cb$trial_lab = gsub("-", "_", trial_names_cb$trial_lab)
trial_names_cb$trial_lab = gsub(" ", "", trial_names_cb$trial_lab)


# delete trials with PPD kinetics ---------------------------------------------

unique(PPD$Trial)
PPD %>% filter(str_detect(Trial, "13-37|14-13")) %>% count(Root, Root_slice) %>% 
  spread(Root_slice, n)
PPD %>% filter(str_detect(Trial, "13-37|14-13")) %>%
  select(Genotype) %>%
  distinct()

# high number of slice per root are referring to PPD kinetic trials -----------

trial_delete <- PPD %>% filter(str_detect(Trial, "13-37|14-13")) %>% pull(Trial)

PPD <- PPD %>% filter(!Trial %in% trial_delete)

# save the raw data --

master_data[["raw_data"]] = PPD


# counted slides per root -----------------------------------------------------
counted <- PPD %>% 
  count(Trial, Genotype, Plant_maturity, Root, Root_slice) %>% 
  spread(Root, n) %>% arrange(desc(root_1))

# save the count slice per root --

master_data[["count_slice"]] = counted


# summary by root color --

PPD_color <- PPD %>% group_by(Trial, Genotype, Plant_maturity, Root_color) %>% 
  summarise(PPD = mean(`PPD (0-10)`))


color <- PPD_color %>% ggplot(aes(x = PPD, fill = Root_color, color = Root_color)) +
  geom_histogram(binwidth = 1, alpha=0.3) +
  theme_xiaofei()
color

# ggsave(paste0(folder,"images/hist_color_", Sys.Date(),".png"), plot = color, 
# units = "in", dpi = 300, width = 8, height = 6)


# load data genotypes to be delete because low root size (data from Jorge)
# according to him, the genotypes belong to 13_21 lab trial

if(TRUE){ 
dele_geno <- read_excel(paste(folder,"Materiales a eliminar de ensayo 13-21.xlsx", sep=""),
                        sheet = "Sheet2")

# select 13_21 trial for selection size root
PPD_13_21 <- PPD %>% filter(str_detect(Trial, "13-21")) %>% ungroup()

PPD_13_21 <- PPD_13_21 %>% filter(!Genotype %in% dele_geno$Genotype) 


PPD <- PPD %>% ungroup() %>% filter(!str_detect(Trial, "13-21")) %>% 
  bind_rows(PPD_13_21)
}

# changing - by _ and delete spaces in trial_name
PPD$Trial = gsub("-", "_", PPD$Trial)
PPD$Trial = gsub(" ", "", PPD$Trial)


# summary by slices

PPD_slices <- PPD %>% group_by(Trial, Genotype, Plant_maturity, Root) %>% 
  summarise(PPD_mean_slice = mean(`PPD (0-10)`), PPD_sd_slice = sd(`PPD (0-10)`)) %>% 
  rename("Plant_maturity_months" = Plant_maturity)

#save means and sd by slices

master_data[["PPD_slices_mean"]] = PPD_slices




# clone mean

PPD$trial_v1 = gsub(".*_", "",PPD$Trial)
unique(PPD$trial_v1)
PPD$trial_v1 = gsub("JULIO2012", "2012Jul",PPD$trial_v1)
PPD$trial_v1 = gsub("SEPTIEMBRE2012", "2012Sep", PPD$trial_v1)
PPD$trial_v1 = gsub("OCTUBRE2013","2013Oct", PPD$trial_v1)
PPD$trial_v1 = gsub("NOVIEMBRE2013", "2013Nov", PPD$trial_v1)
PPD$trial_v1 = gsub("OCTUBRE2014" ,"2014Oct", PPD$trial_v1)
PPD$trial_v1 = gsub("AGOSTO2015", "2015Aug", PPD$trial_v1)
PPD$trial_v1 = gsub("AGOSTO2016","2016Aug", PPD$trial_v1)
PPD$trial_v1 = gsub("JULIO2016" , "2016Jul", PPD$trial_v1)
PPD$trial_v1 = gsub("NOVIEMBRE2016","2016Nov", PPD$trial_v1)
PPD$trial_v1 = gsub("JULIO2017", "2017Jul",PPD$trial_v1)
PPD$trial_v1 = gsub("JULIO2018", "2018Jul", PPD$trial_v1)
PPD$trial_v1 = gsub( "JULIO2019", "2019Jul", PPD$trial_v1)
PPD$trial_v1 = gsub("AGOSTO2019", "2019Aug", PPD$trial_v1)
PPD$trial_v1 = gsub("MARZO2014", "2014Mar", PPD$trial_v1)
PPD$trial_v1 = gsub("FEBRERO2014", "2014Feb", PPD$trial_v1)

unique(PPD$Trial_type)
PPD$trial_type_v1 = gsub("Carotenos", "ProVA", PPD$Trial_type)
PPD$trial_type_v1 = gsub("Segregantes", "Wild", PPD$trial_type_v1)

PPD$trial_name = paste(PPD$trial_v1, 
                       PPD$trial_type_v1,
                       PPD$Plant_maturity,
                       sep="_"
)
data.frame(table(PPD$trial_name))

# bind the data with the equivalence trial_names table 

PPD <- PPD %>% full_join(trial_names_cb, by = c("Trial" = "trial_lab"))

# delete the trial 13-14. This was not upload to cassava base
PPD <- PPD %>%
  filter(!Trial == "CIAT13_14_JULIO2012")

# give new name to new var with trial names from cassava base

PPD <- PPD %>% mutate(cb = paste(cb, Plant_maturity, sep="_")) %>% 
  select(-trial_name) %>% rename("trial_name" = cb) 


names(PPD)

# delete the lab checks

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# summary by trail, genotype, plant maturity

# delete the only trial with harvest at 9 months
PPD_means <- PPD %>% group_by(trial_name, Genotype) %>% 
  summarise(PPD_mean = mean(`PPD (0-10)`), PPD_sd = sd(`PPD (0-10)`)) %>%
  filter(trial_name != "201497BCGN1_ovej_9")


# delete checks not included in the field trial ---------------------------

# PER183, HMC1, COL22, AM206-5, CM523-7 -----------------------------------

geno_BC_final <- PPD_means %>% 
  filter(str_detect(trial_name,"BC"), 
         !Genotype %in% c("HMC1", "CM523-7", "COL22", "PER183", "AM206-5")) %>% 
  ungroup()

PPD_means <- PPD_means %>% 
  filter(!str_detect(trial_name,"BC")) %>% 
  ungroup() %>% 
  bind_rows(geno_BC_final)

PPD_means %>% 
  filter(str_detect(trial_name,"PD")) %>% 
  distinct(Genotype) 


# save tidy data

master_data[["PPD_tidy"]] = PPD_means

#save tidy data in .RData file
saveRDS(PPD_means, paste0(folder, "PPD_slides_tidy.rds"))
PPD_means <- readRDS(file = paste0(folder, "PPD_slides_tidy.rds"))

dim(PPD_means); head(PPD_means)



# variation data means across years and age of the harvest



PPD_boxplot <- PPD_means %>% 
  filter(str_detect(trial_name,"BC")) %>%
  ggplot(aes(x = trial_name, y = PPD_mean)) +
  geom_violin(trim = FALSE, fill="gray")+
  geom_boxplot(width = 0.2, trim = TRUE) +
  labs(x = NULL, y = "PPD (0-10)") +
  theme_xiaofei()
PPD_boxplot
ggsave(paste0("images/", "PPD_boxplot_means_ProVA",Sys.Date(), ".png"), 
       plot = PPD_boxplot, units = "in", dpi = 300, width = 8, height = 4)

PPD_means %>% 
  filter(str_detect(trial_name,"BC")) %>%
  select(trial_name) %>%
  ungroup() %>%
  count(trial_name) %>%
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=FALSE)




PPD_boxplot <- PPD_means %>% 
  filter(str_detect(trial_name,"PD")) %>%
  ggplot(aes(x = trial_name, y = PPD_mean)) +
  geom_violin(trim = T, fill="gray", width = 0.9)+
  geom_boxplot(width = 0.2, trim = TRUE) +
  labs(x = NULL, y = "PPD (0-10)", title = "Wild crossing") +
  theme_xiaofei() +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8),
        plot.title = element_text(color = "black"),
        axis.title.y = element_text(size = 9))
PPD_boxplot
ggsave(paste0("images/", "PPD_boxplot_means_Wild",Sys.Date(), ".png"), 
       plot = PPD_boxplot, units = "in", dpi = 300, width = 10, height = 3)


PPD_means %>% 
  filter(str_detect(trial_name,"PD")) %>%
  select(trial_name) %>%
  ungroup() %>%
  count(trial_name) %>%
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=FALSE)



# Number of shared information --------------------------------------------

if(FALSE) {
  # deleting some additional "_"
  
  PPD_means <- PPD_means %>% ungroup() %>% mutate(Trial = recode_factor(PPD_means$Trial, 
                                                                        OVEJASSUCRE_14_21_MARZO2014 = "OVEJASSUCRE14_21_MARZO2014",
                                                                        VILLAVICENCIO_META_15_01_FEBRERO2014 = "VILLAVICENCIOMETA15_01_FEBRERO2014"))
  
  
  # mutate year variable
  PPD_means <- PPD_means %>% mutate(trial_name = Trial) %>% 
    separate(Trial, c("col1", "col2", "year"), "_") %>% 
    select(-c("col1", "col2")) %>% 
    mutate(year = as.factor(year))
}


# shared information
var = "PPD_mean"
names(PPD_means)


# wild crosses ------------------------------------------------------------

MM <- 
  PPD_means %>% 
  filter(str_detect(trial_name,"PD")) %>%
  group_by(trial_name, Genotype) %>% 
  summarise(avg = mean(.data[[var]], na.rm = T)) %>% 
  pivot_wider(names_from = trial_name, values_from = avg)

g1 <-
  ggCor(MM, returnN = TRUE ) + 
  labs(title = "Shared information") + 
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = 'none',
        panel.grid.minor.x = element_blank(), panel.grid.major = element_blank())
g1
ggsave(paste0("images/", "shared_clones_Wild_", Sys.Date(), ".png"), 
       plot = g1, 
       units =  "in", 
       dpi = 300, width = 5, 
       height = 4)



# BC population -----------------------------------------------------------

MM <- 
  PPD_means %>% 
  filter(str_detect(trial_name,"BC")) %>%
  group_by(trial_name, Genotype) %>% 
  summarise(avg = mean(.data[[var]], na.rm = T)) %>% 
  pivot_wider(names_from = trial_name, values_from = avg)

g1 <-
  ggCor(MM, returnN = TRUE ) + 
  labs(title = "Shared information") + 
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = 'none',
        panel.grid.minor.x = element_blank(), panel.grid.major = element_blank())
g1
ggsave(paste0("images/", "shared_clones_ProVA", Sys.Date(), ".png"), 
       plot = g1, 
       units =  "in", 
       dpi = 300, width = 5, 
       height = 4)


if(FALSE){
  # check connectivity --------------------------------------------
  
  shared_clones <- checkConection(
    data = PPD_means, 
    genotype = "Genotype",
    trial = "trial_name",
    response = var, all = F
  )
  shared_clones
  
  
  master_data[["shared_clones"]] = shared_clones
  
}

# -------------------------------------------------------------------------------------


PPD_means <- PPD_means %>% 
  add_column(rep_number = 1) %>% 
  select(trial_name, rep_number,  everything())

# summary metadata -----------------------------------------------------------

population = "PD" 
# population = "BC" # -----------------------select one


resum <- PPD_means %>%
  filter(str_detect(trial_name,population)) %>%  #"PD
  group_by(trial_name) %>%
  summarise(n_gen = n_distinct(Genotype),
            n_reps = n_distinct(rep_number),
            n_total = n(),
            n_missing = sum(is.na(.data[[var]])),
            n_percent = n_missing/n_total,
            zeros = sum(.data[[var]]  == 0, na.rm = T),
            rcbd = ifelse(n_reps > 1, TRUE, FALSE),
            design = ifelse(n_reps == 1, "unrep", design)
  ) %>%
  type.convert() %>%
  arrange(n_gen) %>%
  print(n = Inf)


resum <- resum %>% as.data.frame() %>% mutate(variable = var)

# save the summary metainformation

master_data[[paste0("meta_data", "_", population)]] = resum







# -------------------------------------------------------------------------
# Modelling with factor analytic ------------------------------------------
# -------------------------------------------------------------------------
PPD_means$Genotype = as.factor(PPD_means$Genotype)
PPD_means$trial_name = as.factor(PPD_means$trial_name)

#pop = "PD"
 pop = "BC"   # -----------------------select one

PPD_means_pop = PPD_means%>%
  filter(str_detect(trial_name, pop)) 


# Summary statistics ------------------------------------------------------



table <- PPD_means_pop %>% 
  select(-c(PPD_sd, trial_name)) %>% 
  group_by(Genotype) %>% 
  summarise_all(list(median = median, mean = mean, min = min, max = max)) 
  
table %>% 
  mutate_at(vars(median, mean, min, max), funs(round(., 2))) %>% 
  unite("range", min:max, sep = "-") %>% 
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

Table_sumary <- data.frame(Median = PPD_means_pop$PPD_mean %>% median(), 
           Mean = PPD_means_pop$PPD_mean %>% mean(),
           Min = PPD_means_pop$PPD_mean %>% min(),
           Max = PPD_means_pop$PPD_mean %>% max())
           
Table_sumary %>% 
  mutate_at(vars(Median, Mean, Min, Max), funs(round(., 2))) %>% 
  unite("range", Min:Max, sep = "-", remove = T) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=TRUE)
  


# Random modeling ---------------------------------------------------------

PPD_means_pop = PPD_means_pop %>% mutate(trial_name = as.factor(trial_name),
                         Genotype = as.factor(Genotype))
model_MET_2stage <- asreml(
  fixed = PPD_mean ~ trial_name,
  random = ~ fa(trial_name, 2):Genotype,
  data = PPD_means_pop,
  family = asr_gaussian(dispersion = 1),
  na.action = list(x = "include", y = "include"),
  trace = 0
)

model_MET_2stage <- update.asreml(model_MET_2stage) 
model_MET_2stage <- update.asreml(model_MET_2stage) 

pred_f <- predict(model_MET_2stage , classify = "Genotype")
BLUPs <- pred_f$pvals %>% mutate(variable = var) %>% arrange(predicted.value) # BLUPs
BLUPs <- BLUPs %>% rename("BLUPs" = predicted.value, "seBLUPs" = std.error) %>% 
  select(-c(status))

summary(model_MET_2stage)$varcomp
varcomp(model_MET_2stage, "Genotype")

master_data[[paste("BLUPs", pop, sep="_")]] = BLUPs


# brief model----------------------------------------------------------------

eqt_fixed <- reformulate(c("trial_name"), response = "PPD_mean")

model_MET_2stage <- asreml(
  fixed = eqt_fixed,
  random = ~ Genotype + trial_name:Genotype,
  data = PPD_means_pop %>% droplevels(),
  family = asr_gaussian(dispersion = 1),
  na.action = list(x = "exclude", y = "exclude"),
  trace = 0)

model_MET_2stage <- update.asreml(model_MET_2stage) 
model_MET_2stage <- update.asreml(model_MET_2stage) 

summary(model_MET_2stage)$varcomp

if(FALSE) {
  # Fixed modeling
  eqt_fixed <- reformulate(c("Genotype"), response = var)
  
  model_MET_2stag_fe <- asreml(
    fixed = eqt_fixed,
    random = ~ fa(trial_name, 2):Genotype,
    data = PPD_means,
    family = asr_gaussian(dispersion = 1),
    na.action = list(x = "include", y = "include"),
    trace = 0
  )
  model_MET_2stag_fe <- update.asreml(model_MET_2stag_fe)
  model_MET_2stag_fe <- update.asreml(model_MET_2stag_fe)
  
  pred <- predict(model_MET_2stag_fe , classify = "Genotype")
  PP_f <- pred$pvals %>% mutate(variable = var) %>% arrange(predicted.value)
  PP_f <- PP_f %>% rename("BLUEs" = predicted.value, "seBLUEs" = std.error) %>%
    select(-c(status)) 
  
  master_data[["BLUEs"]] = PP_f
}



# heritability ------------------------------------------------------------

h2_fa2 <- agriutilities::heritability_fa(model_fa = model_MET_2stage, 
                                               genotype = "Genotype", 
                                               env = "trial_name", 
                                               vc.model = "fa2")$H2Cullis

names(h2_fa2) <- var

master_data[[paste("heritability", pop, sep="_")]] = h2_fa2



# combine BLUE and BLUP ---------------------------------------------------

# BLUE file
#head(bluesDF_kp)

bluesDF_kp_wide <- PPD_means_pop %>%
  ungroup() %>%
  select(trial_name, Genotype, PPD_mean)%>%
  pivot_wider(names_from = trial_name,
              values_from = PPD_mean  )

#View(bluesDF_kp_wide)

blue_blup = bluesDF_kp_wide %>%
  left_join(BLUPs, by="Genotype")


blue_blup_sort = blue_blup %>%
  arrange(BLUPs)

master_data[[paste("BLUP_BLUE", pop, sep="_")]] = blue_blup_sort # revisar que este por populations



# return from population selection -----------------------------------------






# save the results

experiment = "PPD_8_years"

meta_file_name = paste(folder, paste("01_",experiment, "_master_results_",
                                     Sys.Date(),"_v2.xlsx", sep = ""),
                       sep = "")
write.xlsx(master_data, file = meta_file_name) 







if(FALSE){
  

######################## use both BLUP and BLUE #####################################
#### correlation among locations in traits

library(GGally)
# set dplyr functions
select <- dplyr::select; rename <- dplyr::rename; mutate <- dplyr::mutate; 
summarize <- dplyr::summarize; arrange <- dplyr::arrange; 
slice <- dplyr::slice; filter <- dplyr::filter; recode<-dplyr::recode


blue_p_sort = read_excel(meta_file_name, sheet = "BLUP_BLUE_PD")
# data
names(blue_p_sort)
blue_p_sel =  blue_p_sort%>%
  select(-seBLUPs, -variable)

blue_p_sel$accession_name  = blue_p_sel$Genotype
blue_p_sel$check = blue_p_sel$accession_name 
checks = c( "PER183", "CM523-7")
blue_p_sel = blue_p_sel %>% mutate(check = ifelse(check %in%checks, "check", "br_clone"))

mycorrelations <- function(data,mapping,...){
  data2 = data
  data2$x = as.numeric(data[,as_label(mapping$x)])
  data2$y = as.numeric(data[,as_label(mapping$y)])
  data2$group = data[,as_label(mapping$colour)]
  
  correlation_df = data2 %>% 
    bind_rows(data2 %>% mutate(group="Overall Corr")) %>%
    group_by(group) %>% 
    filter(sum(!is.na(x),na.rm=T)>1) %>%
    filter(sum(!is.na(y),na.rm=T)>1) %>%
    summarize(estimate = round(as.numeric(cor.test(x,y,method="spearman")$estimate),2),
              pvalue = cor.test(x,y,method="spearman")$p.value,
              pvalue_star = as.character(symnum(pvalue, corr = FALSE, na = FALSE, 
                                                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                symbols = c("***", "**", "*", "'", " "))))%>%
    group_by() %>%
    mutate(group = factor(group, levels=c(as.character(unique(sort(data[,as_label(mapping$colour)]))), "Overall Corr")))
  
  ggplot(data=correlation_df, aes(x=1,y=group,color=group))+
    geom_text(aes(label=paste0(group,": ",estimate,pvalue_star)))
}

names(blue_p_sel)


col4cor = names(blue_p_sel)[str_detect(names(blue_p_sel), analysis_trait[i])]

data = blue_p_sel %>%
  select( -Genotype)
cor_mean = ggpairs(data,columns=1:(ncol(data)-1),
                   mapping = ggplot2::aes(color=check), 
                   upper = list(continuous = mycorrelations),
                   cardinality_threshold = 88)+
  scale_color_manual(values=c("check"="blue","br_clone"="brown","Overall Corr"="black"))

ggsave(paste0(folder,"images/mean_BLUP_cor__Wild", Sys.Date(),".png"), 
       plot = cor_mean, 
       units = "in", 
       dpi = 300, 
       width = 15, 
       height = 15)


}





##########################  only use BLUP   #######################
#### correlation among traits for BLUP

library(Hmisc)
library(corrplot)
library(ggcorrplot)

list_file = list.files(folder)

sel_file = list_file[str_detect(list_file, "_master_results_") &
                       str_detect(list_file, experiment)]

sel_file[3]


# names(blupDF)


par(mfrow=c(1,1))
blupDF_value = blupDF %>%
  select(-Genotype, -seBLUPs, -variable)

# pdf(paste(folder, "01_", experiment, "_trait_cor_Wild_",
#           Sys.Date(),".pdf", sep=""), width = 8, height = 8)

# see and fix the problem with correlation

col<- colorRampPalette(c("red","white","blue"))(40)


M<-rcorr(as.matrix(blupDF_value))

plot_ <- corrplot(M$r, 
         hc.order = TRUE, type = "lower",
         lab = TRUE, p.mat = M$P, insig = "blank")

ggsave(paste0(folder,"images/", Sys.Date(),".png"), 
       plot = plot_, units = "in", dpi = 300, width = 8, height = 6)

mtext(paste("PPD_Wild crossing"),line=-2,side=2)
dev.off()

?corrplot

##########################  only use BLUP   #######################
#### correlation among traits for BLUP

library(Hmisc)
library(corrplot)

list_file = list.files(folder)

sel_file = list_file[str_detect(list_file, "_master_results_") &
                       str_detect(list_file, experiment)]
sheet = "BLUP_BLUE_BC"
# sheet = "BLUP_BLUE_BC" # ---------------------- select one
blupDF = read_excel(paste(folder,
                          sel_file[3], sep=""),
                    sheet= sheet)

# names(blupDF)


par(mfrow=c(1,1))
blupDF_value = blupDF %>%
  select(-Genotype, -seBLUPs, -variable)

pdf(paste(folder, "01_", experiment, "_trait_cor_wild_crosses",
          Sys.Date(),".pdf", sep=""), width = 8, height = 8)

col<- colorRampPalette(c("red","white","blue"))(40)
M<-rcorr(as.matrix(blupDF_value))
corrplot(M$r, 
         type="upper",
         tl.col="black", 
         tl.cex=0.8,
         tl.srt=45, 
         col=col,
         addCoef.col = "black", 
         number.cex = .7, 
         p.mat = M$P, 
         insig = "blank",
         sig.level = 0.05)

mtext(paste("PPD_Wild crossing"),line=-2,side=2)
dev.off()
