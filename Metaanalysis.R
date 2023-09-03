#Load packages
library(gtools)
library(dplyr)
library(tidyr)
library(gtools)
library(dplyr)
library(tidyr)
library(metafor)
library(forestploter)
library(readxl)
library(ggplot2)
library(grid)

#Import 'empathyreview' dataset

# PART 1: GENERAL DATA WRANGLING ------------------------------------------

#remove Covidence ID and title columns 
empathyreview <- empathyreview[-c(1:2)]

#create new variable with year and author combined 
empathyreview$paper <- paste(empathyreview$author, empathyreview$year, sep = " ")

#remove author and year columns
empathyreview <- empathyreview[-c(1:2)]

#create id variable (unique id for each row)
empathyreview <- empathyreview %>%
  mutate(id = row_number())

#convert male and females reported as % into N
#younger adult age group
for (i in 1:length (empathyreview$id)) {
  if(empathyreview[i, "youngTYPE"] == "%" && !is.na(empathyreview[i, "youngTYPE"])) {
    if(!is.na(empathyreview[i, "youngSEXM"])) {
      empathyreview[i, "youngSEXM"] <- (empathyreview[i, "youngSEXM"]/100)*empathyreview[i, "youngN"]
      empathyreview[i, "youngTYPE"] <- "N"
      empathyreview[i, "notes"] <- empathyreview[i, "youngTYPE"]
    }
    if(is.na(empathyreview[i, "youngSEXM"])&& !is.na(empathyreview[i, "youngSEXF"])){
      empathyreview[i, "youngSEXM"] <- empathyreview[i, "youngN"] -empathyreview[i, "youngSEXF"]
      empathyreview[i, "youngTYPE"] <- "N"
    }
  }
  if(empathyreview[i, "youngTYPE"] == "N" && !is.na(empathyreview[i, "youngTYPE"])){
    if(is.na(empathyreview[i, "youngSEXM"]) && !is.na(empathyreview[i, "youngSEXF"])){
      empathyreview[i, "youngSEXM"] <- empathyreview[i, "youngN"] - empathyreview[i, "youngSEXF"]
    }
  }
}

for (i in 1:length(empathyreview$id)) {
  empathyreview[i, "youngSEXF"] <- empathyreview[i, "youngN"]-empathyreview[i, "youngSEXM"]
  
}

#older adult age group 
for (i in 1:length (empathyreview$id)) {
  if(empathyreview[i, "oldTYPE"] == "%" && !is.na(empathyreview[i, "oldTYPE"])) {
    if(!is.na(empathyreview[i, "oldSEXM"])) {
      empathyreview[i, "oldSEXM"] <- (empathyreview[i, "oldSEXM"]/100)*empathyreview[i, "oldN"]
      empathyreview[i, "oldTYPE"] <- "N"
      empathyreview[i, "notes"] <- empathyreview[i, "oldTYPE"]
    }
    if(is.na(empathyreview[i, "oldSEXM"])&& !is.na(empathyreview[i, "oldSEXF"])){
      empathyreview[i, "oldSEXM"] <- empathyreview[i, "oldN"] -empathyreview[i, "oldSEXF"]
      empathyreview[i, "oldTYPE"] <- "N"
    }
  }
  if(empathyreview[i, "oldTYPE"] == "N" && !is.na(empathyreview[i, "oldTYPE"])){
    if(is.na(empathyreview[i, "oldSEXM"]) && !is.na(empathyreview[i, "oldSEXF"])){
      empathyreview[i, "oldSEXM"] <- empathyreview[i, "oldN"] - empathyreview[i, "oldSEXF"]
    }
  }
}

for (i in 1:length(empathyreview$id)) {
  empathyreview[i, "oldSEXF"] <- empathyreview[i, "oldN"]-empathyreview[i, "oldSEXM"]
  
}

#middle age group
for (i in 1:length (empathyreview$id)) {
  if(empathyreview[i, "middleTYPE"] == "%" && !is.na(empathyreview[i, "middleTYPE"])) {
    if(!is.na(empathyreview[i, "middleSEXM"])) {
      empathyreview[i, "middleSEXM"] <- (empathyreview[i, "middleSEXM"]/100)*empathyreview[i, "middleN"]
      empathyreview[i, "middleTYPE"] <- "N"
      empathyreview[i, "notes"] <- empathyreview[i, "middleTYPE"]
    }
    if(is.na(empathyreview[i, "middleSEXM"])&& !is.na(empathyreview[i, "middleSEXF"])){
      empathyreview[i, "middleSEXM"] <- empathyreview[i, "middleN"] -empathyreview[i, "middleSEXF"]
      empathyreview[i, "middleTYPE"] <- "N"
    }
  }
  if(empathyreview[i, "middleTYPE"] == "N" && !is.na(empathyreview[i, "middleTYPE"])){
    if(is.na(empathyreview[i, "middleSEXM"]) && !is.na(empathyreview[i, "middleSEXF"])){
      empathyreview[i, "middleSEXM"] <- empathyreview[i, "middleN"] - empathyreview[i, "middleSEXF"]
    }
  }
}

for (i in 1:length(empathyreview$id)) {
  empathyreview[i, "middleSEXF"] <- empathyreview[i, "middleN"]-empathyreview[i, "middleSEXM"]
  
}


#total sample male and females
for (i in 1:length (empathyreview$id)) {
  if(empathyreview[i, "sampleTYPE"] == "%" && !is.na(empathyreview[i, "sampleTYPE"])) {
    if(!is.na(empathyreview[i, "sampleSEXM"])) {
      empathyreview[i, "sampleSEXM"] <- (empathyreview[i, "sampleSEXM"]/100)*empathyreview[i, "sampleN"]
      empathyreview[i, "sampleTYPE"] <- "N"
      empathyreview[i, "notes"] <- empathyreview[i, "sampleTYPE"]
    }
    if(is.na(empathyreview[i, "sampleSEXM"])&& !is.na(empathyreview[i, "sampleSEXF"])){
      empathyreview[i, "sampleSEXM"] <- empathyreview[i, "sampleN"] -empathyreview[i, "sampleSEXF"]
      empathyreview[i, "sampleTYPE"] <- "N"
    }
  }
  if(empathyreview[i, "sampleTYPE"] == "N" && !is.na(empathyreview[i, "sampleTYPE"])){
    if(is.na(empathyreview[i, "sampleSEXM"]) && !is.na(empathyreview[i, "sampleSEXF"])){
      empathyreview[i, "sampleSEXM"] <- empathyreview[i, "sampleN"] - empathyreview[i, "sampleSEXF"]
    }
  }
}

for (i in 1:length(empathyreview$id)) {
  empathyreview[i, "sampleSEXF"] <- empathyreview[i, "sampleN"]-empathyreview[i, "sampleSEXM"]
  
}

#see if any numbers don't match up (should be 0 obs.)
check_young_sex <- empathyreview %>%
  mutate(sum_youngN = youngSEXM + youngSEXF) %>%
  select(id,youngSEXM,youngSEXF,youngN,sum_youngN,notes,youngTYPE) %>%
  mutate(diff = youngN - sum_youngN) %>%
  filter(diff !=0)

check_old_sex <- empathyreview %>%
  mutate(sum_oldN = oldSEXM + oldSEXF) %>%
  select(id,oldSEXM,oldSEXF,oldN,sum_oldN,notes,oldTYPE) %>%
  mutate(diff = oldN - sum_oldN) %>%
  filter(diff !=0)

check_middle_sex <- empathyreview %>%
  mutate(sum_middleN = middleSEXM + middleSEXF) %>%
  select(id,middleSEXM,middleSEXF,middleN,sum_middleN,notes,middleTYPE) %>%
  mutate(diff = middleN - sum_middleN) %>%
  filter(diff !=0)

check_sample_sex <- empathyreview %>%
  mutate(sum_sampleN = sampleSEXM + sampleSEXF) %>%
  select(id,sampleSEXM,sampleSEXF,sampleN,sum_sampleN,notes,sampleTYPE) %>%
  mutate(diff = sampleN - sum_sampleN) %>%
  filter(diff !=0)

#### create three separate data frames for correlation, mean, and misc data 
cor_data <- empathyreview %>%
  slice(3, 19, 32:33, 37, 39, 41)

mean_data <- empathyreview %>%
  slice(1:2, 4:18, 20:21, 24:31, 34:36, 38, 40, 42:43)

misc_data <- empathyreview %>%
  slice(22, 23)



# PART 2: MEAN DATA WRANGLING ---------------------------------------------

#remove other age group data and non-descriptive statistics
mean_data <- mean_data %>%
  select(c(1:22, 30:43, 52:60, 69:77, 85:92, 101:102))
View(mean_data)

#convert all emotional empathy measure variables to character ready for pivoting data
mean_data[,28:62]<-sapply(mean_data[,28:62], as.character)

#Convert all emotional empathy measure variables into long format
mean_data <- mean_data %>%
  pivot_longer(
    !c(1:27, 63:64), 
    names_to = c("EE_measure", "descriptive"),
    names_sep = "_")

#Convert emotional empathy measure descriptives back into wide format 
mean_data <- mean_data %>%
  pivot_wider(
    names_from = c(descriptive),
    values_from = value
  )

#Re-order columns so ID and papers are first variables
mean_data <- mean_data[, c(28:29, 1:27, 30:39)]

#Convert numeric variables back into numeric
mean_data[,34:39]<-sapply(mean_data[,34:39], as.numeric)

#Re-name newly formed wide columns
colnames(mean_data)[30] <- "Mno"
colnames(mean_data)[31] <- "Mname"
colnames(mean_data)[32] <- "Mnotes"
colnames(mean_data)[33] <- "Mmodality"
colnames(mean_data)[34] <- "Mmean_young"
colnames(mean_data)[35] <- "Msd_young"
colnames(mean_data)[36] <- "Mmean_old"
colnames(mean_data)[37] <- "Msd_old"
colnames(mean_data)[38] <- "Mmean_middle"
colnames(mean_data)[39] <- "Msd_middle"

#Create combined N for young and middle age groups
mean_data$youngmiddleN <- rowSums(mean_data[, c("youngN", "middleN")], na.rm = FALSE)

#create new id variable for each row ready for nested for statements
mean_data <- mean_data %>%
  mutate(ID = row_number())

#Create merged youngmidleN column
for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "youngN"])) {
    if(is.na(mean_data[i, "youngmiddleN"])) {
      mean_data[i, "youngmiddleN"] <- mean_data[i, "youngN"]
      mean_data[i, "notes"] <- mean_data[i, "youngN"]
    }
  }
}

for (i in 1:length(mean_data$ID)) {
  if(!is.na(mean_data[i, "middleN"])) {
    if(is.na(mean_data[i, "youngmiddleN"])) {
      mean_data[i, "youngmiddleN"] <- mean_data[i, "middleN"]
      mean_data[i, "notes2"] <- mean_data[i, "middleN"]
    }
  }
  
}

#remove ID, notes, and notes2 columns which are no longer needed
mean_data <- select(mean_data, -c(notes, notes2))

#Create combined female for young and middle age groups
mean_data$youngmiddleSEXF <- rowSums(mean_data[, c("youngSEXF", "middleSEXF")], na.rm = FALSE)

#Create merged youngmidleSEXF column
for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "youngSEXF"])) {
    if(is.na(mean_data[i, "youngmiddleSEXF"])) {
      mean_data[i, "youngmiddleSEXF"] <- mean_data[i, "youngSEXF"]
      mean_data[i, "notes"] <- mean_data[i, "youngSEXF"]
    }
  }
}

for (i in 1:length(mean_data$ID)) {
  if(!is.na(mean_data[i, "middleSEXF"])) {
    if(is.na(mean_data[i, "youngmiddleSEXF"])) {
      mean_data[i, "youngmiddleSEXF"] <- mean_data[i, "middleSEXF"]
      mean_data[i, "notes2"] <- mean_data[i, "middleSEXF"]
    }
  }
  
}

#remove ID, notes, and notes2 columns which are no longer needed
mean_data <- select(mean_data, -c(notes, notes2))

#Create pooled mean and SD using function from below link
#https://stackoverflow.com/questions/9222056/existing-function-to-combine-standard-deviations-in-r

grand.sd <- function(S, M, N) {sqrt(weighted.mean(S^2 + M^2, N) -
                                      weighted.mean(M, N)^2)}

for (i in 1:length(mean_data$ID)) {
  if(!is.na(mean_data[i, "Msd_middle"])){
    if(!is.na(mean_data[i, "Msd_young"])){
      mean <- c(mean_data[[i, "Mmean_young"]], mean_data[[i, "Mmean_middle"]])
      n <- c(mean_data[[i, "youngN"]], mean_data[[i, "middleN"]])
      sd <- c(mean_data[[i, "Msd_young"]], mean_data[[i, "Msd_middle"]])
      
      mean_data[i, "Msd_yandm"] <- grand.sd(sd,mean,n)
      mean_data[i, "Mmean_yandm"] <- weighted.mean(mean,n)
    }
  }
}

for (i in 1:length(mean_data$ID)) {
  if(!is.na(mean_data[i, "middleSD"])){
    if(!is.na(mean_data[i, "youngSD"])) {
      mean <- c(mean_data[[i, "youngM"]], mean_data[[i, "middleM"]])
      n <- c(mean_data[[i, "youngN"]], mean_data[[i, "middleN"]])
      sd <- c(mean_data[[i, "youngSD"]], mean_data[[i, "middleSD"]])
      
      mean_data[i, "youngmiddleSD"] <- grand.sd(sd,mean,n)
      mean_data[i, "youngmiddleM"] <- weighted.mean(mean,n)
    }
  }
}


#Merge pooled age group descriptive values with existing younger only values
for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "youngSD"])) {
    if(is.na(mean_data[i, "youngmiddleSD"])) {
      mean_data[i, "youngmiddleSD"] <- mean_data[i, "youngSD"]
      mean_data[i, "notes"] <- mean_data[i, "youngSD"]
    }
  }
}


for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "youngM"])) {
    if(is.na(mean_data[i, "youngmiddleM"])) {
      mean_data[i, "youngmiddleM"] <- mean_data[i, "youngM"]
      mean_data[i, "notes2"] <- mean_data[i, "youngM"]
    }
  }
}

#remove notes and notes2 columns which are no longer needed
mean_data <- select(mean_data, -c(notes, notes2))

#Merge pooled age group descriptive values with existing middle only values 
for (i in 1:length(mean_data$ID)) {
  if(!is.na(mean_data[i, "middleSD"])) {
    if(is.na(mean_data[i, "youngmiddleSD"])) {
      mean_data[i, "youngmiddleSD"] <- mean_data[i, "middleSD"]
      mean_data[i, "notes"] <- mean_data[i, "middleSD"]
    }
  }
  
}

for (i in 1:length(mean_data$ID)) {
  if(!is.na(mean_data[i, "middleM"])) {
    if(is.na(mean_data[i, "youngmiddleM"])) {
      mean_data[i, "youngmiddleM"] <- mean_data[i, "middleM"]
      mean_data[i, "notes2"] <- mean_data[i, "middleM"]
    }
  }
  
}

#remove ID, notes, and notes2 columns which are no longer needed
mean_data <- select(mean_data, -c(notes, notes2))


#Merge pooled measure descriptive values  with existing younger only values 
for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "Msd_young"])) {
    if(is.na(mean_data[i, "Msd_yandm"])) {
      mean_data[i, "Msd_yandm"] <- mean_data[i, "Msd_young"]
      mean_data[i, "notes"] <- mean_data[i, "Msd_young"]
    }
  }
}


for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "Mmean_young"])) {
    if(is.na(mean_data[i, "Mmean_yandm"])) {
      mean_data[i, "Mmean_yandm"] <- mean_data[i, "Mmean_young"]
      mean_data[i, "notes2"] <- mean_data[i, "Mmean_young"]
    }
  }
}


#Merge pooled measure descriptive values with existing middle only values
for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "Msd_middle"])) {
    if(is.na(mean_data[i, "Msd_yandm"])) {
      mean_data[i, "Msd_yandm"] <- mean_data[i, "Msd_middle"]
      mean_data[i, "notes3"] <- mean_data[i, "Msd_middle"]
    }
  }
}


for (i in 1:length(mean_data$ID)){
  if(!is.na(mean_data[i, "Mmean_middle"])) {
    if(is.na(mean_data[i, "Mmean_yandm"])) {
      mean_data[i, "Mmean_yandm"] <- mean_data[i, "Mmean_middle"]
      mean_data[i, "notes4"] <- mean_data[i, "Mmean_middle"]
    }
  }
}

#remove ID and notes columns which are no longer needed
mean_data <- select(mean_data, -c(ID, notes, notes2, notes3, notes4))


#Find total number of females examined across all age groups (needed for subgroup analysis)
for (i in 1:length (mean_data$id)) {
  if(mean_data[i, "sampleTYPE"] == "N" && !is.na(mean_data[i, "sampleTYPE"])) {
    if(!is.na(mean_data[i, "sampleSEXF"])) {
      mean_data[i, "samplePF"] <- (mean_data[i, "sampleSEXF"]/mean_data[i, "sampleN"])*100
      mean_data[i, "sampleTYPE"] <- "%"
      mean_data[i, "notes"] <- mean_data[i, "sampleTYPE"]
    }
  }
}

for (i in 1:length(mean_data$id)) {
  mean_data[i, "samplePM"] <- 100-mean_data[i, "samplePF"]
  
}

#remove notes column which is no longer needed
mean_data <- select(mean_data, -c(notes))


# PART 3: MEAN META-ANALYSIS ----------------------------------------------

#Calculate effect sizes
es <- escalc(measure = "SMD",
             m1i = Mmean_yandm,
             sd1i = Msd_yandm,
             n1i = youngmiddleN,
             m2i = Mmean_old,
             sd2i = Msd_old,
             n2i = oldN,
             data = mean_data) %>%
  filter(!is.na(yi))%>%
  group_by(paper) %>%
  mutate(group_id = cur_group_id())

#Remove outlier
es1 <- es[es$Mmean_old != 5.380, ]

#Reverse order of effect sizes to make interpretation easier
es1$yi <- -es1$yi

#Convert mean values to 2 decimal places
es1$Mmean_yandm <- formatC(es1$Mmean_yandm, digits = 2, format = "f")

#Run multivariate meta-analysis
mvmeta <- rma.mv(data=es1,yi,vi,random= ~1 |group_id)




# PART 3: SUB-GROUP ANALYSES ----------------------------------------------

#Gender meta-regression
genderreg <- rma(yi, vi, data = es1, mods = ~ samplePF, samplePM)

#Measure type meta-regression
#Create separate meta-analyses for state versus trait measures
mod1 <- rma.mv(data=es1, yi,vi,random= ~1 |group_id, subset=Mmodality=="Questionnaire")
mod2 <- rma.mv(data=es1, yi,vi,random= ~1 |group_id, subset=Mmodality=="Video")

#Create funnel plots
funnel(mod1, main ="Trait tasks")
funnel(mod2, main ="State tasks")

#Combine meta-analyses into single data frame 
mod_comb <- data.frame(estimate = c(coef(mod1), coef(mod2)), stderror = c(mod1$se, mod2$se),
                       meta = c("State", "Trait"), sigma2 = round(c(mod1$sigma2, mod2$sigma2),3))

#Run meta-regression
mod_meta <- rma(estimate, sei=stderror, mods = ~ meta, method = "FE", data = mod_comb, digits = 3)


#Measure name meta-regression
#Categorise names into 4 groups 
for (i in 1:length(es1$Mname)) {
  if(!is.na(es1$Mname[i])){
    if(es1$Mname[i] != "IRI EC"){
      if(es1$Mname[i] != "IRI PD"){
        if(es1$Mname[i]!='Emotional Congruence'){
          es1$Mname[i] = 'Other'
        }
      }
    }
  }
  
}

#Create separate meta-analyses for each measure name group
name1 <- rma.mv(data=es1, yi,vi,random= ~1 |group_id, subset=Mname=="IRI EC")
name2 <- rma.mv(data=es1, yi,vi,random= ~1 |group_id, subset=Mname=="IRI PD")
name3 <- rma.mv(data=es1, yi,vi,random= ~1 |group_id, subset=Mname=="Emotional Congruence")
name4 <- rma.mv(data=es1, yi,vi,random= ~1 |group_id, subset=Mname=="Other")

#Create funnel plots
funnel(name1, main ="IRI Empathic Concern")
funnel(name2, main ="IRI Personal Distress")
funnel(name3, main ="Emotional Congruence")
funnel(name4, main ="Other Measures")

#Combine meta-analyses into single data frame 
name_comb <- data.frame(estimate = c(coef(name1), coef(name2), coef(name3), coef(name4)), 
                        stderror = c(name1$se, name2$se, name3$se, name4$se),
                        meta = c("IRI EC", "IRI PD", "Emotional Congruence", "Other"), 
                        sigma2 = round(c(name1$sigma2, name2$sigma2, name3$sigma2, name4$sigma2),5))

#Run meta-regression
name_meta <- rma(estimate, sei=stderror, mods = ~ meta, method = "FE", data = name_comb, digits = 5)




# PART 4: MEAN PLOTS ------------------------------------------------------

#Forest plot
#Save as tiff image
tiff(filename = "forest.tiff", width = 18, height = 16, units = "in", res = 600, compression = "lzw")

#Create forest plot
metafor::forest(mvmeta, slab=paste(paper), xlim=c(-2.8, 2.2), alim = c(-1.5, 2),
                ilab=cbind(Mname), ilab.xpos=c(-1.8), xlab="Hedges' (standardised) g",
                cex=.8, header = "Author Year", mlab="", order = yi)
op <- par(cex=.8, font=2)
text(c(-1.8), 61, c("Measure"))
par(op)

#Add text with k and p for overall model results
text(-2.8, -1, pos=4, cex=0.75, bquote(paste("Model (K = 34, N = ", .(formatC(mvmeta$k, digits=2, format="f")), 
                                             ", p = ", .(formatC(mvmeta$pval, digits=3, format="f")), ")")))

#Add text underneath for heterogeneity
text(-2.8, -2, pos=4, cex=0.75, bquote(paste("Heterogeneity estimate (p <.001, sigma^2 = ", 
                                             .(formatC(mvmeta$sigma2, digits=2, format="f")), ", I^2 = 0.85%)")))

dev.off() 


#Sub-groups
#Save as tiff
tiff(filename = "subgroups.tiff", width = 10, height = 3, units = "in", res = 600, compression = "lzw")

#Heading
mm.head <- c("Measure type", "", "", "", "", "", "", "", round(mod_meta$QM,2), round(mod_meta$QMp,3))
dim(mm.head) <- c(1,10)

#Measure type
mm1 <- c("Trait tasks", paste("27"), mod1$k[1], mod1$beta[1], mod1$ci.lb[1], mod1$ci.ub[1], mod1$se[1], 
         round(mod1$pval,3)[1], "", "")
dim(mm1) <- c(1,10)
mm2 <- c("State tasks", paste("11"), mod2$k, mod2$beta, mod2$ci.lb, mod2$ci.ub, mod2$se, round(mod2$pval,3), "", "")
dim(mm2) <- c(1,10)

#Measure name
mn.head <- c("Measure name", "", "","", "", "", "", "", round(name_meta$QM,2), paste("<.001"))
dim(mn.head) <- c(1,10)
mn1 <- c("IRI Empathic Concern", paste("19"), name1$k[1], name1$beta[1], name1$ci.lb[1], name1$ci.ub[1], 
         name1$se[1], round(name1$pval,3)[1], "", "")
dim(mn1) <- c(1,10)
mn2 <- c("IRI Personal Distress", paste("14"), name2$k, name2$beta, name2$ci.lb, name2$ci.ub, name2$se, round(name2$pval,3), "", "")
dim(mn2) <- c(1,10)
mn3 <- c("Emotional Congruence", paste("3"), name3$k, name3$beta, name3$ci.lb, name3$ci.ub, name3$se, round(name3$pval,3), "", "")
dim(mn3) <- c(1,10)
mn4 <- c("All Other Measures", paste("14"), name4$k, name4$beta, name4$ci.lb, name4$ci.ub, name4$se, paste("<.001"), "", "")
dim(mn4) <- c(1,10)


#Combine effect size estimates for the data frame
ES_domains_mx <- rbind(mm.head,mm1,mm2,mn.head,mn1,mn2,mn3,mn4)
ES_domains_df <- as.data.frame(ES_domains_mx)

#Re-name columns
names(ES_domains_df)[1] <- "Subgroup"
names(ES_domains_df)[2] <- "K studies"
names(ES_domains_df)[3] <- "N measures"
names(ES_domains_df)[4] <- "beta"
names(ES_domains_df)[5] <- "low"
names(ES_domains_df)[6] <- "hi"
names(ES_domains_df)[7] <- "se"
names(ES_domains_df)[8] <- "p"
names(ES_domains_df)[9] <- "QM"
names(ES_domains_df)[10] <- "(p)"

#convert to numeric
ES_domains_df$beta <- as.numeric(ES_domains_df$beta)
ES_domains_df$low <- as.numeric(ES_domains_df$low)
ES_domains_df$hi <- as.numeric(ES_domains_df$hi)
ES_domains_df$se <- as.numeric(ES_domains_df$se)

#add indent for subgroups
ES_domains_df$Subgroup <- ifelse(ES_domains_df$`N measures` == "",
                                 ES_domains_df$Subgroup,
                                 paste0("   ",ES_domains_df$Subgroup))


#add blank column for forest plot to display CI
ES_domains_df$` ` <- paste(rep(" ",20),collapse = " ")

#create confidence interval column to display
ES_domains_df$`Hedges' g (95% CI)` <- ifelse(is.na(ES_domains_df$se),"",
                                             sprintf("%.2f (%.2f to %.2f)",
                                                     ES_domains_df$beta, ES_domains_df$low, ES_domains_df$hi))

#ensure order is correct
ES_domains_df <-ES_domains_df %>%
  select(Subgroup, `K studies`,`N measures`,beta,low,hi,se,` `,`Hedges' g (95% CI)`, p, QM,`(p)`)

#Add forestplot 
p <- forestploter::forest(ES_domains_df[,c(1:3,8:12)],
                          est = ES_domains_df$beta,
                          lower = ES_domains_df$low,
                          upper = ES_domains_df$hi,
                          sizes = ES_domains_df$se,
                          ci_column = 4,
                          ref_line = 0,
                          xlim = c(-0.5,0.5),
)

plot(p)

#Add italics
newp <- forestploter::edit_plot(p,
                                row = c(1, 4), 
                                col = 1, 
                                gp = gpar(fontface = "italic"))

plot(newp)

dev.off() 




# PART 5: COR DATA WRANGLING ----------------------------------------------

#Remove categorical age group data 
cor_data <- cor_data %>%
  select(c(1, 23:37, 44:54, 61:71, 78:86, 93:102))

#Re-order columns so ID and papers are first variables
cor_data <- cor_data[, c(56:57, 1:55)]

#convert all variables to character ready for pivoting data
cor_data[,16:57]<-sapply(cor_data[,16:57], as.character)

#Convert each type of emotional empathy measure into long format
cor_data <- cor_data %>%
  pivot_longer(
    !c(1:15), 
    names_to = c("EE_measure", "descriptive"),
    names_sep = "_")

#Convert descriptive variable back into wide format 
cor_data <- cor_data %>%
  pivot_wider(
    names_from = c(descriptive),
    values_from = value
  )

#Convert numeric variables back into numeric
cor_data[,22]<-sapply(cor_data[,22], as.numeric)

#Calculate descriptive statistics for all included studies 
mean(cor_data$otherM, na.rm = TRUE)
sum(cor_data$sampleN, na.rm = TRUE)
sum(cor_data$sampleSEXF, na.rm = TRUE)




# PART 6: COR META-ANALYSIS AND PLOT -------------------------------------

#Calculate effect sizes
esCOR <- escalc(measure = "ZCOR",
                ri = value,
                ni = sampleN,
                data = cor_data) %>%
  filter(!is.na(yi))%>%
  group_by(id)

#Run meta-analysis (multivariate)
metaCOR <- rma.mv(data=esCOR,yi,vi,random= ~1 |id)

#Save as tiff image
tiff(filename = "forestCOR.tiff", width = 14, height = 8, units = "in", res = 600, compression = "lzw")

#Create forest plot
metafor::forest(metaCOR, slab=paste(paper), xlim=c(-0.7, 1), alim = c(-0.25, 0.5),
                ilab=cbind(name), ilab.xpos=c(-0.3),
                cex=.8, header = "Author Year", mlab="", order = yi)
op <- par(cex=.8, font=2)
text(c(-0.3), 11, c("Measure"))
par(op)

#Add text with k and p for overall model results
text(-0.7, -1, pos=4, cex=0.75, bquote(paste("Model (K = 7, N = ", .(formatC(metaCOR$k, digits=0, format="f")), ", p = ", .(formatC(metaCOR$pval, digits=3, format="f")), ")")))

#Add text underneath for heterogeneity
text(-0.7, -1.5, pos=4, cex=0.75, bquote(paste("Heterogeneity estimate (p <.001, sigma^2 = ", .(formatC(metaCOR$sigma2, digits=2, format="f")), ", I^2 = 96%)")))

dev.off() 



# PART 7: PUBLICATION BIAS ------------------------------------------------

#Funnel plots
funnel(mvmeta)
funnel(metaCOR)

#I2 statistics
(mvmeta$QE - (mvmeta$k - 1)) / mvmeta$QE
(metaCOR$QE - (metaCOR$k - 1)) / metaCOR$QE

#Create univariate models for Eggers test
#Calculate avergae effect size within each study for mean data 
avg_es <- es %>%
  group_by(group_id) %>% 
  mutate(yi = mean(yi,na.rm=T),
         vi = mean(vi,na.rm=T),
         Mmean_yandm = mean(Mmean_yandm, na.rm=T),
         Mmean_old = mean(Mmean_old, na.rm=T),
         youngmiddleN = mean(youngmiddleN, na.rm=T),
         oldN = mean(oldN, na.rm=T))

#Run meta-analysis
uma <- rma(yi = yi, vi = vi, data = avg_es, method = "PM", test = "knha")

#Eggers test 
regtest(uma, model = "rma", predictor = "sei")

#Calculate average effect size within each study for cor studies
avg_COR <- esCOR %>%
  group_by(id) %>% 
  mutate(ri = value,
         ni = sampleN)

#Run meta-analysis
uniCOR <- rma(data=avg_COR, ri, ni, method = "PM", test = "knha")

#Eggers test 
regtest(uniCOR, model = "rma", predictor = "sei")



# PART 8: RISK OF BIAS FIGURE ---------------------------------------------

#Load 'robFigure' dataframe

#Set the levels so ggplot can't automatically change them
robFigure$Rating <- as.character(robFigure$Rating)
robFigure$Rating <- factor(robFigure$Rating, levels=unique(robFigure$Rating))

#Create the figure
tiff(filename = "rob.tiff", width = 8, height = 2, units = "in", res = 300, compression = "lzw")
ggplot(robFigure, aes(fill=Rating, y=Count, x=factor(Domain, level = c("Other bias", "Selective reporting bias", "Detection bias", "Performance bias", "Confounding bias", "Selection bias" )))) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  scale_y_continuous(labels = c("0%", "25%","50%","75%","100%")) +
  coord_flip() +
  scale_fill_manual(name = element_blank(), values = c("#d9d9d9", "#999999", "#525252", "#000000", "#FFFFFF")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black"))
dev.off()



# PART 9: SENSITIVITY ANALYSES --------------------------------------------

#Meta-analysis for younger versus older adults
YO_es <- escalc(measure = "SMD",
              m1i = Mmean_young,
              sd1i = Msd_young,
              n1i = youngN,
              m2i = Mmean_old,
              sd2i = Msd_old,
              n2i = oldN,
              data = mean_data) %>%
  filter(!is.na(yi))%>%
  group_by(paper) %>%
  mutate(group_id = cur_group_id())

#Remove outlier
YO_es1 <- YO_es[YO_es$Mmean_old != 5.380, ]

#Reverse order of effect sizes to make interpretation easier
YO_es1$yi <- -YO_es1$yi

#Run meta-analysis
yando_meta <- rma.mv(data=YO_es1, yi,vi,random= ~1 |group_id)

#Create forest plot
tiff(filename = "yando.tiff", width = 18, height = 16, units = "in", res = 600, compression = "lzw")

metafor::forest(yando_meta, slab=paste(paper), xlim=c(-2.8, 2.2), alim = c(-2, 2),
                ilab=cbind(Mname), ilab.xpos=c(-1.8), xlab="Hedges' (standardised) g",
                cex=.8, header = "Author Year", mlab="", order = yi)
op <- par(cex=.8, font=2)
text(c(-1.8), 62, c("Measure"))
par(op)

text(-2.8, -1, pos=4, cex=0.75, bquote(paste("Model (K = 33, N = ", .(formatC(yando_meta$k, digits=2, format="f")), 
                                             ", p = ", .(formatC(yando_meta$pval, digits=3, format="f")), ")")))

dev.off() 

#Meta-analysis for middle versus older adults 
MO_es <- escalc(measure = "SMD",
              m1i = Mmean_middle,
              sd1i = Msd_middle,
              n1i = middleN,
              m2i = Mmean_old,
              sd2i = Msd_old,
              n2i = oldN,
              data = mean_data) %>%
  filter(!is.na(yi))%>%
  group_by(paper) %>%
  mutate(group_id = cur_group_id())

#Reverse order of effect sizes to make interpretation easier
MO_es$yi <- -MO_es$yi

#Run meta-analysis
mando_meta <- rma.mv(data=MO_es,yi,vi,random= ~1 |group_id)

#Create forest plot
tiff(filename = "mando.tiff", width = 18, height = 16, units = "in", res = 600, compression = "lzw")

metafor::forest(mando_meta, slab=paste(paper), xlim=c(-2.8, 2.2), alim = c(-2, 2),
                ilab=cbind(Mname), ilab.xpos=c(-1.8), xlab="Hedges' (standardised) g",
                cex=.8, header = "Author Year", mlab="", order = yi)
op <- par(cex=.8, font=2)
text(c(-1.8), 62, c("Measure"))
par(op)

text(-2.8, -1, pos=4, cex=0.75, bquote(paste("Model (K = 10, N = ", .(formatC(mando_meta$k, digits=2, format="f")), 
                                             ", p = ", .(formatC(mando_meta$pval, digits=3, format="f")), ")")))

dev.off() 


#Meta-analysis for younger versus middle adults
YM_es <- escalc(measure = "SMD",
              m1i = Mmean_middle,
              sd1i = Msd_middle,
              n1i = middleN,
              m2i = Mmean_young,
              sd2i = Msd_young,
              n2i = youngN,
              data = mean_data) %>%
  filter(!is.na(yi))%>%
  group_by(paper) %>%
  mutate(group_id = cur_group_id())

#Reverse order of effect sizes to make interpretation easier
YM_es$yi <- -YM_es$yi

#Run meta-analysis
yandm_meta <- rma.mv(data=YM_es,yi,vi,random= ~1 |group_id)


#Create forest plot
tiff(filename = "yandm.tiff", width = 18, height = 16, units = "in", res = 600, compression = "lzw")

metafor::forest(yandm_meta, slab=paste(paper), xlim=c(-2.8, 2.2), alim = c(-2, 2),
                ilab=cbind(Mname), ilab.xpos=c(-1.8), xlab="Hedges' (standardised) g",
                cex=.8, header = "Author Year", mlab="", order = yi)
op <- par(cex=.8, font=2)
text(c(-1.8), 62, c("Measure"))
par(op)

text(-2.8, -1, pos=4, cex=0.75, bquote(paste("Model (K = 9, N = ", .(formatC(yandm_meta$k, digits=2, format="f")), 
                                             ", p = ", .(formatC(yandm_meta$pval, digits=3, format="f")), ")")))

dev.off() 

