setwd("C:/Users/cs1jr/Documents/Kristian Roncero/Person tables exported")
library(tidyr)
library(readxl)
library(Hmisc)
library(reshape2)
library(dplyr)

#create the speaker sheet for all data sets. 
#This will merge with other data sets. 
Sp<- read_excel("Classification Sheet - Speaker.xlsx")
View(Sp)
Sp$Village<-factor(Sp$Village)
Sp$Gender<-factor(Sp$Gender)
Sp$`Age group`<-factor(Sp$`Age group`)
Sp$Area<-if_else(Sp$Village %in% c("Bahdanawka","Pare / Vostraw","Žydče"),"East",
                       if_else(Sp$Village %in% c("Podlasie (Poland)"),"Poland","West"))
Sp$Gender.age<-if_else(Sp$Gender %in% c("Male"), "male",
     if_else(Sp$`Age group`%in% c("30-44", "45-59", "60-74"), "female under 75",
     "female 75 plus"))


#Person Accusative plural
#read in Excel sheet
PerAccPlu <- read_excel("Plural Accusative.xlsx")


#create the broad form plus add extra columns for whether present or not.
PerAccPlu.long<-melt(PerAccPlu, id=c("Nid"),Measure=c("B5","C5","D5","E5"),variable.name = "form", value.name = "occurrence")
PerAccPlu.long$pres<-PerAccPlu.long$occurrence>0
PerAccPlu.long<-merge(Sp,PerAccPlu.long,by="Nid")
PerAccPlu.long<-arrange(PerAccPlu.long, Nid, form)

#create totals from long form and merge with original to form total
byNid<- group_by(PerAccPlu.long, Nid)
PerAccPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerAccPlu.wide<-merge(Sp,PerAccPlu,by="Nid")
PerAccPlu.wide<-merge(PerAccPlu.wide,PerAccPlu.tot, by="Nid" )

#tidy up
rm("PerAccPlu","PerAccPlu.tot","byNid")

#Person Genitive plural
#read in Excel sheet
PerGenPlu <- read_excel("Plural Genitive.xlsx")

#create the long form plus add extra columns for whether present or not.
PerGenPlu.long<-melt(PerGenPlu, id=c("Nid"),Measure=c("A7","B7","C7","D7","E7","F7","G7","H7","I7"),variable.name = "form", value.name = "occurrence")
PerGenPlu.long$pres<-PerGenPlu.long$occurrence>0
PerGenPlu.long<-merge(Sp,PerGenPlu.long,by="Nid")
PerGenPlu.long<-arrange(PerGenPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(PerGenPlu.long, Nid)
PerGenPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerGenPlu.wide<-merge(Sp,PerGenPlu,by="Nid")
PerGenPlu.wide<-merge(PerGenPlu.wide,PerGenPlu.tot, by="Nid" )

#tidy up
rm("PerGenPlu","PerGenPlu.tot","byNid")


#Person Nominative plural
#read in Excel sheet
PerNomPlu <- read_excel("Plural Nominative.xlsx")
View(PerNomPlu)



#create the long form plus add extra columns for whether present or not.
PerNomPlu.long<-melt(PerNomPlu, id=c("Nid"),Measure=c("A3","B3","C3","D3","E3"),variable.name = "form", value.name = "occurrence")
PerNomPlu.long$pres<-PerNomPlu.long$occurrence>0
PerNomPlu.long<-merge(Sp,PerNomPlu.long,by="Nid")
PerNomPlu.long<-arrange(PerNomPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(PerNomPlu.long, Nid)
PerNomPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerNomPlu.wide<-merge(Sp,PerNomPlu,by="Nid")
PerNomPlu.wide<-merge(PerNomPlu.wide,PerNomPlu.tot, by="Nid" )

#tidy up
rm("PerNomPlu","PerNomPlu.tot","byNid")

#Person Dative plural
#read in Excel sheet
PerDatPlu <- read_excel("Plural Dative.xlsx")
View(PerDatPlu)


#create the long form plus add extra columns for whether present or not.
PerDatPlu.long<-melt(PerDatPlu, id=c("Nid"),Measure=c("B11","C11","D1"),variable.name = "form", value.name = "occurrence")
PerDatPlu.long$pres<-PerDatPlu.long$occurrence>0
PerDatPlu.long<-merge(Sp,PerDatPlu.long,by="Nid")
PerDatPlu.long<-arrange(PerDatPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(PerDatPlu.long, Nid)
PerDatPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerDatPlu.wide<-merge(Sp,PerDatPlu,by="Nid")
PerDatPlu.wide<-merge(PerDatPlu.wide,PerDatPlu.tot, by="Nid" )

#tidy up
rm("PerDatPlu","PerDatPlu.tot","byNid")




#Person Dative singular
#read in Excel sheet
PerDatSin <- read_excel("Singular Dative.xlsx")
View(PerDatSin)


#create the long form plus add extra columns for whether present or not.
PerDatSin.long<-melt(PerDatSin, id=c("Nid"),Measure=c("B10","C10"),variable.name = "form", value.name = "occurrence")
PerDatSin.long$pres<-PerDatSin.long$occurrence>0
PerDatSin.long<-merge(Sp,PerDatSin.long,by="Nid")
PerDatSin.long<-arrange(PerDatSin.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(PerDatSin.long, Nid)
PerDatSin.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerDatSin.wide<-merge(Sp,PerDatSin,by="Nid")
PerDatSin.wide<-merge(PerDatSin.wide,PerDatSin.tot, by="Nid" )

#tidy up
rm("PerDatSin","PerDatSin.tot","byNid")


#Person Nominative singular
#read in Excel sheet
PerNomSin <- read_excel("Singular Nominative.xlsx")
View(PerNomSin)

#create the long form plus add extra columns for whether present or not.
PerNomSin.long<-melt(PerNomSin, id=c("Nid"),Measure=c("B2","C2","D2"),variable.name = "form", value.name = "occurrence")
PerNomSin.long$pres<-PerNomSin.long$occurrence>0
PerNomSin.long<-merge(Sp,PerNomSin.long,by="Nid")
PerNomSin.long<-arrange(PerNomSin.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(PerNomSin.long, Nid)
PerNomSin.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerNomSin.wide<-merge(Sp,PerNomSin,by="Nid")
PerNomSin.wide<-merge(PerNomSin.wide,PerNomSin.tot, by="Nid" )

#tidy up
rm("PerNomSin","PerNomSin.tot","byNid")


#Person ADMN
#read in Excel sheet
PerADMN <- read_excel("ADMN.xlsx")
View(PerADMN)

#remove total column and row
PerADMN<-head(PerADMN,-1)
PerADMN<-within(PerADMN, rm(Total,A8)) #rather frustrating as I have to add back but cannot do long length without it
names(PerADMN)[1] <-'Nid' #if not altered in Excel

#create the long form plus add extra columns for whether present or not.
PerADMN.long<-melt(PerADMN, id=c("Nid"),Measure=c("B2","C2","D2"),variable.name = "form", value.name = "occurrence")
PerADMN.long$pres<-PerADMN.long$occurrence>0
PerADMN.long<-merge(Sp,PerADMN.long,by="Nid")
PerADMN.long<-arrange(PerADMN.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(PerADMN.long, Nid)
PerADMN.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
PerADMN.wide<-merge(Sp,PerADMN,by="Nid")
PerADMN.wide<-merge(PerADMN.wide,PerADMN.tot, by="Nid" )

#tidy up
rm("PerADMN","PerADMN.tot","byNid","Sp")

#Save dataframes
save.image()

   
save(PerAccPlu.long,PerAccPlu.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Person tables exported/PerAcc.Rdata")
save(PerADMN.long,PerADMN.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Person tables exported/PerADMN.Rdata")
save(PerDatPlu.long,PerDatPlu.wide,PerDatSin.long,PerDatSin.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Person tables exported/PerDat.Rdata")
save(PerGenPlu.long,PerGenPlu.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Person tables exported/PerGen.Rdata")
save(PerNomPlu.long,PerNomPlu.wide,PerNomSin.long,PerNomSin.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Person tables exported/PerNom.Rdata")
