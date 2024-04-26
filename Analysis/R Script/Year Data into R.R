setwd("C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets")
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

#Year Accussative plural
#read in Excel sheet
YrAccPlu <- read_excel("Plural Accusative.xlsx")

#create the broad form plus add extra columns for whether present or not.
YrAccPlu.long<-melt(YrAccPlu, id=c("Nid"),Measure=c("B5","C5"),variable.name = "form", value.name = "occurrence")
YrAccPlu.long$pres<-YrAccPlu.long$occurrence>0
YrAccPlu.long<-merge(Sp,YrAccPlu.long,by="Nid")
YrAccPlu.long<-arrange(YrAccPlu.long, Nid, form)

#create totals from long form and merge with original to form total
byNid<- group_by(YrAccPlu.long, Nid)
YrAccPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrAccPlu.wide<-merge(Sp,YrAccPlu,by="Nid")
YrAccPlu.wide<-merge(YrAccPlu.wide,YrAccPlu.tot, by="Nid" )

#tidy up
rm("YrAccPlu","YrAccPlu.tot","byNid")

#Year Genitive plural
#read in Excel sheet
YrGenPlu <- read_excel("Plural Genitive.xlsx")

#remove total column and row
YrGenPlu<-head(YrGenPlu,-1)
YrGenPlu<-within(YrGenPlu, rm(Total)) #rather frustrating as I have to add back but cannot do long length without it
names(YrGenPlu)[1] <-'Nid' #if not altered in Excel

#create the long form plus add extra columns for whether present or not.
YrGenPlu.long<-melt(YrGenPlu, id=c("Nid"),Measure=c("A7","B7","C7","D7","E7","F7","G7","H7","I7"),variable.name = "form", value.name = "occurrence")
YrGenPlu.long$pres<-YrGenPlu.long$occurrence>0
YrGenPlu.long<-merge(Sp,YrGenPlu.long,by="Nid")
YrGenPlu.long<-arrange(YrGenPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrGenPlu.long, Nid)
YrGenPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrGenPlu.wide<-merge(Sp,YrGenPlu,by="Nid")
YrGenPlu.wide<-merge(YrGenPlu.wide,YrGenPlu.tot, by="Nid" )

#tidy up
rm("YrGenPlu","YrGenPlu.tot","byNid")


#Year Nominative plural
#read in Excel sheet
YrNomPlu <- read_excel("Plural Nominative.xlsx")
View(YrNomPlu)

#create the long form plus add extra columns for whether present or not.
YrNomPlu.long<-melt(YrNomPlu, id=c("Nid"),Measure=c("A3","B3","C3","D3","E3"),variable.name = "form", value.name = "occurrence")
YrNomPlu.long$pres<-YrNomPlu.long$occurrence>0
YrNomPlu.long<-merge(Sp,YrNomPlu.long,by="Nid")
YrNomPlu.long<-arrange(YrNomPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrNomPlu.long, Nid)
YrNomPlu.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrNomPlu.wide<-merge(Sp,YrNomPlu,by="Nid")
YrNomPlu.wide<-merge(YrNomPlu.wide,YrNomPlu.tot, by="Nid" )

#tidy up
rm("YrNomPlu","YrNomPlu.tot","byNid")

#Year Locative plural
#read in Excel sheet
YrLocPlu <- read_excel("Plural Locative.xlsx")

#create the broad form plus add extra columns for whether present or not.
YrLocPlu.long<-melt(YrLocPlu, id=c("Nid"),Measure=c("B12","C12","D12","E12"),variable.name = "form", value.name = "occurrence")
YrLocPlu.long$pres<-YrLocPlu.long$occurrence>0
YrLocPlu.long<-merge(Sp,YrLocPlu.long,by="Nid")
YrLocPlu.long<-arrange(YrLocPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrLocPlu.long, Nid)
YrLocPlu.tot<- summarise(byNid,
                         tot.occ=sum(occurrence),
                         tot.form=sum(pres))
YrLocPlu.wide<-merge(Sp,YrLocPlu,by="Nid")
YrLocPlu.wide<-merge(YrLocPlu.wide,YrLocPlu.tot, by="Nid" )

#tidy up
rm("YrLocPlu","YrLocPlu.tot","byNid")

#Year INS plural
#read in Excel sheet
YrINSPlu <- read_excel("Plural INS.xlsx")


#create the broad form plus add extra columns for whether present or not.
YrINSPlu.long<-melt(YrINSPlu, id=c("Nid"),Measure=c("B10","C10","D10"),variable.name = "form", value.name = "occurrence")
YrINSPlu.long$pres<-YrINSPlu.long$occurrence>0
YrINSPlu.long<-merge(Sp,YrINSPlu.long,by="Nid")
YrINSPlu.long<-arrange(YrINSPlu.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrINSPlu.long, Nid)
YrINSPlu.tot<- summarise(byNid,
                         tot.occ=sum(occurrence),
                         tot.form=sum(pres))
YrINSPlu.wide<-merge(Sp,YrINSPlu,by="Nid")
YrINSPlu.wide<-merge(YrINSPlu.wide,YrINSPlu.tot, by="Nid" )

#tidy up
rm("YrINSPlu","YrINSPlu.tot","byNid")


#Singular forms

#Year Accussative singular
#read in Excel sheet
YrAccSin <- read_excel("Singular Accusative.xlsx")
View(YrAccSin)


#create the long form plus add extra columns for whether present or not.
YrAccSin.long<-melt(YrAccSin, id=c("Nid"),Measure=c("A4"),variable.name = "form", value.name = "occurrence")
YrAccSin.long$pres<-YrAccSin.long$occurrence>0
YrAccSin.long<-merge(Sp,YrAccSin.long,by="Nid")
YrAccSin.long<-arrange(YrAccSin.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrAccSin.long, Nid)
YrAccSin.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrAccSin.wide<-merge(Sp,YrAccSin,by="Nid")
YrAccSin.wide<-merge(YrAccSin.wide,YrAccSin.tot, by="Nid" )

#tidy up
rm("YrAccSin","YrAccSin.tot","byNid")




#Year Genitive Singular
#read in Excel sheet
YrGenSin <-  read_excel("Singular Genitive.xlsx")
View(YrGenSin)


#create the long form plus add extra columns for whether present or not.
YrGenSin.long<-melt(YrGenSin, id=c("Nid"),Measure=c("B10","C10"),variable.name = "form", value.name = "occurrence")
YrGenSin.long$pres<-YrGenSin.long$occurrence>0
YrGenSin.long<-merge(Sp,YrGenSin.long,by="Nid")
YrGenSin.long<-arrange(YrGenSin.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrGenSin.long, Nid)
YrGenSin.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrGenSin.wide<-merge(Sp,YrGenSin,by="Nid")
YrGenSin.wide<-merge(YrGenSin.wide,YrGenSin.tot, by="Nid" )

#tidy up
rm("YrGenSin","YrGenSin.tot","byNid")

#Year Nominative singular
#read in Excel sheet
YrNomSin <- read_excel("Singular Nominative.xlsx")
View(YrNomSin)


#create the long form plus add extra columns for whether present or not.
YrNomSin.long<-melt(YrNomSin, id=c("Nid"),Measure=c("B2","C2","D2"),variable.name = "form", value.name = "occurrence")
YrNomSin.long$pres<-YrNomSin.long$occurrence>0
YrNomSin.long<-merge(Sp,YrNomSin.long,by="Nid")
YrNomSin.long<-arrange(YrNomSin.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrNomSin.long, Nid)
YrNomSin.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrNomSin.wide<-merge(Sp,YrNomSin,by="Nid")
YrNomSin.wide<-merge(YrNomSin.wide,YrNomSin.tot, by="Nid" )

#tidy up
rm("YrNomSin","YrNomSin.tot","byNid")


#Year Locative singular
#read in Excel sheet
YrLocSin <- read_excel("Singular Locative.xlsx")


#create the broad form Sins add extra columns for whether present or not.
YrLocSin.long<-melt(YrLocSin, id=c("Nid"),Measure=c("B11","C11","D11","E11","F11","G11","H11", "I11"),variable.name = "form", value.name = "occurrence")
YrLocSin.long$pres<-YrLocSin.long$occurrence>0
YrLocSin.long<-merge(Sp,YrLocSin.long,by="Nid")
YrLocSin.long<-arrange(YrLocSin.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrLocSin.long, Nid)
YrLocSin.tot<- summarise(byNid,
                         tot.occ=sum(occurrence),
                         tot.form=sum(pres))
YrLocSin.wide<-merge(Sp,YrLocSin,by="Nid")
YrLocSin.wide<-merge(YrLocSin.wide,YrLocSin.tot, by="Nid" )

#tidy up
rm("YrLocSin","YrLocSin.tot","byNid")


#Year ADMN
#read in Excel sheet
YrADMN <- read_excel("ADMN.xlsx")
View(YrADMN)


#create the long form plus add extra columns for whether present or not.
YrADMN.long<-melt(YrADMN, id=c("Nid"),Measure=c("B2","C2","D2"),variable.name = "form", value.name = "occurrence")
YrADMN.long$pres<-YrADMN.long$occurrence>0
YrADMN.long<-merge(Sp,YrADMN.long,by="Nid")
YrADMN.long<-arrange(YrADMN.long, Nid, form)

#create totals from long form and merge with original to form total.
byNid<- group_by(YrADMN.long, Nid)
YrADMN.tot<- summarise(byNid,
                          tot.occ=sum(occurrence),
                          tot.form=sum(pres))
YrADMN.wide<-merge(Sp,YrADMN,by="Nid")
YrADMN.wide<-merge(YrADMN.wide,YrADMN.tot, by="Nid" )

#tidy up
rm("YrADMN","YrADMN.tot","byNid","Sp")


#tidy up
rm("YrAccPlu","YrAccPlu.tot","byNid")

#Save dataframes
save.image()
ACC<-c("YrAccPlu.long","YrAccPlu.wide","YrAccSin.long","YrAccSin.wide")
ADMn<-c("YrADMN.long","YrADMN.wide")
Gen<-c("YrGenPlu.long","YrGenPlu.wide","YrGenSin.long", "YrGenSin.wide")
   
save(YrAccPlu.long,YrAccPlu.wide,YrAccSin.long,YrAccSin.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets/YrAcc.Rdata")
save(YrADMN.long,YrADMN.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets/YrADMN.Rdata")
save(YrGenPlu.long,YrGenPlu.wide,YrGenSin.long,YrGenSin.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets/YrGen.Rdata")
save(YrNomPlu.long,YrNomPlu.wide,YrNomSin.long,YrNomSin.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets/YrNom.Rdata")
save(YrLocPlu.long,YrLocPlu.wide,YrLocSin.long,YrLocSin.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets/YrLoc.Rdata")
save(YrINSPlu.long,YrINSPlu.wide,file="C:/Users/cs1jr/Documents/Kristian Roncero/Year data sets/YrINSPlu.Rdata")
