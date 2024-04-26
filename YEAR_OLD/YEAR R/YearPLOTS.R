install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("esquisse")
install.packages("shiny")
install.packages("plyr")
library("dplyr")
library("esquisse")
library("tidyverse")
library("ggplot2")
library("shiny")
library("plyr")

#NOM SG SUBSETTING

# Filter the data and assign it to a new dataframe
nomsg.gph1 <- YrNomSin.wide %>%
  filter(tot.occ >= 2)

# Group by variables and calculate the count
nomsg.gph2 <- nomsg.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

# Plot the data
ggplot(data = nomsg.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()





#NOM PL SUBSETTING

YRnompl.gph1 <- YrNomPlu.wide %>%
  filter(tot.occ >= 2)

YRnompl.gph2 <- YRnompl.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRnompl.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()




#ACC SG SUBSETTING

YRaccsg.gph1 <- YrAccSin.wide %>%
  filter(tot.occ >= 2)

YRaccsg.gph2 <- YRaccsg.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRaccsg.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()


#GEN SG SUBSETTING

YRgensg.gph1 <- YrGenSin.wide %>%
  filter(tot.occ >= 2)

YRgensg.gph2 <- YRgensg.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRgensg.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()


# Calculate the number of individuals with each combination of occurrences and forms
YRgensg.gph2 <- YRgensg.gph1 %>% 
  group_by(tot.occ, tot.form) %>% 
  summarise(n = n())

# Print the summarized dataframe
print(YRgensg.gph2)

# Calculate the percentage of individuals with 2 or more forms out of all individuals with 2 or more occurrences
perc <- sum(YRgensg.gph2[YRgensg.gph2$tot.form >= 2, "n"]) / sum(YRgensg.gph2$n) * 100

# Print the percentage
cat("Percentage of individuals with 2 or more forms out of all individuals with 2 or more occurrences:", round(perc, 2), "%\n")



#ACC PL SUBSETTING

YRAccPlu.gph1 <- YrAccPlu.wide %>%
  filter(tot.occ >= 2)

YRAccPlu.gph2 <- YRAccPlu.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRAccPlu.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()



#INS PL SUBSETTING

YRInsPl.gph1 <- YrINSPlu.wide %>%
  filter(tot.occ >= 2)

YRInsPl.gph2 <- YRInsPl.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRInsPl.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()

#ADNM SUBSETTING

YRadnm.gph1 <- YrADMN.wide %>%
  filter(tot.occ >= 2)

YRadnm.gph2 <- YRadnm.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRadnm.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()


#LOC SG SUBSETTING
YRLocSin.gph1 <- YrLocSin.wide %>%
  filter(tot.occ >= 2)

YRLocSin.gph2 <- YRLocSin.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRLocSin.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()



# Calculate the percentage of individuals with 2 or more forms out of all individuals with 2 or more occurrences
perc <- sum(YRLocSin.gph2[YRLocSin.gph2$tot.form >= 2, "n"]) / sum(YRLocSin.gph2$n) * 100

# Print the percentage
cat("Percentage of individuals with 2 or more forms out of all individuals with 2 or more occurrences:", round(perc, 2), "%\n")



#LOC PL SUBSETTING
YRLocPlu.gph1 <- YrLocPlu.wide %>%
  filter(tot.occ >= 2)

YRLocPlu.gph2 <- YRLocPlu.gph1 %>%
  group_by(tot.occ, tot.form) %>%
  tally(name = "n")

ggplot(data = YRLocPlu.gph2, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()



#MULTILEVEL
#GEN SG
YrGenSin.long_filtered <- YrGenSin.long %>%
  filter(pres == TRUE)
esquisser(YrGenSin.long_filtered)

#GEN PL
YrGenPlu.long_filtered <- YrGenPlu.long %>%
  filter(pres == TRUE)
esquisser(YrGenPlu.long_filtered)

#LOC SG
YrLocSin.long_filtered <- YrLocSin.long %>%
  filter(pres == TRUE)
esquisser(YrLocSin.long_filtered)