install.packages("tidyverse")
install.packages("ggplot2")
install.packages("esquisse")
install.packages("shiny")
install.packages("dplyr")
library("esquisse")
library("tidyverse")
library("ggplot2")
library("shiny")
library("dplyr")


#these create the data frames to draw the dot plot
genplu.gph1 <- YrGenPlu.wide %>% filter(tot.occ >= 2) %>%
  group_by(tot.occ, tot.form) %>%
  summarise(n = n())

ggplot(data = genplu.gph1, aes(x = tot.occ, y = tot.form, size = n)) +
  geom_point()


# Filter for individuals with 2 or more occurrences
genplu.gph1 <- YrGenPlu.wide %>% 
  filter(tot.occ >= 2)

# Calculate the number of individuals with each combination of occurrences and forms
genplu.gph2 <- genplu.gph1 %>% 
  group_by(tot.occ, tot.form) %>% 
  summarise(n = n())

# Calculate the percentage of individuals with 2 or more forms out of all individuals with 2 or more occurrences
perc <- sum(genplu.gph2[genplu.gph2$tot.form >= 2, "n"]) / sum(genplu.gph2$n) * 100

# Print the percentage
cat("Percentage of individuals with 2 or more forms out of all individuals with 2 or more occurrences:", round(perc, 2), "%\n")

