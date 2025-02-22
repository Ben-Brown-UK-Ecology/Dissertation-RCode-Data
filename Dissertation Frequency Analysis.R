library(readr)
library(dplyr)

data<-read.table("Assessments/Dissertation/Echolocation Data.txt", header=TRUE)
str(data)

library(lubridate)

data <- data %>%
  mutate(TAfterSun = period_to_seconds(hm(TAfterSun)) / 60)

data <- data %>%
  mutate(Social_Echo_Match = ifelse(Social_ID == Echo_ID, "Match", "No Match"))
head(data)
data$Social_Echo_Match <- as.factor(data$Social_Echo_Match)

#The above code creates a new column with each row assigned either match or no match depending on whether Echo_ID and Social_ID are the same or not

data <- data %>%
  mutate(Difference_in_Frequency = Post_Avg_Freq - Pre_Avg_Freq)
head(data)
#The above creates a difference in call rate column

summary(data$Difference_in_Frequency)

shift <- abs(min(data$Difference_in_Frequency)) + 0.001
data <- data %>% 
  mutate(Diff_Freq_Shifted = Difference_in_Frequency + shift)

summary(data$Diff_Freq_Shifted)

#The above turns all negative difference values positive by adding the minimum constant possible to get each value above 0 as model cannot deal with negative

Pipdatapyg <- data[data$Echo_ID=="Pippip"&data$Social_ID!="Pipnat",]
Pipdatanat <- data[data$Echo_ID=="Pippip"&data$Social_ID!="Pippyg",]
Pygdatapip <- data[data$Echo_ID=="Pippyg"&data$Social_ID!="Pipnat",]
Pygdatanat <- data[data$Echo_ID=="Pippyg"&data$Social_ID!="Pippip",]
Natdatapip <- data[data$Echo_ID=="Pipnat"&data$Social_ID!="Pippyg",]
Natdatapyg <- data[data$Echo_ID=="Pipnat"&data$Social_ID!="Pippip",]

#Echo - data - social

Pipdata <- data[data$Echo_ID=="Pippip",]
Pygdata <- data[data$Echo_ID=="Pippyg",]
Natdata <- data[data$Echo_ID=="Pipnat",]

#Pipdatapyg:
hist(Pipdatapyg$Diff_Freq_Shifted, breaks = 20, main = "Histogram of Difference in Frequency")
shapiro.test(Pipdatapyg$Diff_Freq_Shifted)

cor(Pipdatapyg[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])

lm_pippyg <- lm(Diff_Freq_Shifted ~ Social_Echo_Match, 
                data = Pipdatapyg)

summary(lm_pippyg)
summary(Pipdatapyg)

#Pipdatanat:
#N/a as I have no data on pips responding to nat social calls

#Pygdatapip:
hist(Pygdatapip$Diff_Freq_Shifted, breaks = 20, main = "Histogram of Difference in Call Frequency")
shapiro.test(Pygdatapip$Diff_Freq_Shifted)

cor(Pygdatapip[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])


lm_pygpip <- lm(Diff_Freq_Shifted ~ Social_Echo_Match, 
                data = Pygdatapip)
summary(lm_pygpip)

lm_pygpip <- lm(Diff_Freq_Shifted ~ Social_Echo_Match + Social_Freq, 
                           data = Pygdatapip)
summary(lm_pygpip)
summary(Pygdatapip)

#Pygdatanat:
hist(Pygdatanat$Diff_Freq_Shifted, breaks = 20, main = "Histogram of Difference in Frequency")
shapiro.test(Pygdatanat$Diff_Freq_Shifted)

cor(Pygdatanat[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])

lm_pygnat <- lm(Diff_Freq_Shifted ~ Social_Echo_Match, 
                data = Pygdatanat)
summary(lm_pygnat)

lm_pygnat <- lm(Diff_Freq_Shifted ~ Social_Echo_Match + Social_Freq + Social_Len, 
                           data = Pygdatanat)
summary(lm_pygnat)
summary(Pygdatanat)

#Natdatapip:
hist(Natdatapip$Diff_Freq_Shifted, breaks = 20, main = "Histogram of Difference in Frequency")
shapiro.test(Natdatapip$Diff_Freq_Shifted)

cor(Natdatapip[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])

lm_natpip <- lm(Diff_Freq_Shifted ~ Social_Echo_Match, 
                data = Natdatapip)
summary(lm_natpip)

summary(Natdatapip)

#Natdatapyg:
hist(Natdatapyg$Diff_Freq_Shifted, breaks = 20, main = "Histogram of Difference in Frequency")
shapiro.test(Natdatapyg$Diff_Freq_Shifted)

cor(Natdatapyg[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])

lm_natpyg <- lm(Diff_Freq_Shifted ~ Social_Echo_Match, 
                data = Natdatapyg)
summary(lm_natpyg)

summary(Natdatapyg)


#Barplot section:

ggplot(Pipdata, aes(x = Social_ID, y = Diff_Freq_Shifted + -3.101, fill = Social_ID)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "",
       x = "Social Call Identification",
       y = "Change in P. pipistrellus Echolocation Frequency") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Pippip" = "blue", "Pippyg" = "red"))


ggplot(Pygdata, aes(x = Social_ID, y = Diff_Freq_Shifted + -3.101, fill = Social_ID)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "",
       x = "Social Call Identification",
       y = "Change in P. pygmaeus Echolocation Frequency") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Pippip" = "blue", "Pippyg" = "red", "Pipnat" = "Green"))


ggplot(Natdata, aes(x = Social_ID, y = Diff_Freq_Shifted + -3.101, fill = Social_ID)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "",
       x = "Social Call Identification",
       y = "Change in P. nathusii Echolocation Frequency") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Pippip" = "blue", "Pippyg" = "red", "Pipnat" = "Green"))





