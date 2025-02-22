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

#The above code creates a new column with each row assigned either match or no match depending on whether Echo_ID and Social_ID are the same or not

data <- data %>%
  mutate(Difference_in_Call_rate = Post_Call_Rate - Pre_Call_Rate)
head(data)
#The above creates a difference in call rate column

summary(data$Difference_in_Call_rate)

shift <- abs(min(data$Difference_in_Call_rate)) + 0.001
data <- data %>% 
  mutate(Diff_call_rate_shifted = Difference_in_Call_rate + shift)

summary(data$Diff_call_rate_shifted)

#The above turns all negative difference values positive by adding the minimum constant possible to get each value above 0 as model cannot deal with negative

Pipdata <- data[data$Echo_ID=="Pippip",]
Pygdata <- data[data$Echo_ID=="Pippyg",]
Natdata <- data[data$Echo_ID=="Pipnat",]

#This creates three separate data sets, one for each species response to social calls

#Section 1 - Pippip data analysis:

hist(Pipdata$Diff_call_rate_shifted, breaks = 20, main = "Histogram of Difference in Call Rate")
shapiro.test(Pipdata$Diff_call_rate_shifted)
#Data is normal to no need for glm

cor(Pipdata[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])
#Checks for correlations between the data, if correlation >0.7, remove one of the causing variables

lm_pip <- lm(Diff_call_rate_shifted ~ Social_Echo_Match, 
             data = Pipdata)

summary(lm_pip)

shapiro.test(resid(lm_pip))  

plot(lm_pip, which = 1)  

qqnorm(resid(lm_pip))  
qqline(resid(lm_pip), col = "red")  

library(car)
vif(lm_pip)  # If any VIF > 5, remove correlated variables


lm_pip_interaction <- lm(Diff_call_rate_shifted ~ Social_Echo_Match * TAfterSun + Num_Parts + Social_Freq + Social_Len, data = Pipdata)
summary(lm_pip_interaction)
AIC(lm_pip)  
AIC(lm_pip_interaction)  
#Interaction model has higher AIC (bad) so shall use original model


#All that really matters for this section is the initial lm_pip model as all the tests afterwards showed no issue


ggplot(Pipdata, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted, color = Social_Echo_Match)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(title = "Mean Shift in Call Rate by Social Echo Match",
       x = "Social Echo Match",
       y = "Shift in Call Rate") +
  theme_minimal() +
  scale_color_manual(values = c("Match" = "blue", "No Match" = "red"))


ggplot(Pipdata, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted + -7, fill = Social_Echo_Match)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "Call Rate Change in Response to Intraspecific and Interspecific Social Calls",
       x = "Type of Response",
       y = "Change in Echolocation Call Rate per minute") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Match" = "blue", "No Match" = "red")) +
  scale_x_discrete(labels = c("Match" = "Intraspecific Response", "No Match" = "Interspecific Response"))


#Section 2 - Pippyg data analysis:

hist(Pygdata$Diff_call_rate_shifted, breaks = 20, main = "Histogram of Difference in Call Rate")
shapiro.test(Pygdata$Diff_call_rate_shifted)

cor(Pygdata[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])
#Check if correlation >0.7, remove the causing variables

glm_gaussian_pyg1 <- glm(Diff_call_rate_shifted ~ Social_Echo_Match + TAfterSun + Num_Parts + Social_Freq + Social_Len, 
                        family = gaussian(link = "identity"), data = Pygdata)
summary(glm_gaussian_pyg1)

glm_gaussian_pyg <- glm(Diff_call_rate_shifted ~ Social_Echo_Match, 
                        family = gaussian(link = "identity"), data = Pygdata)
summary(glm_gaussian_pyg)

ggplot(Pygdata, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted, color = Social_Echo_Match)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(title = "Mean Shift in Call Rate by Social Echo Match",
       x = "Social Echo Match",
       y = "Shift in Call Rate") +
  theme_minimal() +
  scale_color_manual(values = c("Match" = "blue", "No Match" = "red"))

ggplot(Pygdata, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted + -7, fill = Social_Echo_Match)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "Call Rate Change in Response to Intraspecific and Interspecific Social Calls",
       x = "Type of Response",
       y = "Change in Echolocation Call Rate per minute") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Match" = "blue", "No Match" = "red")) +
  scale_x_discrete(labels = c("Match" = "Intraspecific Response", "No Match" = "Interspecific Response"))

#Section 3 - Pipnat data analysis:

hist(Natdata$Diff_call_rate_shifted, breaks = 20, main = "Histogram of Difference in Call Rate")
shapiro.test(Natdata$Diff_call_rate_shifted)

cor(Natdata[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])

glm_gaussian_nat <- glm(Diff_call_rate_shifted ~ Social_Echo_Match, 
                        family = gaussian(link = "identity"), data = Natdata)
summary(glm_gaussian_nat)

glm_gaussian_nat1 <- glm(Diff_call_rate_shifted ~ Social_Echo_Match + TAfterSun + Num_Parts + Social_Freq + Social_Len, 
                         family = gaussian(link = "identity"), data = Natdata)
summary(glm_gaussian_nat1)

ggplot(Natdata, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted, color = Social_Echo_Match)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Shift in Call Rate by Social Echo Match",
       x = "Social Echo Match",
       y = "Shift in Call Rate") +
  theme_minimal() +
  scale_color_manual(values = c("Match" = "blue", "No Match" = "red"))

ggplot(Natdata, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted + -7, fill = Social_Echo_Match)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "Call Rate Change in Response to Intraspecific and Interspecific Social Calls",
       x = "Type of Response",
       y = "Change in Echolocation Call Rate per minute") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Match" = "blue", "No Match" = "red")) +
  scale_x_discrete(labels = c("Match" = "Intraspecific Response", "No Match" = "Interspecific Response"))













#Section 4 - Template linear GLM:
hist(Natdata$Diff_call_rate_shifted, breaks = 20, main = "Histogram of Difference in Call Rate")
shapiro.test(Natdata$Diff_call_rate_shifted)

cor(Natdata[, c("Social_Freq", "Social_Len", "TAfterSun", "Num_Parts")])

glm_gaussian_nat <- glm(Diff_call_rate_shifted ~ Social_Echo_Match + Social_Freq + Social_Len + TAfterSun + Num_Parts, 
                        family = gaussian(link = "identity"), data = Natdata)
summary(glm_gaussian_nat)

ggplot(data, aes(x = Social_Echo_Match, y = Diff_call_rate_shifted + -7, fill = Social_Echo_Match)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
  labs(title = "Call Rate Change in Response to Intraspecific and Interspecific Social Calls",
       x = "Type of Response",
       y = "Change in Echolocation Call Rate per minute") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Match" = "Grey", "No Match" = "White")) +
  scale_x_discrete(labels = c("Match" = "Intraspecific Response", "No Match" = "Interspecific Response"))
