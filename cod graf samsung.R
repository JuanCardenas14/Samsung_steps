library(readxl)
samsung <- read_excel("~/Desktop/Juan/Samsung_steps/samsung.xlsx")
View(samsung)

library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggthemes)
library(lubridate)

samsung <-as_tibble(samsung)

#Graficar de manera separada

months_samsung <- c("January", "February", "March", "April", "May", 
                    "June", "July", "August","September", "October", "November", 'December')

# Menos de veinte a?os

menos_veinte <- samsung %>%
  select (Month, "<20", Year) %>%
  rename( Steps = "<20")

menos_veinte$Month <- factor(menos_veinte$Month, levels = months_samsung)
menos_veinte$Year <- factor(menos_veinte$Year, levels = c("2018", "2019", "2020", "2021"))

plotunder20 <- menos_veinte %>%
  group_by(Year) %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Under 20 years")

# 20 - 29 a?os

veinte <- samsung %>%
  select (Month, "20-29", Year) %>%
  rename( Steps = "20-29")

veinte$Month <- factor(veinte$Month, levels = months_samsung)
veinte$Year <- factor(veinte$Year, levels = c("2018", "2019", "2020", "2021"))

plot20 <- veinte %>%
  group_by(Year) %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  ggtitle("20-29 years")

# 30 - 39 a?os

treinta <- samsung %>%
  select (Month, "30-39", Year) %>%
  rename( Steps = "30-39")

treinta$Month <- factor(treinta$Month, levels = months_samsung)
treinta$Year <- factor(treinta$Year, levels = c("2018", "2019", "2020", "2021"))

plot30 <- treinta %>%
  group_by(Year) %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  ggtitle("30-39 years")

# 40 - 49 a?os

cuarenta <- samsung %>%
  select (Month, "40-49", Year) %>%
  rename( Steps = "40-49")

cuarenta$Month <- factor(cuarenta$Month, levels = months_samsung)
cuarenta$Year <- factor(cuarenta$Year, levels = c("2018", "2019", "2020", "2021"))

plot40 <- cuarenta %>%
  group_by(Year) %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  ggtitle("40-49 years")

cincuenta <- samsung %>%
  select (Month, "50-59", Year) %>%
  rename( Steps = "50-59")

cincuenta$Month <- factor(cincuenta$Month, levels = months_samsung)
cincuenta$Year <- factor(cincuenta$Year, levels = c("2018", "2019", "2020", "2021"))

# 50 - 59 a?os

plot50 <- cincuenta %>%
  group_by(Year) %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  ggtitle("50-59 years")

# M?s de 60 a?os

sesenta <- samsung %>%
  select (Month, "60-69", Year) %>%
  rename( Steps = "60-69")

sesenta$Month <- factor(sesenta$Month, levels = months_samsung)
sesenta$Year <- factor(sesenta$Year, levels = c("2018", "2019", "2020", "2021"))

plot60 <- sesenta %>%
  group_by(Year) %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Over 60 years")

# Unir todo

menos_veinte$age_group <- c("<20")
veinte$age_group <- c("20 - 29")
treinta$age_group <- c("30 - 39")
cuarenta$age_group <- c("40 - 49")
cincuenta$age_group <- c("50 - 59")
sesenta$age_group <- c(">60")

Pasos <- bind_rows (menos_veinte, veinte, treinta, 
                    cuarenta, cincuenta, sesenta)

Pasos$age_group <- 
  factor(Pasos$age_group, 
         levels = c("<20", "20 - 29", "30 - 39", 
                    "40 - 49", "50 - 59", ">60"))

#Graficar por grupos de edad

plot_pasos_age <- Pasos %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "darkred", linetype = "longdash") +
  ylim (1500, 10500) +
  facet_grid(. ~  age_group) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust= 1),
    legend.position = "top") +
  ggtitle ("Monthly steps by year and age group") +
  scale_y_continuous(breaks =   seq(0, 10000, by= 1500)) + 
  labs(y= "Steps / day") +
  expand_limits(y = c(3800, 9800))

ggsave(plot_pasos_age, filename = "~/Desktop/Juan/Samsung_steps/steps_may2021_age.png",
       width = 9, dpi = 'retina')

# Gr?fica interactiva

plotly_pasos_age<- ggplotly(plot_pasos_age)

htmlwidgets::saveWidget(as_widget(plotly_pasos_age), "steps_age.html")

#Graficar por sexo

mujeres <- samsung %>%
  select (Month, Female, Year) %>%
  rename( Steps = Female)

mujeres$Month <- factor(mujeres$Month, levels = months_samsung)
mujeres$Year <- factor(mujeres$Year, levels = c("2018", "2019", "2020", "2021"))

hombres <- samsung %>%
  select (Month, Male, Year) %>%
  rename( Steps = Male)

hombres$Month <- factor(hombres$Month, levels = months_samsung)
hombres$Year <- factor(hombres$Year, levels = c("2018", "2019", "2020", "2021"))

hombres$sex <- c("Male")
mujeres$sex <- c("Female")

Pasos_sex <- bind_rows(hombres, mujeres)

plot_pasos_sex <- Pasos_sex %>%
  ggplot(data = ., aes(Month, Steps, colour = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "darkred", 
             linetype = "longdash") +
  facet_grid(. ~ sex) +
  theme_clean()+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust= 1)) +
  ggtitle ("Monthly steps by year and sex") +
  scale_y_continuous(breaks =  seq(0, 10000, by= 1500)) + 
  labs(y= "Steps / day") +
  expand_limits(y = c(3800, 9800))

plot_pasos_sex

ggsave(plot_pasos_sex, filename = "~/Desktop/Juan/Samsung_steps/steps_may2021_sex.png", 
       width = 9, dpi = 'retina')


plotly_pasos_sex<- ggplotly(plot_pasos_sex)

htmlwidgets::saveWidget(as_widget(plotly_pasos_sex), "steps_sex.html")


# línea de tiempo

plot_pasos_timeseries<- Pasos%>%
  mutate(Mo_yr = my(paste(Month, Year))) %>%
  ggplot(data = ., aes(Mo_yr, Steps, col = age_group, group = age_group)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "darkred", 
             linetype = "longdash") +
  theme_clean()+
  ggtitle ("Monthly steps by year and age group") +
  scale_color_grey()+
  scale_y_continuous(breaks =  seq(0, 10000, by= 1500)) +
  labs(y= "Steps / day", x = 'Year', colour = 'Age Group') +
  expand_limits(y = c(3800, 9800))
  
ggsave(plot_pasos_timeseries, filename = "~/Desktop/Juan/Samsung_steps/steps_mayo2021_timeseries.png", 
       width = 9, dpi = 'retina')


## Todo en blanco y negro
override.shape_age <- c(16, 17, 15, 3, 7, 8)
override.shape_year <- c(16, 17, 15, 8)
override.shape_sex <- c(16, 17)


plot_pasos_age_bw <- Pasos %>%
  ggplot(data = ., aes(Month, Steps, colour = Year,
                       shape = Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "black", 
             linetype = "longdash", size = 1) +
  ylim (1500, 10500) +
  facet_grid(. ~  age_group) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust= 1),
        legend.position = "top") +
  ggtitle ("Monthly steps by year and age group") +
  scale_y_continuous(breaks =   seq(0, 10000, by= 1500)) +
  scale_color_grey(start = 0.7, end = 0.1)+
  guides(colour = guide_legend(
    override.aes = list(shape = override.shape_year)))+
  scale_shape(guide = FALSE)+
  labs(y= "Steps / day") +
  expand_limits(y = c(3800, 9800))

plot_pasos_sex_bw <- Pasos_sex %>%
  ggplot(data = ., aes(Month, Steps, shape = Year, 
                       colour= Year, group = Year)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "black", 
             linetype = "longdash", size = 1) +
  facet_grid(. ~ sex) +
  theme_clean()+
  scale_color_grey(start = 0.7, end = 0.1)+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust= 1)) +
  ggtitle ("Monthly steps by year and sex") +
  scale_y_continuous(breaks =  seq(0, 10000, by= 1500)) + 
  guides(colour = guide_legend(
    override.aes = list(shape = override.shape_year)))+
  scale_shape(guide = FALSE)+
  labs(y= "Steps / day") +
  expand_limits(y = c(3800, 9800))


plot_pasos_timeseries_bw<- Pasos%>%
  mutate(Mo_yr = my(paste(Month, Year))) %>%
  ggplot(data = ., aes(Mo_yr, Steps, col = age_group, 
                       group = age_group,
                       shape = age_group)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "black", 
             linetype = "longdash", size = 1) +
  theme_clean()+
  ggtitle ("Monthly steps by year and age group") +
  scale_color_grey(start = 0.7, end = 0.1)+
  scale_y_continuous(breaks =  seq(0, 10000, by= 1500)) +
  guides(colour = guide_legend(
    override.aes = list(shape = override.shape_age)))+
  scale_shape(guide = FALSE)+
  labs(y= "Steps / day", x = 'Year', colour = 'Age Group') +
  expand_limits(y = c(3800, 9800))

plot_pasos_sex_timeline_bw <- Pasos_sex %>%
  mutate(Mo_yr = my(paste(Month, Year))) %>%
  ggplot(data = ., aes(Mo_yr, Steps, shape = sex, 
                       colour= sex)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 7500, col= "black", 
             linetype = "longdash", size = 1) +
  theme_clean()+
  scale_color_grey(start = 0.1, end = 0.5)+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust= 1),
        legend.position = 'top') +
  ggtitle ("Monthly steps by year and sex") +
  scale_y_continuous(breaks =  seq(0, 10000, by= 1500)) + 
  guides(colour = guide_legend(
    override.aes = list(shape = override.shape_sex)))+
  scale_shape(guide = FALSE)+
  labs(y= "Steps / day", x = 'Year', colour = 'Sex') +
  expand_limits(y = c(3800, 9800))

ggsave(plot_pasos_age_bw, filename = "~/Desktop/Juan/Samsung_steps/steps_may2021_age_bw.png", 
       width = 9, dpi = 'retina')
ggsave(plot_pasos_sex_bw, filename = "~/Desktop/Juan/Samsung_steps/steps_may2021_sex_bw.png", 
       width = 9, dpi = 'retina')
ggsave(plot_pasos_timeseries_bw, filename = "~/Desktop/Juan/Samsung_steps/steps_may2021_timeseries_bw.png", 
       width = 9, dpi = 'retina')
ggsave(plot_pasos_sex_timeline_bw, filename = "~/Desktop/Juan/Samsung_steps/steps_may2021_sex_timeseries_bw.png", 
       width = 9, dpi = 'retina')
