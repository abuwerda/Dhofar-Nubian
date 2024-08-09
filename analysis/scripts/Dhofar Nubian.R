## Global options

library(ggpubr)
library(knitr)
library(tidyverse)
library(kableExtra)
library(ggsci)
library(plotly)
library(viridis)

knitr::opts_chunk$set(
  cache = TRUE,
  echo = FALSE,
  fig.pos = "!H",
  message = FALSE,
  warning = FALSE,
  comment = NA,
  prompt = FALSE
)

options(knitr.table.format = "markdown")
options(qwraps2_markup = "markdown")
options(tinytex.verbose = TRUE)
options(warn = -1)

## The dataset and markdown files are in the same directory

data = read_csv("analysis/data/raw_data/DN_Database_20240808.csv")
df = data %>%
  select(c("Site", "Art_Type", "Art_Class", "Max_Length", "Core_Type_Simple", "Lev_Scar_Length", "Condition", "Patination", "Dissolution")) %>%
  filter(Art_Class == "core")

## Color Palette
virdis_colors = viridis(7, option = "D", direction = -1)
color_mapping = setNames(virdis_colors, as.character(1:7))

## Figure counter to track the figure number
figure_counter = 1
sup_figure_counter = 1

## Descriptive Statistics

## Techno Histograms

custom_labels=c(
  bi="Bidirectional",
  Lev_Nub="Nubian Levallois",
  Lev_other="Other Levallois",
  uni_blade="Blade"
)

ggplot(data = df %>% filter(!is.na(Core_Type_Simple))) +
  geom_histogram(aes(y = ..count.., x = Patination, fill = as.factor(Patination)),
                 color = "black", binwidth = 1) +
  scale_fill_manual(values = color_mapping) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,7, by = 1)) +
  labs(fill = "Patina Stage", y = "artifact count", x = "") +
  facet_wrap(~Core_Type_Simple, scales = "free_y", labeller=as_labeller(custom_labels))

## Complete Nubian Core & Product Metric Box Plots

plot = ggplot(df %>% filter(Art_Type == "Lev_Nub", Condition == "complete", Site != "TH.069"),
                aes(x = as.factor(Patination), y = Max_Length,
                    fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Patina Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot

plot = ggplot(df %>% filter(Art_Type == "Lev_Nub", Condition == "complete", Site != "TH.069"),
              aes(x = as.factor(Dissolution), y = Max_Length,
                  fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Dissolution Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot

plot = ggplot(df %>% filter(Art_Type == "blank_Lev", Condition == "complete", Site != "TH.069"),
              aes(x = as.factor(Patination), y = Max_Length,
                  fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Patina Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot

plot = ggplot(df %>% filter(Art_Type == "blank_Lev", Condition == "complete", Site != "TH.069"),
              aes(x = as.factor(Dissolution), y = Max_Length,
                  fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Dissolution Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot

plot = ggplot(df %>% filter(Art_Type == "Lev_Nub", Condition == "complete", Site != "TH.069"),
              aes(x = as.factor(Patination), y = Lev_Scar_Length,
                  fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Patina Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot

plot = ggplot(df %>% filter(Art_Type == "Lev_Nub", Condition == "complete", Site != "TH.069"),
              aes(x = as.factor(Dissolution), y = Lev_Scar_Length,
                  fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Dissolution Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot



## Plots for counts and percentages

library(tidyverse)

# Filter out NA values from Core_Type_Simple
df_filtered <- df %>%
  filter(!is.na(Core_Type_Simple))

# Calculate percentages
percentage_table <- df_filtered %>%
  group_by(Site, Core_Type_Simple) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create a bar plot
ggplot(percentage_table, aes(x = Site, y = percentage, fill = Core_Type_Simple)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage of Core_Type_Simple by Site", x = "Site", y = "Percentage") +
  theme_minimal() +
  theme(legend.title = element_blank())



### One table for each site

library(tidyverse)

# Calculate percentages of Art_Type by Patination for each Site
percentage_table <- df %>%
  group_by(Site, Patination, Art_Type) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create a bar plot with facets per site
ggplot(percentage_table, aes(x = Patination, y = percentage, fill = Art_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage of Art_Type by Patination for Each Site",
       x = "Patination",
       y = "Percentage") +
  facet_wrap(~ Site) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


