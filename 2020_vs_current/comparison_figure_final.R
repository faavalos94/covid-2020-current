install.packages("tidyverse")
install.packages("ggrepel")
install.packages("patchwork")
install.packages("ggtext")
install.packages("showtext")
library(tidyverse)
library(ggrepel)
library(patchwork)
library(ggtext)
library(showtext)

# fonts from google
font_add_google(family = "playfair", "Playfair Display")
font_add_google(family = "montserrat", "Montserrat")

showtext_auto()

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
  select(country = location,
         date,
         all_vax = people_vaccinated_per_hundred, 
         fully_vax = people_fully_vaccinated_per_hundred) %>%
  drop_na(fully_vax) %>% 
  group_by(country) %>% 
  slice_max(date, n=1) %>% 
  ungroup() %>% 
  select(-date)

data_aug_oct <- read_csv("aug_oct_2020/data-8KmKL.csv") %>%
  rename(country = X.1,
         percent_august = "Total Agree - August 2020",
         percent_october = "Total Agree - October 2020")

data_oct_dec <- read_csv("oct_dec_2020/data-TDEsC.csv") %>% 
  rename(country='X.1',
         percent_october='Total agree - October',
         percent_december = 'Total agree - December') %>% 
  mutate(country=str_replace_all(country, "\\*",""))

ipsos <- inner_join(data_aug_oct, data_oct_dec, by="country") %>% 
  select(-contains("october"))

ipsos_owid <- inner_join(owid, ipsos, by = "country") %>% 
  mutate(diff = fully_vax-percent_december,          # or diff in fct_reorder 
         country = fct_reorder(country, -diff),     # function to show opposite, this alone allow the plots to be descending
         country = recode(country,
                          "United States" = "USA",
                          "United Kingdom" = "UK",
                          "South Korea" = "S. Korea",
                          "South Africa" = "S. Africa"))

dot <- ipsos_owid %>% 
  ggplot(aes(x=diff, y=country)) +
  geom_hline(aes(yintercept=country), color="gray", linewidth=0.25) +
  geom_point() +
  geom_vline(xintercept = 0) +
  labs(x="Difference between actual\nand intended vaccination rate",
       y=NULL) +
  theme_classic() +
  theme(
    text = element_text(family = "montserrat"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=10)
  )

dumbbell <- ipsos_owid %>% 
  select(country, actual=fully_vax, intended=percent_december) %>% 
  pivot_longer(cols=c(actual, intended), names_to="status", values_to="percent") %>% 
  mutate(status = factor(status, levels=c("intended", "actual"))) %>% 
  ggplot(aes(x=percent, y=country, group=country, color=status)) +
  geom_hline(aes(yintercept=country), color="gray", linewidth=0.25) +
  geom_path(arrow=arrow(ends="first", angle=20, length=unit(0.1, "inches"), type="closed"),
            color="black") +
  labs(x="2020 intended and 2023\nactual vaccination rates",
       y=NULL) +
  theme_classic() +
  theme(
    text = element_text(family = "montserrat"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_text(size=10)
  )

dot + dumbbell +
  plot_annotation(
    title="Countries are doing a pretty good job at meeting their people's stated desire to receive the COVID-19 vaccine",
    theme=theme(
      plot.title=element_textbox_simple(size=20, face="bold", family = "playfair")
    )
  ) +
  plot_layout(widths = c(2, 3))

ggsave("2020_vs_current/comparison_figure.tiff", width=6, height=4)
