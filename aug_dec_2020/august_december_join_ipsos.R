install.packages("tidyverse")
install.packages("ggtext")
library(tidyverse)
library(glue)
library(ggtext)

data_aug_oct <- read_csv("aug_oct_2020/data-8KmKL.csv") %>% 
  rename(country = X.1,
         percent_august = "Total Agree - August 2020",
         percent_october = "Total Agree - October 2020")

data_oct_dec <- read_csv("oct_dec_2020/data-TDEsC.csv") %>% 
  rename(country='X.1',
         percent_october='Total agree - October',
         percent_december = 'Total agree - December') %>% 
  mutate(country=str_replace_all(country, "\\*",""))

data_aug_dec <- inner_join(data_aug_oct, data_oct_dec, by="country") %>% 
  select(-contains("october")) 

total_aug <- data_aug_dec %>%
  summarize(mean_aug = round(mean(percent_august),0))

total_adec <- data_aug_dec %>% 
  summarize(mean_dec = round(mean(percent_december),0))

df_ad <- data.frame("Total", total_aug, total_adec)
names(df_ad) <- c("country", "percent_august", "percent_december")

data_aug_dec <- tibble(rbind(df_ad, data_aug_dec)) %>% 
  mutate(bump_august = if_else(percent_august < percent_december,
                               percent_august - 3,
                               percent_august + 3),
         bump_december = if_else(percent_august < percent_december,
                                 percent_december + 3,
                                 percent_december - 3))

main_plot_aug_dec <- data_aug_dec %>%
  pivot_longer(cols = -country, names_to=c(".value", "month"),
               names_sep = "_") %>% replace_na(list(percent=43)) %>%
  mutate(country = factor(country, levels = rev(data_aug_dec$country))) %>%
  ggplot(aes(x=percent, y=country, color=month)) +
  geom_line(color="#e6e6e6", linewidth=1.75, show.legend = FALSE) +
  geom_point(size=2, show.legend = FALSE) +
  geom_text(aes(label=glue("{percent}%"), x=bump),size=3, show.legend = FALSE) +
  scale_color_manual(name=NULL,
                     breaks=c("august", "december"),
                     values=c("#727272", "#15607a"),
                     labels=c("August", "December")) +
  scale_x_continuous(limits=c(35, 100),
                     breaks=seq(35, 100, by=5),
                     labels=glue("{seq(35, 100, 5)}%")) +
  labs(x=NULL, y=NULL,
       title="If a vaccine for COVID-19 were available, I would get it",
       caption="<i>Base: Over 25,500 online adults aged 16-74 across 14 countries</i><br>Source: Ipsos")+
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=20)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", linewidth=0.1),
    panel.grid.major.y = element_line(color="gray", linewidth=0.1, linetype="dotted")
  )

total_aug_dec <- data_aug_dec %>% 
  filter(country == "Total") %>% 
  pivot_longer(cols = -country, names_to=c(".value","month"),
               names_sep="_") %>% 
  mutate(pretty = if_else(month == "august",
                          "Total Agree -<br>August 2020",
                          "Total Agree -<br>December 2020"),
         align = if_else(month == "august", 0, 1))

main_plot_aug_dec +
  coord_cartesian(clip="off") +
  geom_textbox(data = total_aug_dec,
               aes(x=percent, y=country, color=month, label=pretty, hjust=align),
               size=2,
               box.color=NA,
               width = NULL,
               vjust=-0.5,
               box.padding=margin(0,0,0,0),
               fill=NA,
               show.legend=FALSE)

ggsave("aug_dec_2020/aug_dec_2020_figure.tiff", width=6, height=4)

