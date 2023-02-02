install.packages("tidyverse")
install.packages("ggtext")
library(tidyverse)
library(glue)
library(ggtext)

data_oct_dec <- read_csv("oct_dec_2020/data-TDEsC.csv") %>% 
  rename(country='X.1',
         percent_october='Total agree - October',
         percent_december = 'Total agree - December') %>% 
  mutate(country=str_replace_all(country, "\\*",""))

total_oct <- data_oct_dec %>% drop_na() %>% 
  summarize(mean_oct = round(mean(percent_october),0))

total_dec <- data_oct_dec %>% 
  summarize(mean_dec = round(mean(percent_december),0))

df_od <- data.frame("Total", total_oct, total_dec)
names(df_od) <- c("country", "percent_october", "percent_december")

data_oct_dec <- tibble(rbind(df_od, data_oct_dec)) %>% 
  mutate(bump_october = if_else(percent_october < percent_december,
                                percent_october - 3,
                                percent_october + 3,),
         bump_december = if_else(percent_october < percent_december,
                                 percent_december + 3,
                                 percent_december - 3)) %>% 
  replace_na(list(bump_october=40, bump_december=40))

main_plot_oct_dec <- data_oct_dec %>%
  pivot_longer(cols = -country, names_to=c(".value", "month"),
               names_sep = "_") %>% replace_na(list(percent=43)) %>%
  mutate(country = factor(country, levels = rev(data_oct_dec$country))) %>%
  ggplot(aes(x=percent, y=country, color=month)) +
  geom_line(color="#e6e6e6", linewidth=1.75, show.legend = FALSE) +
  geom_point(size=2, show.legend = FALSE) +
  geom_text(aes(label=glue("{percent}%"), x=bump),size=3, show.legend = FALSE) +
  scale_color_manual(name=NULL,
                     breaks=c("october", "december"),
                     values=c("#727272", "#15607a"),
                     labels=c("October", "December")) +
  scale_x_continuous(limits=c(35, 100),
                     breaks=seq(35, 100, by=5),
                     labels=glue("{seq(35, 100, 5)}%")) +
  labs(x=NULL, y=NULL,
       title="If a vaccine for COVID-19 were available, I would get it",
       caption="<i>Base: 13,542 online adults aged 16-74 across 15 countries</i><br>Source: Ipsos")+
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

total_oct_dec <- data_oct_dec %>% 
  filter(country == "Total") %>% 
  pivot_longer(cols = -country, names_to=c(".value","month"),
               names_sep="_") %>% 
  mutate(pretty = if_else(month == "october",
                          "Total Agree -<br>October 2020",
                          "Total Agree -<br>December 2020"),
         align = if_else(month == "october", 0, 1))

main_plot_oct_dec +
  coord_cartesian(clip="off") +
  geom_textbox(data = total_oct_dec,
               aes(x=percent, y=country, color=month, label=pretty, hjust=align),
               size=2,
               box.color=NA,
               width = NULL,
               vjust=-0.5,
               box.padding=margin(0,0,0,0),
               fill=NA,
               show.legend=FALSE)

ggsave("oct_dec_2020/oct_dec_2020_figure.tiff", width=6, height=4)
