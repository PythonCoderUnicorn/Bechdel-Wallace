
# DS9 Bechdel Test

library(tidyverse)
library(ggplot2)


# read in data
ds9 = read_csv('StarTrek-DS9.csv')

head(ds9)


ds9 %>% count(Season)

ds9 %>% count(`Bechdel-Wallace`) %>% 
  ggplot( aes(x= `Bechdel-Wallace`, y= n ) ) +
  geom_col( fill= c("#cc0000",'#00cc00'))+ 
  labs(title = "Star Trek DS9 Bechdel-Wallace Test",
       subtitle = "Total number of pass and fails per episode") +
  theme(axis.line = element_line(colour = "white",
    linetype = "solid"), axis.ticks = element_line(colour = "white",
    linetype = "blank"), panel.grid.major = element_line(colour = "gray",
    linetype = "blank"), panel.grid.minor = element_line(linetype = "blank"),
    axis.text = element_text(size = 12, colour = "ghostwhite"),
    panel.background = element_rect(fill = "gray0"),
    plot.background = element_rect(fill = "black")) + theme(plot.subtitle = element_text(size = 11,
    colour = "white" ), plot.caption = element_text(family = "mono",
    size = 13, colour = "white", hjust = 0),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_text(colour = "white"),
    legend.text = element_text(size = 12,
        colour = "white"), legend.title = element_text(size = 12,
        colour = "white")) +labs(caption = "@StarTrek_Lt")

  


  
ds9 %>% 
  select(Director_Gender, Writer_Gender, Episode, `Bechdel-Wallace`) %>% 
  group_by(Director_Gender) %>% 
  count() %>% 
  ggplot( aes(x= Director_Gender, y= n)) +
  geom_col( fill= c('#FFA747','#9999CD')) +
  labs(title = "Directors by Gender") +
  geom_text( aes(label= n, vjust=1.3))+
  theme(axis.line = element_line(colour = "white",
                                 linetype = "solid"), axis.ticks = element_line(colour = "white",
                                                                                linetype = "blank"), panel.grid.major = element_line(colour = "gray",
                                                                                                                                     linetype = "blank"), panel.grid.minor = element_line(linetype = "blank"),
        axis.text = element_text(size = 12, colour = "ghostwhite"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) + theme(plot.subtitle = element_text(size = 11,
                                                                                             colour = "white" ), plot.caption = element_text(family = "mono",
                                                                                                                                             size = 13, colour = "white", hjust = 0),
                                                                axis.text.x = element_text(colour = "white"),
                                                                axis.text.y = element_text(colour = "white"),
                                                                legend.text = element_text(size = 12,
                                                                                           colour = "white"), legend.title = element_text(size = 12,
                                                                                                                                          colour = "white")) +labs(caption = "@StarTrek_Lt")

ds9 %>% 
  select(`Bechdel-Wallace`, Director_Gender, Episode, Season) %>% 
  # filter(`Bechdel-Wallace` =="Pass") %>% 
  group_by(Episode,`Bechdel-Wallace`) %>% 
  ggplot( aes(x= Director_Gender, y= `Bechdel-Wallace` )) +
  geom_bin_2d() + # #6666ff  #ffb3b3
  coord_flip()+
  labs(
    title = "Director Gender and Bechdel-Wallace test",
    subtitle = "Bechdel-Wallace test based on episodes by Director Gender",
    y="Test outcome", x= "Director's Gender")+
  theme(axis.line = element_line(colour = "white",linetype = "solid"), 
        axis.ticks = element_line(colour = "white",linetype = "blank"), 
        panel.grid.major = element_line(colour = "gray",linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"),
        axis.text = element_text(size = 12, colour = "ghostwhite"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) + 
  theme(plot.subtitle = element_text(size = 11,colour = "white" ), 
        plot.caption = element_text(family = "mono",
                                    size = 13, colour = "white", hjust = 0),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        legend.text = element_text(size = 12,colour = "white"), 
        legend.title = element_text(size = 12, colour = "white")) +
  labs(caption = "@StarTrek_Lt") + theme(legend.key = element_rect(size = 0.7),
    legend.background = element_rect(fill = NA))+ 
  theme(axis.title = element_text(size = 13,
    colour = "white"), plot.title = element_text(size = 15,
    colour = "white"))








ds9 %>% 
  filter(Director_Gender == "Female") %>% 
  select(Title, Director, Season) %>% 
  ggplot( aes(x= Season, y= Title))+
  geom_point(size= 5, color='purple')+ 
  theme(plot.subtitle = element_text(size = 11),
    plot.caption = element_text(family = "mono",
        size = 13, colour = "gray", hjust = 0),
    panel.grid.major = element_line(colour = "gray23"),
    panel.grid.minor = element_line(colour = "gray23"),
    axis.text = element_text(size = 12, colour = "gray"),
    plot.title = element_text(colour = "white"),
    panel.background = element_rect(fill = "gray0"),
    plot.background = element_rect(fill = "black")) +labs(title = "Female Directors Kim Friedman & Gabrielle Beaumont Episodes by Season",
    caption = "@StarTrek_Lt")










ds9 %>% 
  filter(Writer_Gender =="Female") %>% 
  select(Title, Writer_Gender, Season, Episode, Written_By) %>% 
  ggplot( aes(x= Written_By, y= Season ))+
  geom_point(size=4, color="#C4DE23")+
  coord_flip()+
  theme(plot.subtitle = element_text(size = 11),
        plot.caption = element_text(family = "mono",
                                    size = 13, colour = "gray", hjust = 0),
        panel.grid.major = element_line(colour = "gray23"),
        panel.grid.minor = element_line(colour = "gray23"),
        axis.text = element_text(size = 12, colour = "gray"),
        plot.title = element_text(colour = "white"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) +
  labs(title = "DS9 Writers with 1 or more Female Writer by Season",
       y= "Season",
       x= "Female Writers",
       caption = "@StarTrek_Lt") + 
  theme(axis.title = element_text(size = 13,colour = "white"))
  









ds9 %>% 
  filter(Director =="Levar Burton") %>% view()
  ggplot( aes(x= Episode, y= `Bechdel-Wallace`, color= Season))+
  geom_point(size=5 ) +
  coord_flip() +
  scale_color_gradientn(colours= rainbow(5) )+  # rainbow(10) terrain.colors
  theme(plot.subtitle = element_text(size = 11),
        plot.caption = element_text(family = "mono",
                                    size = 13, colour = "gray", hjust = 0),
        panel.grid.major = element_line(colour = "gray23"),
        panel.grid.minor = element_line(colour = "gray23"),
        axis.text = element_text(size = 12, colour = "gray"),
        plot.title = element_text(colour = "white"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) +
  labs(title = "Bechdel-Wallace test for Director Levar Burton Episodes",
                                                              caption = "@StarTrek_Lt") + theme(axis.title = element_text(size = 13,
    colour = "white")) + theme(legend.text = element_text(size = 12,
    colour = "white"), legend.title = element_text(size = 12,
    colour = "white"), legend.key = element_rect(fill = "gray100",
    colour = "white", size = 0.7), legend.background = element_rect(fill = "gray0",
    size = 0.9))

  
  
  
  
  
  

bechdel.passes =   ds9 %>% 
  filter(`Bechdel-Wallace` =="Pass")

bechdel.passes %>% view()

ds9 %>% 
    filter(`Bechdel-Wallace` =="Pass") %>% 
    select(Season,Episode,Writer_Gender,Director_Gender) %>% view()
    ggplot( aes(x= Season))+
    geom_bar(fill="#FFA747")+
  theme(plot.subtitle = element_text(size = 11),
        plot.caption = element_text(family = "mono",
                                    size = 13, colour = "gray", hjust = 0),
        panel.grid.major = element_line(colour = "gray23"),
        panel.grid.minor = element_line(colour = "gray23"),
        axis.text = element_text(size = 12, colour = "gray"),
        plot.title = element_text(colour = "white"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) +
  labs(title = "Bechdel-Wallace Passes by Season",
       caption = "@StarTrek_Lt") + 
  theme(axis.title = element_text(size = 13,colour = "white")) + 
  theme(legend.text = element_text(size = 12,colour = "white"), 
        legend.title = element_text(size = 12,colour = "white"), 
        legend.key = element_rect(fill = "gray100",
                                  colour = "white", size = 0.7), 
        legend.background = element_rect(fill = "gray0",size = 0.9))
  
  
  
  
  
  
  
  
  
  

                                                                                                                      
ds9 %>% 
  filter(`Bechdel-Wallace` =="Pass") %>% 
  select(Season,Episode,Writer_Gender) %>% 
  ggplot( aes(x= Season, y=Episode))+
  geom_point( )+
  coord_flip()+
  scale_color_gradientn(colours= rainbow(5) )  # rainbow(10) terrain.colors



ds9 %>% 
  select(Season, Director_Gender, Writer_Gender) %>% 
  group_by(Season,Writer_Gender) %>% 
  filter(Writer_Gender =="Female") %>% 
  count() %>% 
  ggplot( aes(x= Season, y= n))+
  geom_col(fill = '#9999CD' )+
  labs(title = "Female Writers by Season on DS9",
       y="Count" ) +
  theme(plot.subtitle = element_text(size = 11),
        plot.caption = element_text(family = "mono",
                                    size = 13, colour = "gray", hjust = 0),
        panel.grid.major = element_line(colour = "gray23"),
        panel.grid.minor = element_line(colour = "gray23"),
        axis.text = element_text(size = 12, colour = "gray"),
        plot.title = element_text(colour = "white"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) +
  labs(caption = "@StarTrek_Lt") + 
  theme(axis.title = element_text(size = 13,colour = "white")) + 
  theme(legend.text = element_text(size = 12, colour = "white"), 
        legend.title = element_text(size = 12,colour = "white"), 
        legend.key = element_rect(fill = "gray100",colour = "white", size = 0.7), 
        legend.background = element_rect(fill = "gray0",size = 0.9))
  
  
  
  
bechdel.passes %>% 
  group_by(Season)
  
# =========================





ds9 %>% 
  group_by(Writer_Gender) %>% 
  count() %>% 
  ggplot( aes(x= n, y= Writer_Gender)) +
  geom_col( fill= '#C4DE23' ) +
  labs(subtitle ="Gender groupings of writers by episode" , x='Count', y='Writer Genders') +
  geom_text(aes(label= n, vjust= .5, hjust= -0.5 ), color= '#C4DE23', size= 6) +
  theme(plot.subtitle = element_text(size = 11),
        plot.caption = element_text(family = "mono",
                                    size = 13, colour = "gray", hjust = 0),
        panel.grid.major = element_line(colour = "gray23"),
        panel.grid.minor = element_line(colour = "gray23"),
        axis.text = element_text(size = 12, colour = "gray"),
        plot.title = element_text(colour = "white"),
        panel.background = element_rect(fill = "gray0"),
        plot.background = element_rect(fill = "black")) +
  labs(caption = "@StarTrek_Lt") + 
  theme(axis.title = element_text(size = 13,colour = "white")) + 
  theme(legend.text = element_text(size = 12, colour = "white"), 
        legend.title = element_text(size = 12,colour = "white"), 
        legend.key = element_rect(fill = "gray100",colour = "white", size = 0.7), 
        legend.background = element_rect(fill = "gray0",size = 0.9)) + 
  theme(plot.subtitle = element_text(size = 13,colour = "white"))
  
  
  
# ====================


ds9 = ds9 %>% mutate(Director_Gender = as.factor(Director_Gender) ) # 2= Male, 1= Female
ds9 = ds9 %>% mutate(Writer_Gender = as.factor(Writer_Gender) )

ds9 = ds9 %>% mutate(Bechdel = `Bechdel-Wallace`,
                     Bechdel = as.factor(Bechdel))# 2= Pass 1= Fail


ds9 = ds9 %>% 
  mutate(Director_Gender = as.integer(ds9$Director_Gender),
         Writer_Gender = as.integer(ds9$Writer_Gender),
         `Bechdel-Wallace` = as.integer(ds9$`Bechdel-Wallace`)) %>% view()

  
  

#----------- Linear Regression with Factors
# convert factors to numeric
ds9$Bechdel = as.numeric(ds9$Bechdel)
ds9 = ds9 %>% mutate(Director_fct = as.factor(ds9$Director))

output = lm(Bechdel ~ Director_Gender+Writer_Gender+Season, data = ds9)
output

summary(output)

# abline( output)









 ds9 = ds9[!duplicated(ds9), ]




library(stringr)

ds9 %>% select(Written_By, Bechdel) %>% 
  mutate(Written_By = as.factor(Written_By)) %>% 
  group_by(Written_By, Bechdel) %>% 
  count() 











