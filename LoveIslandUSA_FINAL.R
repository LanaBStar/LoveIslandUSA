#Let's open our libraries!
library(dplyr)
library(ggplot2)

#Now we will load in our Love Island USA csv
loveisland<- read.csv("~/Desktop/LoveIslandUSA/LoveIslandUSA.csv")


#And let's load a vector of pretty, Love Island-y colors to use for our data viz!
custom_colorsb <- c("#ff80ff", "#37b8eb","#edff80","#67b5a1","#8e44ad","#f8c471","#DAF7A6","#C70039", "#af7ac5","#85c1e9","#ffb3b3", "#99ffcc","#ff9933", "#006699")

#First, we need to filter out the second appearance of Islanders who were on more than one season of Love Island USA so they aren't counted twice in our data visualizations and other analysis
loveislandnorepeats <- loveisland %>% filter(Have.they.appeared.on.a.previous.season.of.LI.USA.== "No")


#AGE OF ISLANDERS
#age of islanders broken down by sex (here I am leaving Islanders who appeared on multiple seasons since they appeared as two different ages, which should still count as a data point)

age <- loveisland %>% 
  group_by(Age, Sex) %>% 
  summarise(total = sum(n())) %>% 
  arrange(desc(total))



#Data Viz for ages/sex of Islanders across the seasons
ggplot(age) +
  geom_col(position = "dodge",# dodge separates the bars instead of stacking them
           mapping = aes(
             x = Age,
             y = total,
             fill = Sex)) +
  scale_fill_manual(values = custom_colorsb) +
  scale_x_continuous(breaks = seq(from=21, to = 32, by = 1)) +
  scale_y_continuous(breaks = seq(from=0, to = 24, by = 1)) +
  labs(
    title = "Ages of Love Island USA Islanders Across Six Seasons", # plot title
    subtitle = "The majority of Islanders have been a youthful 24-years-old",
    x = "Age", # x-axis label
    y = "Total # of Islanders", # y-axis label
  )


#Ages/Sex of Winners 
winnersexage <- loveisland %>%
  filter(Result == "Winner") %>%
  group_by(Age, Sex) %>%
  summarise(total = n()) %>%
  arrange(desc(total))


#Data Viz for ages/sex of Islanders in winning couples
ggplot(winnersexage) +
  geom_col(position = "dodge",# dodge separates the bars instead of stacking them
           mapping = aes(
             x = Age,
             y = total,
             fill = Sex)) +
  scale_fill_manual(values = custom_colorsb) +
  scale_x_continuous(breaks = seq(from=21, to = 32, by = 1)) +
  scale_y_continuous(breaks = seq(from=0, to = 24, by = 1)) +
  labs(
    title = "Ages of Winners of Love Island USA Across Six Seasons", # plot title
    subtitle = "",
    x = "Age", # x-axis label
    y = "Total # of Islanders", # y-axis label
  )


#DAY ONE TO WIN?

#WINNERS/Runner-UPS DAY THEY ENTERED 

#What days did winning Islanders enter the Villa?
winonly <- loveisland %>% 
  filter(Result =="Winner") %>% 
  group_by(Day.Entered.Villa) %>% 
  summarise(total = n())

#Data Viz for what day WINNERS entered the villa
ggplot(winonly) +
  geom_col(
    show.legend = FALSE,
    mapping = aes(
      x = reorder(Day.Entered.Villa, -total),
      y = total
    ),
    fill = "#ff80ff" # Set the color outside of aes()
  ) +
  scale_y_continuous(
    breaks = seq(0, max(winonly$total), by = 1)) + #this creates the lines along the Y axis, and indicates the lines start at zero, with the highest break being the total of the numbers in the column "total" within the dataframe toptwoonly. The breaks along the y axis are in increments of 1 
  labs(
    title = "When the Winning Islanders Entered the Villa", # plot title
    subtitle = "Over six seasons, nothing has more reliably predicted success than being in the villa on day one",
    x = "Day They Entered the Villa", # x-axis label
    y = "Total # of Winning Islanders", # y-axis label
  )


#What day did RUNNER-UPS enter the Villa?
runneruponly <- loveisland %>% 
  filter(Result %in% c("Runner-Up")) %>% 
  group_by(Day.Entered.Villa) %>% 
  summarise(total = n())

#Dat Viz for what day RUNNER-UPS entered the Villa
ggplot(runneruponly) +
  geom_col(show.legend = FALSE,
           mapping = aes(
             x = reorder(Day.Entered.Villa, -total),
             y = total),
           fill = "#8e44ad"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(runneruponly$total), by = 1)) + #this creates the lines along the Y axis, and indicates the lines start at zero, with the highest break being the total of the numbers in the column "total" within the dataframe toptwoonly. The breaks along the y axis are in increments of 1 
  labs(
    title = "When the Runner-Up Islanders Entered the Villa", # plot title
    subtitle = "Success favors those who start early: even second place is tied to day one presence",
    x = "Day They Entered the Villa", # x-axis label
    y = "Total # of Runner-Up Islanders", # y-axis label
  )



#How many days in total are the majority of Day One Islanders in the villa for?
dayones <- loveisland %>% 
  filter(Day.Entered.Villa == "1") %>% 
  group_by(Days.in.Villa) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) 

#Data Viz for amount of days Day Ones stayed in Villa 
ggplot(dayones) +
  geom_col(
    mapping = aes(
      x = reorder(Days.in.Villa,-total),
      y =total,
      fill = Days.in.Villa)) +
  labs(
    title = "How Long Do Day Ones Last In The Villa?",
    subtitle = "",
    x = "Days in Villa",
    y = "Total"
  )


#What is the most common length of stay in the villa for an Islander who entered any day AFTER day one?
dayslateentry <- loveisland %>% 
  filter(Day.Entered.Villa >1) %>%  
  group_by(Days.in.Villa) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) 


#Data Viz for amount of days Islanders were in Villa if they AREN'T a Day One 
ggplot(dayslateentry) +
  geom_col(
    mapping = aes(
      x = reorder(Days.in.Villa,-total),
      y =total,
      fill = Days.in.Villa)) +
  labs(
    title = "How Long Do Islanders Last If They Aren't Day Ones?",
    x = "Days in Villa",
    y = "Total"
  )


#OCCUPATIONS

#Top Occupations of all the Islanders, listed in descending order
occupations <- loveislandnorepeats %>% 
  filter(Occupation != "") %>% 
  group_by(Occupation) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) 

#Occupations of the WINNING Islanders
occupations_winners <- loveisland %>% 
  filter(Occupation != "") %>% 
  filter(Result == "Winner")
group_by(Occupation) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) 

#Data Viz showing the Top 3 Occupations of ALL Islanders  
occupationstop3 <- occupations %>% 
  slice(1:3)

ggplot(occupationstop3) +
  geom_col(
    mapping = aes(
      x = reorder(Occupation, -total),
      y = total,
      fill = Occupation)) +
  scale_fill_manual(values = c("#ff80ff", "#37b8eb", "#8e44ad")) +
  scale_y_continuous(
    breaks = seq(0, 20, by = 1))+
  labs(
    title = "Top Occupations of Islanders on Love Island USA", # plot title
    subtitle = "",
    x = "Occupation", # x-axis label
    y = "Total # of Islanders", # y-axis label
  ) 

#Not included in the written project, but just for fun...ZODIAC STUFF
#zodiac signs of islanders broken down by sex:  
zodiacbysex<- loveislandnorepeats %>% 
  filter(Zodiac != "") %>% 
  group_by(Zodiac,Sex) %>% 
  summarise(total = sum(n())) %>% 
  arrange(desc(total))

#Islander zodiac count(not broken down by sex)
zodiacfiltered <- loveislandnorepeats %>% 
  filter(Zodiac != "") %>% 
  group_by(Zodiac) %>% 
  summarise(total = sum(n())) %>% 
  slice_max(n= 5, order_by = total) %>% 
  arrange(desc(total))

#Data Viz of the most common Zodiacs of the total group of Islanders
ggplot(zodiacfiltered) +
  geom_col(
    mapping = aes(
      x = reorder(Zodiac, -total),
      y = total,
      fill = Zodiac)) +
  scale_fill_manual(values = custom_colorsb) +
  labs(
    title = "Most common Zodiacs in the Villa?", # plot title
    subtitle = "",
    x = "Zodiac Sign", # x-axis label
    y = "Total #", # y-axis label
  )