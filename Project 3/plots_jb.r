# Jaan Bernberg

library(dplyr)
library(tidyr)
library(RCurl)
library(RMySQL)
library(ggplot2)

#------ Obtain Data ----------------------------------------------------------
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "datascienceskills",
                 host = "35.185.104.222",
                 port = 3306,
                 user = "root",
                 password = "data607pw")

all_data <- dbGetQuery(con, "SELECT Location.Description, Location.Country, Skills.SkillDescription, Categories.Description, skillsdata.Amount, skillsdata.Rating, skillsdata.YearCollected
                       FROM skillsdata LEFT JOIN (Skills LEFT JOIN Categories ON Skills.SkillCategory = Categories.CategoryID, Location)
                       ON (Skills.SkillID = skillsdata.Skill AND Location.LocationID = skillsdata.Place);")
dbDisconnect(con)

#------ 2017 data for plotting -----------------------------------------------
my_plot_data <- all_data[,3:7] %>% 
  group_by(SkillDescription, Description, YearCollected) %>% 
  select(SkillDescription, Description, Amount, YearCollected) %>% 
  summarise(Amount = sum(Amount)) %>% 
  spread(YearCollected, Amount)

# get totals of colums for denom:  
tot_16 <- sum(my_plot_data$`2016`, na.rm = TRUE)
tot_17 <- sum(my_plot_data$`2017`, na.rm = TRUE)

# calculate rate columns:  
my_plot_data <- my_plot_data %>% 
  mutate(rt_2016 = `2016`/tot_16,
         rt_2017 = `2017`/tot_17) %>%
  mutate(All_16 = `2016`, All_17 = `2017`) %>% 
  select(SkillDescription, Description, All_16, rt_2016, All_17, rt_2017)

# replace NA's with zeros
my_plot_data[is.na(my_plot_data)] <- 0

# arrange data from high to low on All_17, then by Description
my_plot_data <- my_plot_data %>% arrange(Description, All_17)

# set plot data as arranged: 
my_plot_data$SkillDescription <- factor(my_plot_data$SkillDescription, 
                                        my_plot_data$SkillDescription)

#------ Year Over Year data for plotting -------------------------------------

# arrange data from high to low on All_17, then by Description
my_plot_data_yoy <- my_plot_data %>% ungroup() %>% 
  mutate(SkillDescription = as.character(SkillDescription)) %>% 
  arrange(All_17) %>% 
  select(SkillDescription, Description, starts_with("rt_"))

# set plot data as arranged: 
my_plot_data_yoy$SkillDescription <- factor(my_plot_data_yoy$SkillDescription, 
                                            as.character(my_plot_data_yoy$SkillDescription))
# tidy for plot
my_plot_data_yoy <- my_plot_data_yoy %>% 
  gather("Year", "% of Totals", starts_with("rt_"))

# for labels, later on. 
descr_labels <- c("Analysis","Big Data", "Database", "Program Lang.")

#------ First plot, Bar Plot of Key Words by Description ---------------------
my_plot <- ggplot(my_plot_data, aes(x=SkillDescription, y=All_17))

my_plot + geom_bar(stat = "identity", alpha = .5, aes(fill = Description)) + 
  coord_flip() + 
  theme(panel.grid.major = element_line(colour = "gray"),
        legend.position = c(.8, .85), 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("March 2017 Keywords", subtitle = 'Counts of Keywords') +
  scale_y_continuous(sec.axis = dup_axis()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_discrete(name = "Area", 
                      labels = descr_labels)


#------ 2nds plot, Bar Plot of Key Words YoY ---------------------------------
my_yoy_plot <- ggplot(my_plot_data_yoy, aes(x = SkillDescription, 
                                            y = `% of Totals`, 
                                            fill = `Year`))

my_yoy_plot + geom_bar(stat = "identity", alpha = .5, position = "dodge") + 
  coord_flip() +
  ggtitle("March 2016 v March 2017", subtitle = '% of Totals') +
  scale_y_continuous(sec.axis = dup_axis(), labels = scales::percent) +
  theme(panel.grid.major = element_line(colour = "gray"),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_discrete(name = "Year", labels = c("2016","2017")) + 
  facet_wrap(~Description, scales = "free_y")