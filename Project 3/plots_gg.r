# Georgia Galanopoulos

library(DBI)
library(RMySQL)
library(tidyr)
library(dplyr)
library(ggplot2)


connection = dbConnect(MySQL(), user='root', password='data607pw', host='35.185.104.222', dbname='datascienceskills')

data = dbGetQuery(connection, 
	"SELECT Location.Description, Location.Country, 
		Skills.SkillDescription, Categories.Description, 
		skillsdata.Amount, skillsdata.Rating, skillsdata.YearCollected
	FROM skillsdata 
	LEFT JOIN (Skills 
	LEFT JOIN Categories 
		ON Skills.SkillCategory = Categories.CategoryID, Location)
		ON (Skills.SkillID = skillsdata.Skill 
			AND Location.LocationID = skillsdata.Place);")

colnames(data) = c("Location", "Country", "Skill", 
	"SkillType", "Amount", "Rating", "Year")

# Let's take the Countries and make stats for each of them, okay?
# Non-US countries
# Make a chart with mean Amount and mean Rating for each skill
# No Year comparisons included because only 2017 data available for non-US countries
data %>% 
  filter(Country != "US") %>% 
  group_by(Country, Location, Skill) %>% 
  summarise(Mean_Amount = mean(Amount),
            Mean_Rating = mean(Rating)) %>% 
  group_by(Country) %>% 
  top_n(5, Mean_Rating) %>% 
ggplot(aes(x=Skill, y=Mean_Rating, colour = Skill)) + 
  geom_bar( aes(fill= Skill), stat="identity", position=position_dodge())+
  facet_grid(~ Country) +
  labs(title = "Top 5 skills by Rating in Countries Abroad") +
  labs(subtitle = 'For Year 2017')+ 
  theme(axis.text.x = element_text(angle=90))


# And now we can look at the US data
# and it's a mess side by side with the years in one graph,
data %>% 
  filter(Country == "US") %>% 
  group_by(Year, Location, Skill) %>% 
  summarise(Mean_Amount = mean(Amount),
            Mean_Rating = mean(Rating)) %>% 
  group_by(Year, Location) %>%
  top_n(5, Mean_Rating) %>% 
ggplot(aes(x= Year, y=Mean_Rating)) + 
  geom_bar(aes(fill= factor(Skill)), colour = "black", stat="identity", position=position_dodge())+
  facet_grid(~ Location) +
  labs(title = "Top 5 skills by Rating in US") +
  labs(subtitle = 'For Years 2017 and 2016')

# So, we can split it up by years
# 2016
data %>% 
  filter(Country == "US") %>% 
  filter(Year == "2016") %>% 
  group_by(Year, Location, Skill) %>% 
  summarise(Mean_Amount = mean(Amount),
            Mean_Rating = mean(Rating)) %>% 
  group_by(Year, Location) %>%
  top_n(5, Mean_Rating) %>% 
ggplot(aes(x= reorder(Skill, Mean_Rating), y=Mean_Rating)) + 
  geom_bar(aes(fill= factor(Skill)), colour = "black", stat="identity", position=position_dodge())+
  facet_grid(~ Location) +
  labs(title = "Top 5 skills by Rating in US") +
  labs(subtitle = 'For Year 2016')+ 
  theme(axis.text.x = element_text(angle=90))

# and 2017
data %>% 
  filter(Country == "US") %>% 
  filter(Year == "2017") %>% 
  group_by(Year, Location, Skill) %>% 
  summarise(Mean_Amount = mean(Amount),
            Mean_Rating = mean(Rating)) %>% 
  group_by(Year, Location) %>%
  top_n(5, Mean_Rating) %>% 
ggplot(aes(x= reorder(Skill, Mean_Rating), y=Mean_Rating)) + 
  geom_bar(aes(fill= factor(Skill)), colour = "black", stat="identity", position=position_dodge())+
  facet_grid(~ Location) +
  labs(title = "Top 5 skills by Rating in US") +
  labs(subtitle = 'For Year 2017')+ 
  theme(axis.text.x = element_text(angle=90))

