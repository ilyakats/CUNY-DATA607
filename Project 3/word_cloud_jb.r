# Jaan Bernberg

library(wordcloud2)

my_cloud_words <- all_data[,3:7] %>%
  filter(YearCollected == 2017) %>% 
  select(SkillDescription, Amount) %>% 
  group_by(SkillDescription) %>% 
  summarise(Amount = sum(Amount)) %>% 
  arrange(desc(Amount)) %>% as.data.frame()

rownames(my_cloud_words) <- my_cloud_words$SkillDescription

wordcloud2(my_cloud_words, size = 1.5, 
           minRotation = -pi/6, 
           maxRotation = -pi/6,
           rotateRatio = 1)
