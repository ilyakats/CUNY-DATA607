library(XML)
library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyjson)
library(urltools)
library(tidytext)
library(ggplot2)
library(RTextTools)

urlRoot <- "https://en.wikipedia.org"
urlMain <- "/wiki/AFI_100_Years..._series"
links <- getURL(str_c(urlRoot, urlMain))
links <- htmlParse(links)
links <- xpathSApply(links, "//div[@id='mw-content-text']//table[@class='infobox']//a", xmlGetAttr, "href")
links <- links[str_detect(links, "^((?!Template).)*$")]

links

films <- data.frame(film=character(), year=integer())

for(i in 1:(length(links)-1)) {
  url <- getURL(str_c(urlRoot, links[i]))
  tmp <- readHTMLTable(url, which = 2)
  if (any(str_detect(str_to_lower(colnames(tmp)), "film")) && 
      any(str_detect(str_to_lower(colnames(tmp)), "year"))) {
    tmp <- tmp[, c(which(str_detect(str_to_lower(colnames(tmp)), "film")), 
                   which(str_detect(str_to_lower(colnames(tmp)), "year")))]
    colnames(tmp) <- c("film", "year")
    
    films <- rbind(films, tmp)
  }
}

for(i in 2:11) {
  url <- getURL(str_c(urlRoot, links[length(links)]))
  tmp <- readHTMLTable(url, which = i)
  if (any(str_detect(str_to_lower(colnames(tmp)), "film")) && 
      any(str_detect(str_to_lower(colnames(tmp)), "year"))) {
    tmp <- tmp[, c(which(str_detect(str_to_lower(colnames(tmp)), "film")), 
                   which(str_detect(str_to_lower(colnames(tmp)), "year")))]
    colnames(tmp) <- c("film", "year")
    
    films <- rbind(films, tmp)
  }
}

films <- films %>% 
  group_by(film, year) %>% 
  summarize() 

films <- as.data.frame(films)

# I have noticed that there are definitely a handful of incorrectly formatted movie titles; however, we are not looking for perfection.

film_plots <- data.frame(film=character(), year=integer(), genre=character(), imdbID=character(), plot=character())

for(i in 1:nrow(films)) {
  title <- as.character(films[i,1])
  title <- url_encode(title)
  year <- as.character(films[i,2])
  
  url <- str_c("http://www.omdbapi.com/?t=", title, "&y=", year, "&type=movie&plot=full")
  request <- getURL(url)
  if(jsonlite::validate(request)) {
    response <- request %>% spread_values(response = jstring("Response"))
    if(response$response=="True") {
      request <- request %>% spread_values(title = jstring("Title"), 
                                           year = jstring("Year"), 
                                           genre = jstring("Genre"), 
                                           imdbID = jstring("imdbID"), 
                                           plot = jstring("Plot")) %>% 
        select(title, year, genre, imdbID, plot)
      film_plots <- rbind(film_plots, request)
    }
  }
  Sys.sleep(1)
}

film_plots <- film_plots %>% 
  group_by(title, year, genre, imdbID, plot) %>% 
  summarize() 

saveRDS(film_plots, file = "film_plots.rds")
write.csv(film_plots, file = "film_plots.csv", row.names = FALSE)

film_plots["class"] <- NA
film_plots[str_detect(film_plots$genre, "Drama")!=TRUE & str_detect(film_plots$genre, "Comedy")==TRUE, 6] <- "Comedy"
film_plots[str_detect(film_plots$genre, "Drama")==TRUE & str_detect(film_plots$genre, "Comedy")!=TRUE, 6] <- "Drama"

table(film_plots$class)

film_plots <- film_plots %>% 
  ungroup() %>% 
  filter(!is.na(class)) %>% 
  select(title, year, class, imdbID, plot)

data("stop_words")

tidy_plots <- film_plots %>% 
  unnest_tokens(word, plot) %>% 
  anti_join(stop_words)

tidy_plots %>% 
  group_by(class, word) %>% 
  summarize(count = n()) %>% 
  filter(class == "Comedy") %>% 
  arrange(desc(count)) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, count)) %>% 
  ggplot(aes(word, count)) +
  geom_bar(stat = "identity", fill = "red") + coord_flip()

tidy_plots %>% 
  group_by(class, word) %>% 
  summarize(count = n()) %>% 
  filter(class == "Drama") %>% 
  arrange(desc(count)) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, count)) %>% 
  ggplot(aes(word, count, fill = class)) +
  geom_bar(stat = "identity", fill = "blue") + coord_flip()

plot_words <- tidy_plots %>% 
  count(class, word, sort = TRUE) %>% 
  ungroup()

total_words <- plot_words %>% 
  group_by(class) %>% 
  summarize(total = sum(n))

plot_words <- left_join(plot_words, total_words)

plot_words

plot_words <- plot_words %>% 
  bind_tf_idf(word, class, n)

plot_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(class == "Comedy") %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = "red") + coord_flip()

plot_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(class == "Drama") %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = "blue") + coord_flip()
tidy_plots %>% filter (word == "woody")

# Collect additional films

url <- getURL("https://www.loc.gov/programs/national-film-preservation-board/film-registry/complete-national-film-registry-listing/")
tmp <- readHTMLTable(url, which = 1)
tmp <- tmp[, 1:2]
colnames(tmp) <- c("film", "year")
tmp

film_plots2 <- data.frame(film=character(), year=integer(), genre=character(), imdbID=character(), plot=character())

for(i in 1:nrow(tmp)) {
  title <- as.character(tmp[i,1])
  title <- url_encode(title)
  year <- as.character(tmp[i,2])
  
  url <- str_c("http://www.omdbapi.com/?t=", title, "&y=", year, "&type=movie&plot=full")
  request <- getURL(url)
  if(jsonlite::validate(request)) {
    response <- request %>% spread_values(response = jstring("Response"))
    if(response$response=="True") {
      request <- request %>% spread_values(title = jstring("Title"), 
                                           year = jstring("Year"), 
                                           genre = jstring("Genre"), 
                                           imdbID = jstring("imdbID"), 
                                           plot = jstring("Plot")) %>% 
        select(-document.id)
      film_plots2 <- rbind(film_plots2, request)
    }
  }
  Sys.sleep(1)
}

film_plots2 <- film_plots2 %>% 
  group_by(title, year, genre, imdbID, plot) %>% 
  summarize() 

film_plots2["class"] <- NA
film_plots2[str_detect(film_plots2$genre, "Drama")!=TRUE & str_detect(film_plots2$genre, "Comedy")==TRUE, 6] <- "Comedy"
film_plots2[str_detect(film_plots2$genre, "Drama")==TRUE & str_detect(film_plots2$genre, "Comedy")!=TRUE, 6] <- "Drama"

table(film_plots2$class)

film_plots2 <- film_plots2 %>% 
  ungroup() %>% 
  filter(!is.na(class)) %>% 
  select(title, year, class, imdbID, plot)

film_plots <- rbind(film_plots, film_plots2)
film_plots <- film_plots %>% 
  group_by(title, year, class, imdbID, plot) %>% 
  summarize() 

film_plots <- film_plots %>% 
  filter(plot != "N/A")

saveRDS(film_plots, file = "film_plots2.rds")
write.csv(film_plots, file = "film_plots2.csv", row.names = FALSE)

data("stop_words")

tidy_plots <- film_plots %>% 
  unnest_tokens(word, plot) %>% 
  anti_join(stop_words)

plot_words <- tidy_plots %>% 
  count(class, word, sort = TRUE) %>% 
  ungroup()

total_words <- plot_words %>% 
  group_by(class) %>% 
  summarize(total = sum(n))

plot_words <- left_join(plot_words, total_words)

plot_words

plot_words <- plot_words %>% 
  bind_tf_idf(word, class, n)

plot_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(class == "Comedy") %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = "red") + coord_flip()

plot_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(class == "Drama") %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = "blue") + coord_flip()


plot_bigrams <- film_plots %>%
  unnest_tokens(bigram, plot, token = "ngrams", n = 2)

plot_bigrams <- plot_bigrams %>% 
  count(class, bigram, sort = TRUE) %>% 
  ungroup()

total_bigrams <- plot_bigrams %>% 
  group_by(class) %>% 
  summarize(total = sum(n))

plot_bigrams <- 
  left_join(plot_bigrams, total_bigrams)

plot_bigrams <- plot_bigrams %>% 
  bind_tf_idf(bigram, class, n)

plot_bigrams %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(class == "Comedy") %>% 
  top_n(10) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(bigram, n)) +
  geom_bar(stat = "identity", fill = "red") + coord_flip()

plot_bigrams %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(class == "Drama") %>% 
  top_n(10) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(bigram, n)) +
  geom_bar(stat = "identity", fill = "red") + coord_flip()


dtm <- tidy_plots %>%
  group_by(imdbID, word) %>% 
  summarize(count = n()) %>% 
  cast_dtm(imdbID, word, count)

matrix <- create_matrix(film_plots$plot, language="english",
                        removeNumbers=TRUE, stemWords=TRUE, weighting=tm::weightTf)

container <- create_container(dtm, film_plots$class, trainSize = 1:375, testSize = 376:length(film_plots$class), virgin = FALSE)
container <- create_container(matrix, film_plots$class, trainSize = 1:375, testSize = 376:length(film_plots$class), virgin = FALSE)

svm_model <- train_model(container, "SVM")
tree_model <- train_model(container, "TREE")
maxent_model <- train_model(container, "MAXENT")

svm_out <- classify_model(container, svm_model)
tree_out <- classify_model(container, tree_model)
maxent_out <- classify_model(container, maxent_model)

head(svm_out)
head(tree_out)
head(maxent_out)

labels_out <- data.frame(plot = film_plots$plot[376:length(film_plots$plot)],
                         correct_label = film_plots$class[376:length(film_plots$class)],
                         svm = as.character(svm_out[,1]),
                         tree = as.character(tree_out[,1]),
                         maxent = as.character(maxent_out[,1]),
                         stringsAsFactors = FALSE)

prop.table(table(labels_out$correct_label == labels_out$svm))
prop.table(table(labels_out$correct_label == labels_out$tree))
prop.table(table(labels_out$correct_label == labels_out$maxent))


table(labels_out$correct_label == labels_out$tree)
table(labels_out$svm)
table(labels_out$tree)
table(labels_out$maxent)

bad_predictions <- labels_out[labels_out$correct_label != labels_out$tree, ]

classNo <- as.numeric(as.factor(film_plots$class))
container <- create_container(matrix, classNo, trainSize = 1:375, testSize = 376:length(film_plots$class), virgin = FALSE)
maxent_model <- train_model(container, "MAXENT")
maxent_out <- classify_model(container, maxent_model)
analytics <- create_analytics(container, maxent_out, b=1)
analytics@label_summary


tmp <- read.csv("films3.csv")

film_plots3 <- data.frame(film=character(), year=integer(), genre=character(), imdbID=character(), plot=character())

for(i in 1:nrow(tmp)) {
  title <- as.character(tmp[i,1])
  title <- url_encode(title)
  year <- as.character(tmp[i,2])
  
  url <- str_c("http://www.omdbapi.com/?t=", title, "&y=", year, "&type=movie&plot=full")
  request <- getURL(url)
  if(jsonlite::validate(request)) {
    response <- request %>% spread_values(response = jstring("Response"))
    if(response$response=="True") {
      request <- request %>% spread_values(title = jstring("Title"), 
                                           year = jstring("Year"), 
                                           genre = jstring("Genre"), 
                                           imdbID = jstring("imdbID"), 
                                           plot = jstring("Plot")) %>% 
        select(-document.id)
      film_plots3 <- rbind(film_plots3, request)
    }
  }
  Sys.sleep(1)
}

film_plots3 <- film_plots3 %>% 
  group_by(title, year, genre, imdbID, plot) %>% 
  summarize() 

film_plots3["class"] <- NA
film_plots3[str_detect(film_plots3$genre, "Drama")!=TRUE & str_detect(film_plots3$genre, "Comedy")==TRUE, 6] <- "Comedy"
film_plots3[str_detect(film_plots3$genre, "Drama")==TRUE & str_detect(film_plots3$genre, "Comedy")!=TRUE, 6] <- "Drama"

table(film_plots3$class)

film_plots3 <- film_plots3 %>% 
  ungroup() %>% 
  filter(!is.na(class)) %>% 
  select(title, year, class, imdbID, plot)

film_plots <- read.csv("film_plots2.csv")
film_plots <- rbind(film_plots, as.data.frame(film_plots3))

film_plots <- film_plots %>% 
  group_by(title, year, class, imdbID, plot) %>% 
  summarize() 

film_plots <- film_plots %>% 
  filter(plot != "N/A")

write.csv(film_plots, file = "film_plots3.csv", row.names = FALSE)

film_plots <- film_plots[sample(nrow(film_plots)),]
