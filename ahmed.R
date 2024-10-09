install.packages('tidyverse')
install.packages("ggplot2")
library("tidyverse")
library("ggplot2")

df <- read.csv("c:/Users/yehia/OneDrive/Documents/imdb_top_1000.csv")
view(df)



df$Released_Year <- as.numeric(as.character(df$Released_Year))

df$Runtime <- as.numeric(gsub(" min", "", df$Runtime))


df$Gross <- as.numeric(gsub(",", "", df$Gross))


df <- df %>% drop_na(Released_Year, Runtime)


df$Certificate[is.na(df$Certificate)] <- "Not Rated" 
df$Meta_score[is.na(df$Meta_score)] <- median(df$Meta_score, na.rm = TRUE) 
df$Gross[is.na(df$Gross)] <- 0  


head(df)


str(df)


summary(df)


colSums(is.na(df))
library(tidyr)
df_genres <- df %>% separate_rows(Genre, sep = ", ")
genre_counts <- df_genres %>% count(Genre, sort = TRUE)
library(ggplot2)
ggplot(genre_counts, aes(x = reorder(Genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top Genres in IMDb Top 1000 Movies", x = "Genre", y = "Count")
ggplot(df, aes(x = IMDB_Rating)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of IMDb Ratings", x = "IMDb Rating", y = "Count")
top_directors <- df %>% count(Director, sort = TRUE) %>% top_n(10)
ggplot(top_directors, aes(x = reorder(Director, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 10 Directors in IMDb Top 1000 Movies", x = "Director", y = "Number of Movies")
ggplot(df, aes(x = IMDB_Rating, y = Gross)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "IMDb Rating vs Gross Revenue", x = "IMDb Rating", y = "Gross Revenue") +
  scale_y_continuous(labels = scales::comma)
ggplot(df, aes(x = Runtime)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Distribution of Movie Runtimes", x = "Runtime (minutes)", y = "Count")
install.packages('corrplot')
library('corrplot')
numeric_columns <- df %>% select(IMDB_Rating, No_of_Votes, Gross)

correlation_matrix <- cor(numeric_columns, use = "complete.obs")
library(corrplot)
corrplot(correlation_matrix, method = "circle")
ggplot(df, aes(x = Released_Year, y = IMDB_Rating)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "IMDb Ratings Over Time", x = "Year", y = "IMDb Rating")

df_actors <- df %>% 
  select(Star1, Star2, Star3, Star4) %>% 
  gather(key = "Star_Role", value = "Actor") %>% 
  count(Actor, sort = TRUE) %>% 
  top_n(10)


ggplot(df_actors, aes(x = reorder(Actor, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 Actors in IMDb Top 1000 Movies", x = "Actor", y = "Number of Movies")

write.csv(df, "cleaned_imdb_top_1000.csv", row.names = FALSE)








