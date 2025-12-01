library(stringr)
library(dplyr)
library(udpipe)
install.packages("writexl") 
library(writexl)

# модель для норвежского
model <- udpipe_download_model(language = "norwegian-bokmaal")
model <- udpipe_load_model(model$file_model)

# Извлечение слов из сырых данных
get_all_coll_words <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, header = FALSE)
  all_text <- paste(unlist(data), collapse = " ")
  words <- str_extract_all(all_text, "(?<=<coll>)[^<]+(?=</coll>)")[[1]]
  return(words)
}

all_words <- get_all_coll_words("filename.csv")

# Лемматизация + регистр
lemmatized <- udpipe_annotate(model, x = all_words)
lemmatized_words <- as.data.frame(lemmatized)$lemma
lemmatized_words_lower <- tolower(lemmatized_words)

# Сортировка по частотности
word_freq <- table(lemmatized_words_lower)
word_freq_sorted <- sort(word_freq, decreasing = TRUE)

# Сохранение файла
df <- as.data.frame(word_freq_sorted)
write_xlsx(df, "filename.xlsx")

### Анализ

# Кол-во уникальных слов
unique_count <- length(unique(lemmatized_words_lower))
cat("Количество уникальных слов:", unique_count, "\n")

# Кол-во гапаксов
words_once <- sum(table(lemmatized_words_lower) == 1)
words_once_count <- length(which(table(lemmatized_words_lower) == 1))
cat("Количество слов, встретившихся только 1 раз:", words_once, "\n")

# 15 самых частотных слов
top_15 <- head(word_freq_sorted, 15)
total_words <- length(lemmatized_words_lower)
top_15 <- head(sort(table(lemmatized_words_lower), decreasing = TRUE), 15)

# Процент для топ-15
top_15_table <- data.frame(
  word = names(top_15),
  frequency = as.numeric(top_15),
  percentage = round(as.numeric(top_15) / total_words * 100, 2)
)

cat("\nТоп-15 слов (слово | частота | %):\n")
for(i in 1:nrow(top_15_table)) {
  cat(sprintf("%-15s | %4d | %5.2f%%\n", 
              top_15_table$word[i], 
              top_15_table$frequency[i], 
              top_15_table$percentage[i]))
}


