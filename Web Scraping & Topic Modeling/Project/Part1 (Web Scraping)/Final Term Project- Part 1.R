library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(textclean)
library(tm)
library(hunspell)
library(textstem)
library(stopwords)
library(pbapply)


categories <- c("tag/israel-palestine-conflict/", "sports/", "tag/india-pakistan-tensions/")


scrape_full_content <- function(link) {
  tryCatch({
    page <- read_html(link)
    
    content <- page %>%
      html_nodes("div.wysiwyg--all-content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    
    return(content)
  }, error = function(e) {
    return(NA)
  })
}


scrape_news <- function(category, num_articles = 100) {
  base_url <- "https://www.aljazeera.com/"
  articles <- data.frame()
  page <- 1
  
  while (nrow(articles) < num_articles) {
    cat("Scraping:", category, "Page:", page, "\n")
    full_url <- paste0(base_url, category, "?page=", page)
    webpage <- tryCatch(read_html(full_url), error = function(e) return(NULL))
    if (is.null(webpage)) break
    
    
    titles <- html_nodes(webpage, ".gc__title") %>% html_text(trim = TRUE)
    descriptions <- html_nodes(webpage, ".gc__excerpt") %>% html_text(trim = TRUE)
    dates <- html_nodes(webpage, ".gc__meta") %>% html_text(trim = TRUE)
    links <- html_nodes(webpage, ".gc__title a") %>% html_attr("href")
    links <- paste0("https://www.aljazeera.com", links)
    
  
    min_len <- min(length(titles), length(descriptions), length(dates), length(links))
    if (min_len == 0) break 
    
    temp_data <- data.frame(
      title = titles[1:min_len],
      description = descriptions[1:min_len],
      date = dates[1:min_len],
      link = links[1:min_len],
      category = rep(category, min_len),
      stringsAsFactors = FALSE
    )
    
    articles <- bind_rows(articles, temp_data)
    page <- page + 1
    Sys.sleep(1)
  }
  
  return(head(articles, num_articles))
}


news_data <- map_df(categories, ~ scrape_news(.x, num_articles = 20))  # Use 20 for testing


news_data$content <- sapply(news_data$link, scrape_full_content)


news_data <- news_data[!is.na(news_data$content) & news_data$content != "", ]



write.csv(news_data, "aljazeera_news_scrapped.csv", row.names = FALSE, fileEncoding = "UTF-8") 

getwd()
head(news_data, 3)




# ------------------------------
preprocess_article_verbose <- function(text) {
  text <- tolower(text)
  text <- replace_contraction(text)
  text <- str_replace_all(text, "<.*?>", " ")
  text <- str_replace_all(text, "[^[:alnum:][:space:]]", " ")
  text <- str_replace_all(text, "[0-9]", " ")
  text <- str_squish(text)
  
  tokens <- unlist(str_split(text, "\\s+"))
  cat(" Tokenization Example:", paste(head(tokens, 5), collapse = " "), "\n")
  
  tokens <- iconv(tokens, to = "ASCII//TRANSLIT")
  cat("Normalized Tokens:", paste(head(tokens, 5), collapse = " "), "\n")
  
  tokens <- tokens[!(tokens %in% stopwords("en"))]
  cat("After Stopword Removal:", paste(head(tokens, 5), collapse = " "), "\n")
  
  tokens <- lemmatize_words(tokens)
  cat("Lemmatized Tokens:", paste(head(tokens, 5), collapse = " "), "\n")
  
  tokens <- tokens[nchar(tokens) > 1]
  cleaned_text <- paste(tokens, collapse = " ")
  return(cleaned_text)
}



preprocess_article_simple <- function(text) {
  text <- tolower(text)
  text <- replace_contraction(text)
  text <- str_replace_all(text, "<.*?>", " ")
  text <- str_replace_all(text, "[^[:alnum:][:space:]]", " ")
  text <- str_replace_all(text, "[0-9]", " ")
  text <- str_squish(text)
  
  
  tokens <- unlist(str_split(text, "\\s+")) 
  tokens <- iconv(tokens, to = "ASCII//TRANSLIT")  
  
  tokens <- tokens[!(tokens %in% stopwords("en"))]
  
  tokens <- lemmatize_words(tokens) 
  tokens <- tokens[nchar(tokens) > 1] 
  paste(tokens, collapse = " ") 
  
}


cat("Preprocessing first article with output:\n")
preprocess_article_verbose(news_data$content[1])


news_data$processed <- sapply(news_data$content, preprocess_article_simple)


write.csv(news_data, "aljazeera_processed_final_file.csv", row.names = FALSE)
cat("\n Preprocessed data saved to: aljazeera_processed_final_file.csv\n")

