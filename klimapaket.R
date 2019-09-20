url = "https://www.bundesregierung.de/resource/blob/997532/1673502/768b67ba939c098c994b71c0b7d6e636/2019-09-20-klimaschutzprogramm-data.pdf?download=1"

library("tm",quietly = T)
library("SnowballC",quietly = T)
library("dplyr",quietly = T)
wahl_word_count = function(link,stem = F) {
  partei_text = pdftools::pdf_text(link) %>%  #returns vector of length of pages!
    strsplit(.,"\n") %>%                  #split vector by row breaks
    unlist() %>%                          #coerce to string vectors
    strsplit(.,"\r") %>%                  #split vector by breaks
    trimws(.) %>%                         #remove whitespaces at beginning or end
    paste0(.,collapse = " ") %>%          #make one large character vector
    gsub("  "," ",.) %>%                  #remove double spaces
    gsub('„',"",.) %>%                    #remove quotation marks
    gsub("“","",.)                        #dito
  
  if (stem == T) { # doesnt work well with german data
    partei_text = stemDocument(partei_text,language = "ger")
  }
  
  
  df = data.frame(doc_id = seq(1,length(partei_text)),
                  text = partei_text, 
                  stringsAsFactors = FALSE)
  
  
  df_corpus = Corpus(DataframeSource(df))
  
  docs = df_corpus %>% 
    tm_map(.,removeWords,stopwords("ger")) %>% #removing stopwords
    tm_map(.,removePunctuation)                #removing punctuation
  
  
  df = TermDocumentMatrix(docs,                #create Matrix of Word counts
                          control = list(tolower = FALSE)) %>% 
    as.matrix(.) %>%                           
    as.data.frame(.) %>% 
    mutate(freq = `1`,
           word = row.names(.)) %>% 
    select(word,freq)
  
  
  df$word = gsub("“","",df$word)
  
  #stopwords with capital at the beginning
  stopwords2 = lapply(1:length(stopwords("ger")),FUN = function(x) {
    paste0(toupper(substr(stopwords("ger")[x],1,1)),substr(stopwords("ger")[x],2,nchar(stopwords("ger")[x])))
  }) %>% 
    unlist(.)
  
  #build flag vector if a stopword matches the word in matrix
  flag = unlist(lapply(1:nrow(df),
                       FUN = function(x) {
                         df$word[x] %in% c(stopwords("ger"),"dass",
                                           stopwords2,"Dass")
                       }
  )
  ) 
  
  flag[grepl("–",as.character(df$word))] = TRUE # removing hyphen
  
  
  df = df %>% 
    filter(!flag) %>%                              #removing stopwords
    filter(nchar(word) > 3 | substr(word,1,1) %in% LETTERS) %>%                    #keeping words with more than 2 letters
    group_by(word) %>%                             #summarizing same words and...
    summarize(freq = sum(freq)) %>%                #counting them..
    ungroup(.) %>% 
    mutate(rel = round(100*(freq/sum(freq)),4)) %>% #compute percentage of word in relation to all unique words
    select(word,freq,rel)                         # keeping word, frequency and percentage
  
  #return df 
  df
}

word_count_abs = data.frame(x = 1:200)
word_count_abs = cbind(word_count_abs,
                       wahl_word_count(url,stem = F) %>% 
                         arrange(desc(freq)) %>% 
                         head(200))

#naming dataframe
names_df = c("x","_word","_freq","_rel")
names(word_count_abs) = names_df


  library("tm",quietly = T)
  library("dplyr",quietly = T)
  library("ggplot2",quietly = T,warn.conflicts = F)
  library("ggwordcloud",quietly = T)
  
  set.seed(42) #makes it reproducible!
  
      word_count_abs %>% 
        select(-x) %>% 
        ggplot(.,aes(label = .[,1],size = .[,2],colour = .[,2])) + 
        geom_text_wordcloud() +
        scale_size_area(max_size = 10) +
        theme_bw()
      
      plot(plot_word)
plot_word  
