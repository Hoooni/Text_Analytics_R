library(twitteR)
library(tm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(tidyr)
library(scales)

##################### Importing files from temp folder ######################

Trumph <- readLines('C:/temp/2020 Trumph State of the Union Speech.txt')
Buttigieg <- readLines('C:/temp/2020 Buttigieg Iowa Speech.txt')

my_trump <- data_frame(line=1:102, text = Trumph)
my_butti <- data_frame(line=1:48, text = Buttigieg)

# Tokenization

my_token_trump <- my_trump %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>% #delete stop word
  count(word, sort=TRUE)#sort and count

my_token_butti <- my_butti %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>% #delete stop word
  count(word, sort=TRUE)#sort and count

##################### Frequency Histogram ############################

# User defined for Lexicon

my_junk <- data_frame(
  word = c("american","america", "americans","country","tonight",
           "Congress","congress"),
  lexicon = rep("junk", each = 7)
)

library(ggplot2)
freq_hist1 <- my_trump %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word,n)) %>% 
  top_n(10) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist1)

library(ggplot2)
freq_hist2 <- my_butti %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word,n)) %>% 
  top_n(10) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist2)

##################### Frequency correlograms ############################

my_tidy_trump <- my_trump %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>% #delete stop word
  anti_join(my_junk)

my_tidy_butti <- my_butti %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>%
  anti_join(my_junk)

#Frequency grouping

frequency <- bind_rows(mutate(my_tidy_trump, author= "Trumph"),
                       mutate(my_tidy_butti, author= "Buttigieg")
                       )%>%#closing bind_rows
            mutate(word=str_extract(word, "[a-z']+")) %>%
            count(author, word) %>%
            group_by(author) %>%
            mutate(proportion = n/sum(n))%>%
            select(-n) %>%
            spread(author, proportion)%>%
            gather(author, proportion, `Trumph`)

head(frequency)

#Plotting Frequency

ggplot(frequency, aes(x=proportion, y=`Buttigieg`, 
                      color = abs(`Buttigieg`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Buttigieg", x=NULL)

################## Bigram - Pair Words Analysis #########################

my_trump_bigrams <- my_trump %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
  
my_butti_bigrams <- my_butti %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
  
my_trump_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

my_butti_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:

trump_big_sep <- my_trump_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

butti_big_sep <- my_butti_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

turmp_big_fil <- trump_big_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

butti_big_fil <- butti_big_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":

trump_big_counts <- turmp_big_fil %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
print(trump_big_counts)

butti_big_counts <- butti_big_fil %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
print(butti_big_counts)


