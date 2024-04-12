# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

# function to count syllables (from http://lingtools.uoregon.edu/scripts/english_syllable_counter-102.R)
syllable_count <- function(ortho) {
  
  # Can add words to these lists of 2 syllable and 3 syllable 'exceptions'
  # Note that final -e is stripped before checking these lists!
  Specials.2 <- c('every', 'different', 'family', 'girl', 'girls', 'world', 'worlds', 'bein', 'being', 'something', 'mkay', 'mayb')
  Specials.3 <- c('anyon', 'everyon') # final -e is dropped	
  
  # Regular Expression exceptions
  # SubSyl - remove a syllable from the count for each sub-string match
  SubSyl <- c('cial',
              'tia',
              'cius',
              'cious',
              'giu',              # belgium!
              'ion',
              'iou',
              '^every',           # every, but also everything, everybody
              'sia$',
              '.ely$',            # absolutely! (but not ely!)
              '[^szaeiou]es$',    # fates, but not sasses
              '[^tdaeiou]ed$',    # trapped, but not fated
              '^ninet',           # nineteen, ninety
              '^awe'				# awesome
  )
  
  # AddSyl - add a syllable to the count for each sub-string match
  AddSyl <- c('ia',
              'rie[rt]',
              'dien',
              'ieth',
              'iu',
              'io',
              'ii',
              'ienc',	      # ambience, science, ...
              'les?$',
              '[aeiouym][bp]l$',  # -Vble, plus -mble and -Vple
              '[aeiou]{3}',       # agreeable
              'ndl(ed)?$',        # handle, handled
              'mpl(ed)?$',	    # trample, trampled
              '^mc',				# McEnery
              'ism$',             # -isms
              '([^aeiouy])\\1l(ed)?$',  # middle twiddle battle bottle, etc.
              '[^l]lien',         # alien, salient [1]
              '^coa[dglx].',      # [2]
              '[^gq]ua[^aeiou]',  # i think this fixes more than it breaks
              '[sd]nt$',          # couldn't, didn't, hasn't, wasn't,...
              '\\wshes$',          # add one back for esh (since it's -'d)
              '\\wches$',          #  and for affricate (witches)
              '\\wges$',           #  and voiced (ages)
              '\\wces$',	      #  and sibilant 'c's (places)
              '\\w[aeiouy]ing[s]?$'   # vowels before -ing = hiatus
  )
  
  tot_syls <- 0
  ortho.l <- tolower(ortho)
  stripchars <- "[:'\\[\\]]"
  ortho.cl <- gsub(stripchars, "", ortho.l, perl=T)
  spacechars <- "[\\W_]" # replace other non-word chars with space
  ortho.cl <- gsub(spacechars, " ", ortho.cl, perl=T)
  ortho.vec <- unlist(strsplit(ortho.cl, " ", perl=T))
  ortho.vec <- ortho.vec[ortho.vec!=""]
  for (w in ortho.vec) {
    w <- gsub("e$", "", w, perl=T) # strip final -e
    syl <- 0
    # is word in the 2 syllable exception list?
    if (w %in% Specials.2) {
      syl <- 2
      
      # is word in the 3 syllable exception list?
    } else if (w %in% Specials.3) {
      syl <- 3
      
      # if not, than check the different parts...
    } else {
      for (pat in SubSyl) {
        if (length(grep(pat, w, perl=T))>=1) 
          syl <- syl - 1
      }
      for (pat in AddSyl) {
        if (length(grep(pat, w, perl=T))>=1) 
          syl <- syl + 1
      }
      if (nchar(w)==1) {
        syl <- 1
      } else {
        chnk <- unlist(strsplit(w, "[^aeiouy:]+"))
        chnk <- chnk[chnk!=""]
        syl <- syl + length(chnk)
        if (syl==0) syl <- 1
      }
    }
    tot_syls <- tot_syls + syl
  }
  tot_syls
}

# American Presidency Project: http://www.presidency.ucsb.edu/ws/index.php
# https://www.google.com/search?ei=O6rIW_LeDYW80PEP6J-ZeA&q=site%3Ahttp%3A%2F%2Fwww.presidency.ucsb.edu%2Fws%2Findex.php+%22remarks%22
# http://www.presidency.ucsb.edu/ws/index.php?pid=130906

speeches <- data.frame(rep(0,130906))
for (i in c(1:100)) {
  url <- paste0('http://www.presidency.ucsb.edu/ws/index.php?pid=',as.character(i))
  page_html <- read_html(url)
  speeches$date[i] <- html_nodes(page_html,"span.docdate") %>% html_text()
  
  x <- c(1:length(html_nodes(page_html,"span.ver10")))
  citation_nodes <- html_nodes(page_html,"span.ver10") %>% html_text()
  author_ind <- x[!is.na(str_match(citation_nodes,"Citation"))] + 1 #finds next node after "Citation"
  speeches$author[i] <- citation_nodes[author_ind]
  speeches$title[i] <- citation_nodes[author_ind+1]
  
  # only get speeches that don't start with "Letter" or "Memorandum"
  if (!grepl("Letter",speeches$title[i]) & !grepl("Memorandum",speeches$title[i])){
  speech_text <- html_nodes(page_html,"p") %>% html_text()
  speeches$main_text[i] <- paste(speech_text[], collapse = "")
  }
}

for (i in c(1:130906)){
  speeches$newdate[i] <- as.Date(speeches$date[i], format = "%B %d, %Y") #numeric values, days +- from 1/1/1970
  speeches$newauthor[i] <- strsplit(speeches$author[i],":")[[1]][1] #removes colon and whitespace
} 

# compute Flesch Kincaid reading age (grade)
for (k in c(1:130906)){
sentences <- get_sentences(speeches$main_text[k])
if (length(sentences)>1){
  sentencesdf <- data.frame(sentences)
  for (i in c(1:length(sentences))){
    sentence <- sentences[i]
    words <- strsplit(sentence, " ")
    syll_counts <- rep(0,length(words[[1]]))
    for (j in c(1:length(words[[1]]))){
      syll_counts[j] <-syllable_count(words[[1]][j])
    }
  sentencesdf$word_count[i] <- length(words[[1]])
  sentencesdf$av_syllables[i] <- mean(syll_counts)
  }

  fk_grade <- 0.39 * mean(sentencesdf$word_count) + 11.8 * sum(sentencesdf$word_count*sentencesdf$av_syllables)/sum(sentencesdf$word_count) - 15.59
}
speeches$fk_grade[k] <- fk_grade
}

# faster method using only 50 sampled sentences
for (k in c(12000:12010)){
  sentences <- get_sentences(remarks2$main_text[k])
  if (length(sentences)>1){
    if (length(sentences)>50){
      sentences <- sample(sentences,50)
    }
    asl <- mean(lengths(strsplit(sentences, " ")))
    total_syll_count <- syllable_count(strsplit(sentences, " "))
    asw <- total_syll_count/sum(lengths(strsplit(sentences, " ")))
    fk_grade <- 0.39*asl + 11.8 * asw - 15.59
  }
  remarks2$fk_grade[k] <- fk_grade
}

#### SAVE dataframe
save(speeches,file="C:/Users/wbutler/Dropbox/Datafun/pres speeches/speeches")
save(remarks2,file="C:/Users/wbutler/Dropbox/Datafun/pres speeches/remarks")

# make into sub data frames for visualizing/analyzing
# remove exchanges, news conferences, and interviews (other speakers besides President)
remarks <- speeches[grepl("Remarks",speeches$title),] # ***need to remove "Exchange"s
remarks2 <- remarks[!grepl("Exchange", remarks$title),] # removes "Exchange"s
remarks2 <- remarks2[!grepl("Question", remarks2$title),] # removes Q&A's
remarks2 <- remarks2[!grepl("Telephone", remarks2$title),] #removes telephone conversations
remarks2 <- remarks2[!grepl("Call", remarks2$title),] # removes teleconferences and other calls
briefings
messages # to congress, the senate, etc.
noms_appointments
addresses <- speeches[grepl("Address",speeches$title),]
proclamations



############## FIGURES ##################
# number of "speeches" over time
hist(as.Date(speeches$newdate, origin = "1970/01/01"),"years", col = 'grey', format = "%Y", las = 2)
# zooming in on post 2000
hist(as.Date(speeches$newdate[speeches$newdate>365*30+100], origin = "1970/01/01"),"months", col = 'grey', format = "%Y")

# FK grade over time
library(colorspace)
library(scales)
library(dichromat)
pal <- choose_palette()
graph_pal <- pal(19)
plot(as.Date(remarks3$newdate, origin = "1970/01/01"),remarks3$fk_grade, col = alpha(graph_pal[as.factor(remarks3$newauthor)],0.3),pch=20, xlim=c(-8000,17000),xaxt="n", xlab = "year", ylab = "F-K grade level")
axis(1,at=c(-365*20,-365*10,0,365*10,365*20,365*30,365*40), labels=c(1950,1960,1970,1980,1990,2000,2010))

# compare only first XX days of each term (~600 for Trump so far)
term_edges <- c(-6178,-4734,-3267,-2232,-1809,-345,1121,1681,2576,4037,5497,6957,8417, 9877,11344,12804,14264,15725,17187)
for (i in c(1:17)){
  subdata <- remarks3[remarks3$newdate>term_edges[i] & remarks3$newdate<term_edges[i+1],]
  subdata$termdate <- subdata$newdate - term_edges[i]
  remarks3$termdate[remarks3$newdate>term_edges[i] & remarks3$newdate<term_edges[i+1]] <- subdata$termdate
}
firstsix <- remarks3[remarks3$termdate<=600,]

# word usage
pres_names <- levels(as.factor(remarks3$newauthor))
we_words <- c("we","We","We're","we'll","We'll","we're", "we've", "We've","us", "Us", "Our", "our")
i_words <- c("I", "I'm", "I'll", "I've","me", "Me", "my", "My")
they_words <- c("they", "They", "them", "Them", "they're", "They're", "they'll", "They'll", "they've", "They've", "Their", "their")
par(mfrow=c(4,5))
for (j in c(1:19)){
  pres_name <- pres_names[j]
  words <- c()
  temp_sub <- remarks3[remarks3$newauthor==pres_name,]
  for (i in c(1:nrow(temp_sub))){
    w <- strsplit(temp_sub$main_text[i], " ")
    words <- c(words,w[[1]])
  }
  t <- sort(table(words), decreasing = TRUE) %>% as.data.frame()
  t$freq <- t$Freq/sum(t$Freq)
  
  barplot(c(sum(t$freq[is.element(t$words,we_words)]),sum(t$freq[is.element(t$words,i_words)]),sum(t$freq[is.element(t$words,they_words)])), main=pres_name)
}

# tf-idf analysis
t_all <- t
for (j in c(2:19)){
  pres_name <- pres_names[j]
  words <- c()
  temp_sub <- remarks3[remarks3$newauthor==pres_name,]
  for (i in c(1:nrow(temp_sub))){
    w <- strsplit(temp_sub$main_text[i], " ")
    words <- c(words,w[[1]])
  }
  t2 <- sort(table(words), decreasing = TRUE) %>% as.data.frame()
  t2$pres <- as.factor(pres_name)
  t2$total <- as.numeric(sum(t2$Freq))
  t2$words <- as.character(t2$words)
  t_all <- rbind(t_all,t2)
}

freq_by_rank <- as_tibble(t_all) %>% 
  group_by(pres) %>% 
  mutate(rank = row_number(), 
         `term frequency` = Freq/total)

binded <- bind_tf_idf(freq_by_rank,words,pres,Freq)

# n-grams
subdata <- remarks3[remarks3$newauthor==pres_names[1],]
bigrams <- tokenizers::tokenize_ngrams(paste(subdata$main_text, collapse = " "),n=2)
bigram1 <- sort(table(bigrams), decreasing = TRUE) %>% as.data.frame
bigram1$pres <- pres_names[1]
bigram1$total <- sum(bigram1$Freq)
bigram1$freq <- bigram1$Freq/bigram1$total
bigram_all <- bigram1 %>% separate(bigrams,c("word1", "word2"), sep = " ") %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
#bigram_all <- bigram1

for (i in c(2:19)){
  subdata <- remarks3[remarks3$newauthor==pres_names[i],]
  bigrams <- tokenizers::tokenize_ngrams(paste(subdata$main_text, collapse = " "),n=2)
  bigram_t <- sort(table(bigrams), decreasing = TRUE) %>% as.data.frame
  bigram_t$pres <- pres_names[i]
  bigram_t$total <- sum(bigram_t$Freq)
  bigram_t$freq <- bigram_t$Freq/bigram_t$total
  # remove stop_words
  bigram_t <- bigram_t %>% separate(bigrams,c("word1", "word2"), sep = " ") %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
  bigram_all <- rbind(bigram_all, bigram_t[c(1:250),])
}

bigram_all <- unite(bigram_all,bigrams,word1,word2, sep = " ")
freq_by_rank_bigram <- as_tibble(bigram_all) %>% 
  group_by(pres) %>% 
  mutate(rank = row_number(), 
         `term frequency` = Freq/total)

binded_bigram <- bind_tf_idf(freq_by_rank_bigram,bigrams,pres,Freq)

# count stop words
for (i in c(2)){
     subdata <- remarks3[remarks3$newauthor==pres_names[i],]
     words <- tokenizers::tokenize_ngrams(paste(subdata$main_text, collapse = " "),n=1)
     stop_count <- sum(words[[1]] %in% stop_words$word)
     word_count <- length(words[[1]])
}

# count sentences containing "i"
for (i in c(1:19)){
  subdata <- remarks3[remarks3$newauthor==pres_names[i],]
  sents <- get_sentences(subdata$main_text)
  has_i <- c()
  for (j in c(1:length(sents))){
    words <- c(get_tokens(sents[j]),"no")
    has_i[j] <- sum("i" %in% words)/length(words)
  }
  pres_stop$i_sentences[i] <- sum(has_i)/length(has_i)
  pres_stop$date[i] <- mean(subdata$newdate)
}
