install.packages("easyPubMed")
library(easyPubMed)

#######

## Example 01: retrieve data in TXT format
dami_query_string <- '((("Nature"[Journal]) OR "Science"[Journal]) OR "Cell"[Journal]) AND ("2009/01/01"[Date - Publication] : "3000"[Date - Publication])' 
# dami_on_pubmed <- get_pubmed_ids(dami_query_string)
# dami_papers <- fetch_pubmed_data(dami_on_pubmed, format = "abstract")
# dami_papers[dami_papers == ""] <- "\n"
# cat(paste(dami_papers[1:65], collapse = ""))

out1 <- batch_pubmed_download(pubmed_query_string = dami_query_string, dest_dir = 'CNS', batch_size = 5000, format="xml")


#####
noncns_query <- '((("Nature Biotechnology"[Journal]) OR ("Nature Cell Biology"[Journal]) OR ("Nature Chemical Biology"[Journal]) OR ("Nature Chemistry"[Journal]) OR ("Nature Communications"[Journal]) OR ("Nature Ecology Evolution"[Journal]) OR ("Nature Genetics"[Journal]) OR ("Nature Immunology"[Journal]) OR ("Nature Medicine"[Journal]) OR ("Nature Nanotechnology"[Journal]) OR ("Nature Neuroscience"[Journal]) OR ("Nature New Biology"[Journal]) OR ("Nature Plants"[Journal]) OR ("Nature Structural Biology"[Journal]) OR ("Nature Structural Molecular Biology"[Journal]) OR ("proc natl acad sci u s a"[Journal]) OR ("scientific reports"[Journal]) OR ("Current Biology"[Journal]) OR ("Neuron"[Journal]) OR ("Molecular Cell"[Journal]) OR ("Cell Stem Cell"[Journal]) OR ("Cancer Cell"[Journal]) OR ("Immunity"[Journal]) AND ("2009/01/01"[Date - Publication] : "3000"[Date - Publication])'
noncns_pub <- batch_pubmed_download(pubmed_query_string = noncns_query, dest_dir = 'nCNS', batch_size = 5000)




##### extract words from xml data
library(XML)
library(xml2)



cns_titles <- c()
for (i in c("001","002","003","004","005","006","007","008","009","010","011")){
  x1 <- read_xml(paste("CNS/easyPubMed_data_",i,".txt", sep=""))
  titles <- xml_text(xml_find_all(x1,"//ArticleTitle"))
  abs <- xml_text(xml_find_all(x1,"//AbstractText"))
  auths_last <- xml_text(xml_find_all(x1,"//LastName"))
  auths_first <- xml_text(xml_find_all(x1,"//ForeName"))
  affs <- xml_text(xml_find_all(x1,"//Affiliation"))
  cns_titles <- c(cns_titles,titles)
}


library(tokenizers)
library(tm)

title_words <- tokenize_word_stems(paste(cns_titles,collapse = " "))[[1]]
t <- as.data.frame(table(title_words))

stop_words <- stopwords()
t <- t[!t$title_words %in% stop_words,]

#######################################
ncns_titles <- c()
for (i in c("001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039")){
  x2 <- read_xml(paste("nCNS/easyPubMed_data_",i,".txt", sep=""))
  titles <- xml_text(xml_find_all(x2,"//ArticleTitle"))
  abs <- xml_text(xml_find_all(x2,"//AbstractText"))
  auths_last <- xml_text(xml_find_all(x2,"//LastName"))
  auths_first <- xml_text(xml_find_all(x2,"//ForeName"))
  affs <- xml_text(xml_find_all(x2,"//Affiliation"))
  ncns_titles <- c(cns_titles,titles)
}

title_words2 <- tokenize_word_stems(paste(ncns_titles,collapse = " "))[[1]]
t2 <- as.data.frame(table(title_words2))

stop_words <- stopwords()
t2 <- t2[!t2$title_words2 %in% stop_words,]
