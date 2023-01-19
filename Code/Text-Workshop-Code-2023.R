#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# This is the R code from the January 2023
# workshop on quantitative text analysis, 
# conducted at the School of Public Policy at
# Oregon State University. It replicates the
# materials used in the workshop.
#
# Author: Christopher Zorn
#         Pennsylvania State University
#         Email: zorn@psu.edu
#
# File created December 21, 2022.
#
# File last updated January 19, 2023.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                     ####
#
# This first part of the code loads the R packages 
# used in the workshop, and sets a few other parameters.
#
# The following bit of code takes a list of packages
# ("P") and (a) checks  for whether the package is 
# installed or not, (b) installs it if it is not, 
# and then (c) loads each of them. It also prints
# little smiley-faces, because computer code should
# be silly as well as functional.
#
# Note that if you're doing so for the first time
# (prior to any package installations) you may need 
# to run this block of code a few times to get all 
# the packages to load.

P<-c("devtools","readr","haven","plyr","dplyr",
     "statmod","lubridate","stringr","MASS","httr",
     "pdftools","RSelenium","rvest","tesseract",
     "readtext","htmltab","rtweet","wordcloud",
     "wordcloud2","tm","stopwords","SnowballC","car",
     "tokenizers","lsa","arm","stm","SentimentAnalysis",
     "quanteda","topicmodels","quanteda.textmodels",
     "quanteda.textplots","reticulate","here")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

#=-=-=-=-=-=-=-=-=-=-=-=
# Setting more options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also: There's the issue of "where" your files are.
# An opinionated section of R users think the -setwd()-
# command is basically evil and should never be used.
# While their motivations are good, forcing people to
# do anything in one way is bad. So, you can do a few
# things here:
#
# - you can setwd("Path") to wherever you want your working
# directory to be;
#
# - you can create an RStudio Project for this code
# (for details, see here: https://intro2r.com/rsprojs.html).
#
# All the data and other materials for this code will be 
# pulled directly from the web, so there's no need to 
# choose one of these over the other from a functionality
# perspective.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Text Data                                         ####
# 
# First, the simplest example: reading text (in 
# this case, Lincoln's Gettysburg Address) from a
# "plain text" file that is stored locally. Note to
# pedants: the whole point of this is to illustrate 
# reading code from a local drive, so the fact that
# it doesn't work without setting the correct file
# path, etc. is kind of the point.

GBA<-read_file("~/Dropbox (Personal)/OSU Workshop/Data/GBA.txt")
GBA<-stripWhitespace(GBA)
GBA

# This is the same thing, but reading the same 
# text from a PDF (here, the Library of Congress
# version of the Gettysburg Address). Once again,
# this code won't work unless you include the 
# correct filepath:

GBA.2<-pdf_text("~/Dropbox (Personal)/OSU Workshop/Data/GBA.pdf")
GBA.2

# Great; now let's read some things from the (static)
# web. Here's how to get the plain-text Gettysburg Address 
# from the Github repository for this workshop:

GBA.3<-read_file("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/GBA.txt")
GBA.3

# And the same from the web-version of the PDF:

GBA.4<-pdf_text("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/GBA.pdf")
GBA.4

# Getting data from a standard / HTML webpage is
# often a bit more complex; see (e.g.) the reference
# pages for the -rvest- package, at 
# https://rvest.tidyverse.org/reference/html.html.
#
# Next: getting text from an image file... printed text:

eng<-tesseract("eng")
GBA.5<-tesseract::ocr("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/GBA.png",
                     engine = eng)
cat(GBA.5)

# Now reading from a hand-written image file:

OSU.RS<-tesseract::ocr("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/OSU-RS.jpg",
                       engine = eng)
cat(OSU.RS)

# That is hilariously wrong.
#
# Next: Doing more than one document at a time. We'll 
# illustrate this with the text of the speeches from
# the famous Lincoln-Douglas debates, which appear in
# PDF form on the workshop Github repository. This 
# code would be modified if your PDFs are "local" files,
# or plain-text / HTML / etc.
#
# Get files names

req<-GET("https://api.github.com/repos/PrisonRodeo/OSU-Text/contents/Data/LD-Debates")
files<-httr::content(req)
fnames<-sapply(files,function(x) x$name)
fnames

# Loop over file name list to get files; name each file
# "LD#" where "#" is the number of the file (so they'll 
# end up read in alphabetically):

for(i in 1:length(fnames)){
  fn<-paste0("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/LD-Debates/",
             fnames[i])
  f<-pdf_text(fn)
  out<-paste0("LD",i)
  assign(out,f)
  rm(f)
}

# We now have seven text objects, labeled "LD1" to "LD7," 
# each with the text of one of the Lincoln-Douglas 
# debates.

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Working With Text                               ####
#
# Various tools for processing text.... this illustrates
# how we can work with and manage text using basic 
# commands in R.
#
# Make a character object:

GBAC<-"It is rather for us to be here dedicated to the 
       great task remaining before us -- that from these 
       honored dead we take increased devotion to that 
       cause for which they gave the last full measure of 
       devotion -- that we here highly resolve that these 
       dead shall not have died in vain -- that this nation, 
       under God, shall have a new birth of freedom -- 
       and that government of the people, by the people, 
       for the people, shall not perish from the earth."

GBAC

# Some basic operations... Make everything lower-case:

tolower(GBAC)

#... and upper-case:

toupper(GBAC)

# and "title case":

str_to_title(GBAC)

# Replace characters (ex: "a" with "A"):

chartr("a","A",GBAC)

# Punctuation removal:

removePunctuation(GBAC)

# Remove the words "us" and "that":

removeWords(GBAC, c("us","that"))

# Strip whitespace:

stripWhitespace(GBAC)

# Next, we'll taker the entire Gettysburg address
# and "tokenize" it, breaking it into sentences:

GBA<-stripWhitespace(GBA)

GBA.sent<-tokenize_sentences(GBA)
GBA.sent

# How many sentences?

length(GBA.sent[[1]])

# Tokenize words:

GBA.words <- tokenize_words(GBA)
GBA.words
length(GBA.words[[1]]) # total word count

# Turn sentences into a nested list of words:

GBA.sw <- tokenize_words(GBA.sent[[1]])
GBA.sw

# Count words per sentence:

sapply(GBA.sw, length)

# Stop-word removal:

removeWords(GBA,stopwords("en"))

# Stemming:

stemDocument(GBA)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# TF-IDF things...
#
# OK, cool. Now we talk about TF-IDF in the slides. Here
# are the toy example documents:

A<-"red blue red"
B<-"green blue orange"
C<-"yellow blue yellow"

# Term-Document matrix:

TDM<-TermDocumentMatrix(c(A,B,C))
TDM2<-as.matrix(TDM)
TDM2

# TF-IDF

TFIDF<-as.matrix(weightTfIdf(TDM))
TFIDF

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, let's work with something bigger, albeit
# from a similar era: "The Adventures of Huckleberry 
# Finn":

AHF<-pdf_text("https://contentserver.adobe.com/store/books/HuckFinn.pdf")

# Clean up that document...
#
# First: remove everything that isn't an
# alphanumeric symbol:

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
AHF<-removeSpecialChars(AHF)

# Now text processing:

AHF.C<-VCorpus(VectorSource(AHF)) # create a "corpus"
AHF.C<-tm_map(AHF.C,content_transformer(tolower))
AHF.C<-tm_map(AHF.C,content_transformer(removeNumbers))
AHF.C<-tm_map(AHF.C,content_transformer(removePunctuation))
AHF.C<-tm_map(AHF.C,removeWords,stopwords('en'))
AHF.C<-tm_map(AHF.C,stripWhitespace)
AHF.C<-tm_map(AHF.C,removeWords,"H U C K L E B E R R Y F I N N") # strip page headings

# TDM --> 100 most common words:

AHF.TDM<-TermDocumentMatrix(AHF.C)
findFreqTerms(AHF.TDM,100,Inf)

# Barplot of the most frequently-used 50 words:

AHF.freqs<-rowSums(as.matrix(AHF.TDM))
HiFreq<-sort(tail(sort(AHF.freqs),40))

pdf("Materials/WordFreqs.pdf",7,6)
par(mar=c(4,4,2,2))
barplot(HiFreq,las=2,cex.names=0.6,
        xlab="Words",ylab="Frequency")
abline(h=c(100,200,300,400,500),lty=2)
dev.off()

# So now... Let's make a word cloud! That's always kinda 
# fun (in a completely nerdy way)...

pdf("Materials/AHF-Wordcloud.pdf",7,6) # make a PDF of the graphic
set.seed(7222009) # set random number seed
wordcloud(AHF,min.freq=40,random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))
dev.off() # write the pdf

# TF-IDF (for word importance...)

AHF.TFIDF<-weightTfIdf(AHF.TDM)
str(AHF.TFIDF)
M<-as.matrix(AHF.TFIDF)
M2<-data.frame(M)

# Now we can make a simple line plot of the importance
# of different words over each page of the book:

pdf("Materials/AHF-TFIDF-Plot.pdf",8,6)
par(mar=c(4,4,2,2))
plot(M[rownames(M)=="river"],t="l",lwd=2,
     ylim=c(0,0.12),xlab="Page",ylab="Word Importance (TF-IDF)")
lines(M[rownames(M)=="jim"],t="l",lwd=2,lty=2,col="orange")
lines(M[rownames(M)=="duke"],t="l",lwd=2,lty=3,col="navy")
lines(M[rownames(M)=="grangerford"],t="l",lwd=2,lty=4,col="green")
lines(M[rownames(M)=="pap"],t="l",lwd=2,lty=5,col="red")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","orange","navy","green","red"),
       legend=c("\"River\"","\"Jim\"","\"Duke\"",
                "\"Grangerford\"","\"Pap\""))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dictionary-Based Methods: Sentiment Analysis    ####
#
# Example: Speeches by every UN High Commissioner
# for Refugees, 1970-2016...
#
# Get the data from the Github repo:

UN <- read.csv("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/UNHCRSpeeches.csv")

# Now do a bunch of text cleaning and preprocessing
# to get it in shape for analysis:

UN$content <- removeNumbers(UN$content) # no numbers
UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks
UN$content <- removeWords(UN$content,stopwords("en")) # remove stopwords
UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$foo <- NULL
# Speech authors:
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Create a corpus:

UN2 <- with(UN, data.frame(doc_id = id,
                           text = content))
ds <- DataframeSource(UN2) 
UNC <- Corpus(ds)
meta(UNC)

UNCount<-countWords(UNC,removeStopwords=FALSE)
summary(UNCount$WordCount)

pdf("Materials/UNHCR-Hist.pdf",6,5)
par(mar=c(4,4,2,2))
hist(UNCount$WordCount,main=" ",xlab="Word Count",
     col="grey32")
dev.off()

# Do a simple sentiment analysis on those speeches:

UNSent <- analyzeSentiment(UNC)
summary(UNSent)

# Plot: sentiment vs. word count: Are longer speeches
# happier or sadder?

# Plots, etc.:

rSC<-with(UNSent, cor(log(WordCount),SentimentGI))

pdf("Materials/UNHCRSentVsCount.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~WordCount,data=UNSent,log="x",
            pch=20,grid=FALSE,xlab="ln(Word Count)",
            ylab="Sentiment",spread=FALSE)
abline(h=0,lty=2,lwd=1.5)
text(100,0.25,paste0("r = ",round(rSC,2)))
dev.off()

# How does speech sentiment vary over time?

pdf("Materials/UNHCRSentOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNSent$SentimentGI,t="l",lwd=1.5,
     xlab="Date",ylab="Speech Sentiment")
lines(lowess(UN$Date,UNSent$SentimentGI),lwd=2,col="red")
abline(h=0,lty=2)
dev.off()

# Same plot, but aggregated by year...

AnnMeans<-aggregate(UNSent$SentimentGI,list(UN$Year),mean)

pdf("Materials/UNHCRAnnMeans.pdf",6,5)
par(mar=c(4,4,2,2))
plot(AnnMeans$Group.1,AnnMeans$x,t="l",lwd=1.5,
     xlab="Year",ylab="Average Sentiment",ylim=c(0.04,0.17))
lines(lowess(AnnMeans$Group.1,AnnMeans$x),lwd=2,col="red")
dev.off()

# Sentiment by speech author / speaker; do different
# UNHCRs have different levels of sentiment?:

UN$Author<-ordered(UN$Author,levels=c("Goedhart","Lindt",
                                      "Schnyder","Khan","Hartling",
                                      "Hocké","Stoltenberg","Ogata",
                                      "Lubbers","Guterres"))

pdf("Materials/UNHCR-by-Author.pdf",6,5)
par(mar=c(6,4,2,2))
boxplot(UNSent$SentimentGI~UN$Author,las=2,
        xlab="",ylab="Sentiment")
abline(h=0,lty=2)
dev.off()

# Next: Does the choice of sentiment dictionary 
# "matter"?
#
# Compare "General Inquirer" to "QDAP":

GI<-loadDictionaryGI()
QD<-loadDictionaryQDAP()

compareDictionaries(GI,QD)

# Plot the differences:

r.GI.QDAP <- with(UNSent, cor(SentimentGI,SentimentQDAP))

pdf("Materials/UNHCR-Dict-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~SentimentQDAP,data=UNSent,
            xlab="QDAP",ylab="General Inquirer",pch=20,
            grid=FALSE)
text(0,0.20,paste0("r = ",round(r.GI.QDAP,2)))
dev.off()

# "Custom" dictionary -- here, useful for topic
# identification:

# Custom dictionary by-hand:

YugoWords <- c("yugoslavia","serbia","bosnia","herzegovina",
               "kosovo","montenegro","macedonia","croatia",
               "vojvodina","balkans")

FmrYugo <- SentimentDictionaryWordlist(YugoWords)

UNHCRYugo <- analyzeSentiment(UNC,
                              rules=list("YugoTopic"=list(
                                ruleRatio,FmrYugo)))

summary(UNHCRYugo$YugoTopic)

# Plot the extent of Fmr. Yugoslavia content over
# time:

pdf("Materials/UNHCRYugoOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNHCRYugo$YugoTopic,t="l",lwd=1.5,
     xlab="Date",ylab="Fmr. Yugoslavia Content")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inductive Methods: Topic Models                 ####
#
# Once again, get the UNHCR data (we could use the
# same file we did above, but this way I can demon-
# strate a different text processor):

UN <- read.csv("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/UNHCRSpeeches.csv")

# Clean things up a bit:

UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Process text (this time using textProcessor from stm):
#
# Note that default to converting cases, removing stopwords / 
# punctuation / words < 3 characters / extra white space, 
# and stemming the words.

UNHCR <- textProcessor(UN$content, metadata=UN) 

# Create stm corpus. Note that this defaults to dropping
# words that only appear in one document:

UNCorp <- prepDocuments(UNHCR$documents,UNHCR$vocab,UNHCR$meta)

# Next: Basic LDA (latent dirichlet allocation) 
# using the -topicmodels- package...
#
# Convert the corpus format:

UNLDACorp <- convertCorpus(UNCorp$documents,UNCorp$vocab,
                           type="slam")

# Basic LDA, with six topics:

UN.LDAV.6 <- LDA(UNLDACorp,6,method="VEM",
                 seed=7222009)

# Check out the terms / topics:

terms(UN.LDAV.6,10)

# Which of the six topics is the highest-probability
# one for each of the 703 documents?

pdf("Materials/UNTopicBarplot.pdf",8,4)
par(mar=c(4,4,2,2))
barplot(table(topics(UN.LDAV.6,10)[1,]),
        xlab="Topic Number",ylab="Frequency")
dev.off()

# Generate posterior probabilities of the topics 
# for each document and the terms for each topic:

V.6.Post <- posterior(UN.LDAV.6)
# cor(V.6.Post$topics)

# Plot those:

pdf("Materials/LDA-Posteriors.pdf",9,7)
scatterplotMatrix(V.6.Post$topics,pch=".",smooth=FALSE,
                  col="black",regLine=FALSE,
                  var.labels=paste0("Topic ",
                  colnames(V.6.Post$topics)))
dev.off()

# How do topics vary across authors / speech-givers?

pdf("Materials/LDA-By-Author.pdf",8,6)
par(mar=c(6,4,2,2))
par(mfrow=c(2,3))
boxplot(V.6.Post$topic[,1]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic One")
boxplot(V.6.Post$topic[,2]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Two")
boxplot(V.6.Post$topic[,3]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Three")
boxplot(V.6.Post$topic[,4]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Four")
boxplot(V.6.Post$topic[,5]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Five")
boxplot(V.6.Post$topic[,6]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Six")
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inductive Methods: Text Scaling                 ####
#
# We'll stick with the UNHCR speech data for some 
# minimal examples of text scaling. 
#
# Reread + clean the data again:

UN<-read_csv("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/UNHCRSpeeches.csv")
UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Create a quanteda corpus:

UNs<-data.frame(doc_id=1:nrow(UN),
               text=UN$content,
               Author=UN$Author,
               Year=UN$Year)

UNC<-corpus(UNs)       # corpus
UN.token <- tokens(UNC) # "tokenize"
UN.DFM <- dfm(UN.token)

# Wordscores require "training" (human-coded)
# data, which we don't have for the UNHCR 
# speeches; and Wordfish doesn't seem to like
# these data very much. In the interest of having
# *something* here, I'll fit a couple alter-
# ative models instead.
#
# Estimate a latent semantic analysis model:

UN.LSM<-textmodel_lsa(UN.DFM,nd=0)

# Estimate correspondence analysis scaling:

UN.CA<-textmodel_ca(UN.DFM)

# etc.

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Transformers, LLMs, etc.                        ####
#
# There isn't much here for code... a good place
# to start is by uncommenting and running this code,
# taken from https://github.com/OscarKjell/text
#
# devtools::install_github("oscarkjell/text")
# library(text)
# textrpp_install() # installs other things...
# 2
# textrpp_initialize(save_profile = TRUE)
# 1
#
# That installs a bunch of things, including a 
# small version of the CONDA environment. It also
# probably fails at the end, albeit with a bunch of 
# helpful verbiage about why it did so (probably 
# because you need a working Python environment
# installed first, including the pip() installer).
#
# As I note in the talk, if you're going to get
# deep into BERT and GPT models of language, R
# is probably not (right now) the language to do 
# it with; Python is best, and Java or C/C++ are
# also viable options.
#
# Here's my very minimal example of calling
# transformers in Python via R. To do this, 
# you first need to install Python on your
# machine; you can see how to do that here:
#
# https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/
#
# Once you've done that, you need to install 
# PyTorch (or Tensorflow) and set up a virtual
# environment. On my recent M1 Mac, I did this
# by running these things **in the shell* (that
# is, in the "Terminal" app if you're on a Mac):
#
# python3 -m pip install --user --upgrade pip
# python3 -m pip install --user virtualenv
# python3 -m venv foo
# source foo/bin/activate
# pip3 install torch torchvision torchaudio
#
# Then and only then, you can start Python:

use_python("/Users/cuz10/foo/bin/python")
py_config()

# ... and install the transformers:

py_install("transformers",pip=TRUE)

# Next, bring the transformers into the current
# R session:

trans<-reticulate::import("transformers")

# ...and create a "pipeline," in this case
# for sentiment classification:

sentimentGPT<-trans$pipeline(task="text-classification") 

GBA.sentiment<-sentimentGPT(GBA)
GBA.sentiment

# Now, do the same with the UN speeches:

UN <- read.csv("https://github.com/PrisonRodeo/OSU-Text/raw/main/Data/UNHCRSpeeches.csv")

# Minimal cleaning and preprocessing to get it in 
# shape for analysis:

UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks

# Break into sentences:

UN.sent<-unlist(tokenize_sentences(UN$content))

# Sentiment for each sentence...

bar<-list() # a place to put the results

# Pass each sentence of the 703 speeches through
# our pipeline, and store the result:

for(i in 1:length(UN.sent)){
 bar[[i]]<-sentimentGPT(UN.sent[[i]])
}

# Now take the thing called "bar" (that is a long
# names list of the sentiment scores) and turn 
# it into data that we can use:

df.BERT<-as.data.frame(matrix(unlist(bar),ncol=2,byrow=TRUE))
colnames(df.BERT)<-c("Direction","Score")
df.BERT$Score<-as.numeric(df.BERT$Score)
df.BERT$SignScore<-ifelse(df.BERT$Direction=="NEGATIVE",
                          -df.BERT$Score,df.BERT$Score)

#... e.g., to make a plot (in this case, over 
# time):

pdf("Materials/BERT-UN-Sentiment.pdf",7,6)
par(mar=c(4,4,2,2))
plot(lowess(df.BERT$SignScore),t="l",lwd=2,
     ylim=c(0.22,0.35),
     xlab="Sentence Number",ylab="Sentiment (Lowess smooth)")
dev.off()


# OK, that's all. :)
