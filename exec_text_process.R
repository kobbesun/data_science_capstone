source("./define_text_process.R")

split_text(input_file = "./data/final/en_US/en_US.blogs.txt", 
           sample_prob = 0.6,
           sample_file = "./data/US_blogs_train.txt", 
           other_file = "./data/US_blogs_test_validate.txt")

split_text(input_file = "./data/US_blogs_test_validate.txt", 
           sample_prob = 0.5,
           sample_file = "./data/US_blogs_validate.txt", 
           other_file = "./data/US_blogs_test.txt")

split_text(input_file = "./data/final/en_US/en_US.news.txt", 
           sample_prob = 0.6,
           sample_file = "./data/US_news_train.txt", 
           other_file = "./data/US_news_test_validate.txt")

split_text(input_file = "./data/US_news_test_validate.txt", 
           sample_prob = 0.5,
           sample_file = "./data/US_news_validate.txt", 
           other_file = "./data/US_news_test.txt")

split_text(input_file = "./data/final/en_US/en_US.twitter.txt", 
           sample_prob = 0.6,
           sample_file = "./data/US_twitter_train.txt", 
           other_file = "./data/US_twitter_test_validate.txt")

split_text(input_file = "./data/US_twitter_test_validate.txt", 
           sample_prob = 0.5,
           sample_file = "./data/US_twitter_validate.txt", 
           other_file = "./data/US_twitter_test.txt")


## generate term frequency charts
setStopwords <- union(stopwords("en"), stopwords("SMART"))
intTopTerm <- 10

## generate US Blog test unigram distribution
strDirUSBlogTestTermFreq_unigram <- "./data/TermFreq_unigram_US_blog_test.csv"
dtUSBlogTestTermFreq_unigram <- fread(strDirUSBlogTestTermFreq_unigram,
                                      encoding = FILE_ENCODING)
setnames(dtUSBlogTestTermFreq_unigram, c("Index", "Term", "N"))

dtUSBlogTestTermFreq_unigram <- dtUSBlogTestTermFreq_unigram[, sum(N), 
                                                             by = "Term"]
setnames(dtUSBlogTestTermFreq_unigram, old = "V1", new = "Count")

intUSBlogTestUniNum <- sum(dtUSBlogTestTermFreq_unigram$Count)
dtUSBlogTestTermFreq_unigram[, Percent := percent(Count/intUSBlogTestUniNum, 
                                                  digits = 8)]

write.csv(dtUSBlogTestTermFreq_unigram, 
          "./data/US_blog_test_unigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSBlogTestTermFreq_unigram[, Term_Lower := sapply(Term, try_tolower)]

#dtUSBlogTestTermFreq_unigram[order(-rank(Count))][1:100]

dtUSBlogTestTermFreq_unigram_lower <- dtUSBlogTestTermFreq_unigram[
                                      !(Term_Lower %in% setStopwords),]
dtUSBlogTestTermFreq_unigram_lower <- dtUSBlogTestTermFreq_unigram_lower[,
                                      sum(Count), by = "Term_Lower"]
setnames(dtUSBlogTestTermFreq_unigram_lower, old = "V1", new = "Count_Lower")

dtUSBlogTestTermFreq_unigram_lower[, Percent_Lower :=
                                   percent(Count_Lower/intUSBlogTestUniNum,
                                           digits = 8)]

write.csv(dtUSBlogTestTermFreq_unigram_lower, 
          "./data/US_blog_test_unigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSBlogShow_unigram <- dtUSBlogTestTermFreq_unigram_lower[
                        order(-rank(Count_Lower))][1:intTopTerm]

#ggplot(data = dtUSBlogShow_unigram, 
#       aes(x = Term_Lower, y = Percent_Lower, fill = Term_Lower)) + 
#    geom_bar(stat = "identity")


## generate US News test unigram distribution
strDirUSNewsTestTermFreq_unigram <- "./data/TermFreq_unigram_US_news_test.csv"
dtUSNewsTestTermFreq_unigram <- fread(strDirUSNewsTestTermFreq_unigram,
                                      encoding = FILE_ENCODING)
setnames(dtUSNewsTestTermFreq_unigram, c("Index", "Term", "N"))

dtUSNewsTestTermFreq_unigram <- dtUSNewsTestTermFreq_unigram[, sum(N), 
                                                             by = "Term"]
setnames(dtUSNewsTestTermFreq_unigram, old = "V1", new = "Count")

intUSNewsTestUniNum <- sum(dtUSNewsTestTermFreq_unigram$Count)
dtUSNewsTestTermFreq_unigram[, Percent := percent(Count/intUSNewsTestUniNum, 
                                                  digits = 8)]

write.csv(dtUSNewsTestTermFreq_unigram, 
          "./data/US_news_test_unigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSNewsTestTermFreq_unigram[, Term_Lower := sapply(Term, try_tolower)]

#dtUSNewsTestTermFreq_unigram[order(-rank(Count))][1:100]

dtUSNewsTestTermFreq_unigram_lower <- dtUSNewsTestTermFreq_unigram[
                                      !(Term_Lower %in% setStopwords),]
dtUSNewsTestTermFreq_unigram_lower <- dtUSNewsTestTermFreq_unigram_lower[,
                                      sum(Count), by = "Term_Lower"]
setnames(dtUSNewsTestTermFreq_unigram_lower, old = "V1", new = "Count_Lower")

dtUSNewsTestTermFreq_unigram_lower[, Percent_Lower :=
                                   percent(Count_Lower/intUSNewsTestUniNum,
                                           digits = 8)]

write.csv(dtUSNewsTestTermFreq_unigram_lower, 
          "./data/US_news_test_unigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSNewsShow_unigram <- dtUSNewsTestTermFreq_unigram_lower[
                        order(-rank(Count_Lower))][1:intTopTerm]

#ggplot(data = dtUSNewsShow_unigram, 
#       aes(x = Term_Lower, y = Percent_Lower, fill = Term_Lower)) + 
#    geom_bar(stat = "identity")



## generate US Twitter test unigram distribution
strDirUSTwtTestTermFreq_unigram <- "./data/TermFreq_unigram_US_twitter_test.csv"
dtUSTwtTestTermFreq_unigram <- fread(strDirUSTwtTestTermFreq_unigram,
                                     encoding = FILE_ENCODING)
setnames(dtUSTwtTestTermFreq_unigram, c("Index", "Term", "N"))

dtUSTwtTestTermFreq_unigram <- dtUSTwtTestTermFreq_unigram[, sum(N), 
                                                           by = "Term"]
setnames(dtUSTwtTestTermFreq_unigram, old = "V1", new = "Count")

intUSTwtTestUniNum <- sum(dtUSTwtTestTermFreq_unigram$Count)
dtUSTwtTestTermFreq_unigram[, Percent := percent(Count/intUSTwtTestUniNum, 
                                                 digits = 8)]

write.csv(dtUSTwtTestTermFreq_unigram, 
          "./data/US_twitter_test_unigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSTwtTestTermFreq_unigram[, Term_Lower := sapply(Term, try_tolower)]

#dtUSTwtTestTermFreq_unigram[order(-rank(Count))][1:100]

dtUSTwtTestTermFreq_unigram_lower <- dtUSTwtTestTermFreq_unigram[
                                     !(Term_Lower %in% setStopwords),]
dtUSTwtTestTermFreq_unigram_lower <- dtUSTwtTestTermFreq_unigram_lower[,
                                     sum(Count), by = "Term_Lower"]
setnames(dtUSTwtTestTermFreq_unigram_lower, old = "V1", new = "Count_Lower")

dtUSTwtTestTermFreq_unigram_lower[, Percent_Lower :=
                                  percent(Count_Lower/intUSTwtTestUniNum,
                                          digits = 8)]

write.csv(dtUSTwtTestTermFreq_unigram_lower, 
          "./data/US_twitter_test_unigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSTwtShow_unigram <- dtUSTwtTestTermFreq_unigram_lower[
                       order(-rank(Count_Lower))][1:intTopTerm]

#ggplot(data = dtUSTwtShow_unigram, 
#       aes(x = Term_Lower, y = Percent_Lower, fill = Term_Lower)) + 
#    geom_bar(stat = "identity")



## Comapre Top 10 unigram terms of Blog, News, and Twitter
dtUSBlogShow_unigram[, Source := "US_Blog"]
dtUSNewsShow_unigram[, Source := "US_News"]
dtUSTwtShow_unigram[, Source := "US_Twitter"]

dtUSAllShow_unigram <- rbind(dtUSBlogShow_unigram, dtUSNewsShow_unigram,
                             dtUSTwtShow_unigram)

write.csv(dtUSAllShow_unigram, "./data/US_test_top_unigram_term_freq.csv",
          fileEncoding = FILE_ENCODING)

ggplot(data = dtUSAllShow_unigram, 
       aes(x = Term_Lower, y = Percent_Lower, fill = Source)) + 
    geom_bar(stat = "identity",  position = position_dodge()) +
    ggtitle("Top 10 Unigram") + scale_fill_discrete(name="Data Source") +
    xlab("Unigram") + ylab("Weight")



## generate US Blog test bigram distribution
strDirUSBlogTestTermFreq_bigram <- "./data/TermFreq_bigram_US_blog_test.csv"
dtUSBlogTestTermFreq_bigram <- fread(strDirUSBlogTestTermFreq_bigram, 
                                     encoding = FILE_ENCODING)
setnames(dtUSBlogTestTermFreq_bigram, c("Index", "Term", "N"))

dtUSBlogTestTermFreq_bigram <- dtUSBlogTestTermFreq_bigram[, sum(N), 
                                                           by = "Term"]
setnames(dtUSBlogTestTermFreq_bigram, old = "V1", new = "Count")

intUSBlogTestBiNum <- sum(dtUSBlogTestTermFreq_bigram$Count)
dtUSBlogTestTermFreq_bigram[, Percent := percent(Count/intUSBlogTestBiNum, 
                                                 digits = 8)]

write.csv(dtUSBlogTestTermFreq_bigram, 
          "./data/US_blog_test_bigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSBlogTestTermFreq_bigram[, Term_Lower := sapply(Term, try_tolower)]
dtUSBlogTestTermFreq_bigram_lower <- dtUSBlogTestTermFreq_bigram[
                                     !sapply(Term_Lower, term_in_set, 
                                     set_check_against = setStopwords),]
dtUSBlogTestTermFreq_bigram_lower <- dtUSBlogTestTermFreq_bigram_lower[
                                     sapply(Term_Lower, filter_ngram, 
                                            int_n = 2),]

dtUSBlogTestTermFreq_bigram_lower <- dtUSBlogTestTermFreq_bigram_lower[,
                                     sum(Count), by = "Term_Lower"]
setnames(dtUSBlogTestTermFreq_bigram_lower, old = "V1", new = "Count_Lower")

dtUSBlogTestTermFreq_bigram_lower[, Percent_Lower :=
                                  percent(Count_Lower/intUSBlogTestBiNum,
                                          digits = 8)]

write.csv(dtUSBlogTestTermFreq_bigram_lower, 
          "./data/US_blog_test_bigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSBlogShow_bigram <- dtUSBlogTestTermFreq_bigram_lower[
                       order(-rank(Count_Lower))][1:intTopTerm]


## generate US News test bigram distribution
strDirUSNewsTestTermFreq_bigram <- "./data/TermFreq_bigram_US_news_test.csv"
dtUSNewsTestTermFreq_bigram <- fread(strDirUSNewsTestTermFreq_bigram,
                                     encoding = FILE_ENCODING)
setnames(dtUSNewsTestTermFreq_bigram, c("Index", "Term", "N"))

dtUSNewsTestTermFreq_bigram <- dtUSNewsTestTermFreq_bigram[, sum(N), 
                                                           by = "Term"]
setnames(dtUSNewsTestTermFreq_bigram, old = "V1", new = "Count")

intUSNewsTestBiNum <- sum(dtUSNewsTestTermFreq_bigram$Count)
dtUSNewsTestTermFreq_bigram[, Percent := percent(Count/intUSNewsTestBiNum, 
                                                 digits = 8)]

write.csv(dtUSNewsTestTermFreq_bigram, 
          "./data/US_news_test_bigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSNewsTestTermFreq_bigram[, Term_Lower := sapply(Term, try_tolower)]
dtUSNewsTestTermFreq_bigram_lower <- dtUSNewsTestTermFreq_bigram[
                                     !sapply(Term_Lower, term_in_set, 
                                     set_check_against = setStopwords),]
dtUSNewsTestTermFreq_bigram_lower <- dtUSNewsTestTermFreq_bigram_lower[
                                     sapply(Term_Lower, filter_ngram, 
                                            int_n = 2),]

dtUSNewsTestTermFreq_bigram_lower <- dtUSNewsTestTermFreq_bigram_lower[,
                                     sum(Count), by = "Term_Lower"]
setnames(dtUSNewsTestTermFreq_bigram_lower, old = "V1", new = "Count_Lower")

dtUSNewsTestTermFreq_bigram_lower[, Percent_Lower :=
                                  percent(Count_Lower/intUSNewsTestBiNum,
                                  digits = 8)]

write.csv(dtUSNewsTestTermFreq_bigram_lower, 
          "./data/US_news_test_bigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSNewsShow_bigram <- dtUSNewsTestTermFreq_bigram_lower[
                       order(-rank(Count_Lower))][1:intTopTerm]



## generate US Twitter test bigram distribution
strDirUSTwtTestTermFreq_bigram <- "./data/TermFreq_bigram_US_twitter_test.csv"
dtUSTwtTestTermFreq_bigram <- fread(strDirUSTwtTestTermFreq_bigram,
                                    encoding = FILE_ENCODING)
setnames(dtUSTwtTestTermFreq_bigram, c("Index", "Term", "N"))

dtUSTwtTestTermFreq_bigram <- dtUSTwtTestTermFreq_bigram[, sum(N), 
                                                         by = "Term"]
setnames(dtUSTwtTestTermFreq_bigram, old = "V1", new = "Count")

intUSTwtTestBiNum <- sum(dtUSTwtTestTermFreq_bigram$Count)
dtUSTwtTestTermFreq_bigram[, Percent := percent(Count/intUSTwtTestBiNum, 
                                                digits = 8)]

write.csv(dtUSTwtTestTermFreq_bigram, 
          "./data/US_twitter_test_bigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSTwtTestTermFreq_bigram[, Term_Lower := sapply(Term, try_tolower)]
dtUSTwtTestTermFreq_bigram_lower <- dtUSTwtTestTermFreq_bigram[
                                    !sapply(Term_Lower, term_in_set, 
                                    set_check_against = setStopwords),]
dtUSTwtTestTermFreq_bigram_lower <- dtUSTwtTestTermFreq_bigram_lower[
                                     sapply(Term_Lower, filter_ngram, 
                                            int_n = 2),]

dtUSTwtTestTermFreq_bigram_lower <- dtUSTwtTestTermFreq_bigram_lower[,
                                    sum(Count), by = "Term_Lower"]
setnames(dtUSTwtTestTermFreq_bigram_lower, old = "V1", new = "Count_Lower")

dtUSTwtTestTermFreq_bigram_lower[, Percent_Lower :=
                                 percent(Count_Lower/intUSTwtTestBiNum,
                                         digits = 8)]

write.csv(dtUSTwtTestTermFreq_bigram_lower, 
          "./data/US_twitter_test_bigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSTwtShow_bigram <- dtUSTwtTestTermFreq_bigram_lower[
                      order(-rank(Count_Lower))][1:intTopTerm]


## Comapre Top 10 bigram terms of Blog, News, and Twitter
dtUSBlogShow_bigram[, Source := "US_Blog"]
dtUSNewsShow_bigram[, Source := "US_News"]
dtUSTwtShow_bigram[, Source := "US_Twitter"]

dtUSAllShow_bigram <- rbind(dtUSBlogShow_bigram, dtUSNewsShow_bigram,
                            dtUSTwtShow_bigram)

write.csv(dtUSAllShow_bigram, "./data/US_test_top_bigram_term_freq.csv",
          fileEncoding = FILE_ENCODING)

ggplot(data = dtUSAllShow_bigram, 
       aes(x = Term_Lower, y = Percent_Lower, fill = Source)) + 
    geom_bar(stat = "identity",  position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Top 10 Bigram") + scale_fill_discrete(name="Data Source") +
    xlab("Bigram") + ylab("Weight")




## generate US Blog test trigram distribution
strDirUSBlogTestTermFreq_trigram <- "./data/TermFreq_trigram_US_blog_test.csv"
dtUSBlogTestTermFreq_trigram <- fread(strDirUSBlogTestTermFreq_trigram,
                                      encoding = FILE_ENCODING)
setnames(dtUSBlogTestTermFreq_trigram, c("Index", "Term", "N"))

dtUSBlogTestTermFreq_trigram <- dtUSBlogTestTermFreq_trigram[, sum(N), 
                                                             by = "Term"]
setnames(dtUSBlogTestTermFreq_trigram, old = "V1", new = "Count")

intUSBlogTestTriNum <- sum(dtUSBlogTestTermFreq_trigram$Count)
dtUSBlogTestTermFreq_trigram[, Percent := percent(Count/intUSBlogTestTriNum, 
                                                  digits = 8)]

write.csv(dtUSBlogTestTermFreq_trigram, 
          "./data/US_blog_test_trigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSBlogTestTermFreq_trigram[, Term_Lower := sapply(Term, try_tolower)]
dtUSBlogTestTermFreq_trigram_lower <- dtUSBlogTestTermFreq_trigram[
                                      !sapply(Term_Lower, term_in_set, 
                                      set_check_against = setStopwords),]
dtUSBlogTestTermFreq_trigram_lower <- dtUSBlogTestTermFreq_trigram_lower[
                                      sapply(Term_Lower, filter_ngram, 
                                             int_n = 3),]

dtUSBlogTestTermFreq_trigram_lower <- dtUSBlogTestTermFreq_trigram_lower[,
                                      sum(Count), by = "Term_Lower"]
setnames(dtUSBlogTestTermFreq_trigram_lower, old = "V1", new = "Count_Lower")

dtUSBlogTestTermFreq_trigram_lower[, Percent_Lower :=
                                   percent(Count_Lower/intUSBlogTestTriNum,
                                           digits = 8)]

write.csv(dtUSBlogTestTermFreq_trigram_lower, 
          "./data/US_blog_test_trigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSBlogShow_trigram <- dtUSBlogTestTermFreq_trigram_lower[
                        order(-rank(Count_Lower))][1:intTopTerm]


## generate US news test trigram distribution
strDirUSNewsTestTermFreq_trigram <- "./data/TermFreq_trigram_US_news_test.csv"
dtUSNewsTestTermFreq_trigram <- fread(strDirUSNewsTestTermFreq_trigram,
                                      encoding = FILE_ENCODING)
setnames(dtUSNewsTestTermFreq_trigram, c("Index", "Term", "N"))

dtUSNewsTestTermFreq_trigram <- dtUSNewsTestTermFreq_trigram[, sum(N), 
                                                             by = "Term"]
setnames(dtUSNewsTestTermFreq_trigram, old = "V1", new = "Count")

intUSNewsTestTriNum <- sum(dtUSNewsTestTermFreq_trigram$Count)
dtUSNewsTestTermFreq_trigram[, Percent := percent(Count/intUSNewsTestTriNum, 
                                                  digits = 8)]

write.csv(dtUSNewsTestTermFreq_trigram, 
          "./data/US_news_test_trigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSNewsTestTermFreq_trigram[, Term_Lower := sapply(Term, try_tolower)]
dtUSNewsTestTermFreq_trigram_lower <- dtUSNewsTestTermFreq_trigram[
                                      !sapply(Term_Lower, term_in_set, 
                                      set_check_against = setStopwords),]
dtUSNewsTestTermFreq_trigram_lower <- dtUSNewsTestTermFreq_trigram_lower[
                                      sapply(Term_Lower, filter_ngram, 
                                             int_n = 3),]

dtUSNewsTestTermFreq_trigram_lower <- dtUSNewsTestTermFreq_trigram_lower[,
                                      sum(Count), by = "Term_Lower"]
setnames(dtUSNewsTestTermFreq_trigram_lower, old = "V1", new = "Count_Lower")

dtUSNewsTestTermFreq_trigram_lower[, Percent_Lower :=
                                   percent(Count_Lower/intUSNewsTestTriNum,
                                           digits = 8)]

write.csv(dtUSNewsTestTermFreq_trigram_lower, 
          "./data/US_news_test_trigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSNewsShow_trigram <- dtUSNewsTestTermFreq_trigram_lower[
                        order(-rank(Count_Lower))][1:intTopTerm]



## generate US Twitter test trigram distribution
strDirUSTwtTestTermFreq_trigram <- "./data/TermFreq_trigram_US_twitter_test.csv"
dtUSTwtTestTermFreq_trigram <- fread(strDirUSTwtTestTermFreq_trigram,
                                     encoding = FILE_ENCODING)
setnames(dtUSTwtTestTermFreq_trigram, c("Index", "Term", "N"))

dtUSTwtTestTermFreq_trigram <- dtUSTwtTestTermFreq_trigram[, sum(N), 
                                                           by = "Term"]
setnames(dtUSTwtTestTermFreq_trigram, old = "V1", new = "Count")

intUSTwtTestTriNum <- sum(dtUSTwtTestTermFreq_trigram$Count)
dtUSTwtTestTermFreq_trigram[, Percent := percent(Count/intUSTwtTestTriNum, 
                                                  digits = 8)]

write.csv(dtUSTwtTestTermFreq_trigram, 
          "./data/US_twitter_test_trigram_origin_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSTwtTestTermFreq_trigram[, Term_Lower := sapply(Term, try_tolower)]
dtUSTwtTestTermFreq_trigram_lower <- dtUSTwtTestTermFreq_trigram[
                                     !sapply(Term_Lower, term_in_set, 
                                     set_check_against = setStopwords),]
dtUSTwtTestTermFreq_trigram_lower <- dtUSTwtTestTermFreq_trigram_lower[
                                     sapply(Term_Lower, filter_ngram, 
                                     int_n = 3),]

dtUSTwtTestTermFreq_trigram_lower <- dtUSTwtTestTermFreq_trigram_lower[,
                                     sum(Count), by = "Term_Lower"]
setnames(dtUSTwtTestTermFreq_trigram_lower, old = "V1", new = "Count_Lower")

dtUSTwtTestTermFreq_trigram_lower[, Percent_Lower :=
                                  percent(Count_Lower/intUSNewsTestTriNum,
                                          digits = 8)]

write.csv(dtUSTwtTestTermFreq_trigram_lower, 
          "./data/US_twitter_test_trigram_lower_term_freq.csv",
          fileEncoding = FILE_ENCODING)

dtUSTwtShow_trigram <- dtUSTwtTestTermFreq_trigram_lower[
                       order(-rank(Count_Lower))][1:intTopTerm]


## Comapre Top 10 trigram terms of Blog, News, and Twitter
dtUSBlogShow_trigram[, Source := "US_Blog"]
dtUSNewsShow_trigram[, Source := "US_News"]
dtUSTwtShow_trigram[, Source := "US_Twitter"]

dtUSAllShow_trigram <- rbind(dtUSBlogShow_trigram, dtUSNewsShow_trigram,
                            dtUSTwtShow_trigram)

write.csv(dtUSAllShow_trigram, "./data/US_test_top_trigram_term_freq.csv",
          fileEncoding = FILE_ENCODING)

ggplot(data = dtUSAllShow_trigram, 
       aes(x = Term_Lower, y = Percent_Lower, fill = Source)) + 
    geom_bar(stat = "identity",  position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Top 10 Trigram") + scale_fill_discrete(name="Data Source") +
    xlab("Trigram") + ylab("Weight")



intTopTermRaw <- 20
# 1. Some words are more frequent than others - what are the distributions
# of word frequencies? 
dtUSBlogUTF <- fread("./data/US_blog_test_unigram_origin_term_freq.csv", 
                     encoding = "UTF-8")
dtUSBlogUTFTop <- dtUSBlogUTF[order(-rank(Count))][1:intTopTermRaw]
dtUSBlogUTFTop[, Source := "US_Blog"]

dtUSNewsUTF <- fread("./data/US_news_test_unigram_origin_term_freq.csv", 
                     encoding = "UTF-8")
dtUSNewsUTFTop <- dtUSNewsUTF[order(-rank(Count))][1:intTopTermRaw]
dtUSNewsUTFTop[, Source := "US_News"]

dtUSTwtUTF <- fread("./data/US_twitter_test_unigram_origin_term_freq.csv", 
                      encoding = "UTF-8")
dtUSTwtUTFTop <- dtUSTwtUTF[order(-rank(Count))][1:intTopTermRaw]
dtUSTwtUTFTop[, Source := "US_Twitter"]

dtUSAllUTFTop <- rbind(dtUSBlogUTFTop, dtUSNewsUTFTop, dtUSTwtUTFTop)
ggplot(data = dtUSAllUTFTop, 
       aes(x = Term, y = paste0(substr(Percent, 0, 4), "%"), fill = Source)) + 
    geom_bar(stat = "identity",  position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Top 10 Unigram - Raw Terms") + scale_fill_discrete(name="Data Source") +
    xlab("Unigram") + ylab("Weight")


dtUSBlogBTF <- fread("./data/US_blog_test_bigram_origin_term_freq.csv", 
                     encoding = "UTF-8")
dtUSBlogBTFTop <- dtUSBlogBTF[order(-rank(Count))][1:intTopTermRaw]
dtUSBlogBTFTop[, Source := "US_Blog"]

dtUSNewsBTF <- fread("./data/US_news_test_bigram_origin_term_freq.csv", 
                      encoding = "UTF-8")
dtUSNewsBTFTop <- dtUSNewsBTF[order(-rank(Count))][1:intTopTermRaw]
dtUSNewsBTFTop[, Source := "US_News"]

dtUSTwtBTF <- fread("./data/US_twitter_test_bigram_origin_term_freq.csv", 
                     encoding = "UTF-8")
dtUSTwtBTFTop <- dtUSTwtBTF[order(-rank(Count))][1:intTopTermRaw]
dtUSTwtBTFTop[, Source := "US_Twitter"]

dtUSAllBTFTop <- rbind(dtUSBlogBTFTop, dtUSNewsBTFTop, dtUSTwtBTFTop)
ggplot(data = dtUSAllBTFTop, 
       aes(x = Term, y = paste0(substr(Percent, 0, 4), "%"), fill = Source)) + 
    geom_bar(stat = "identity",  position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Top 10 Bigram - Raw Terms") + scale_fill_discrete(name="Data Source") +
    xlab("Bigram") + ylab("Weight")


dtUSBlogTTF <- fread("./data/US_blog_test_trigram_origin_term_freq.csv", 
                     encoding = "UTF-8")
dtUSBlogTTFTop <- dtUSBlogTTF[order(-rank(Count))][1:intTopTermRaw]
dtUSBlogTTFTop[, Source := "US_Blog"]

dtUSNewsTTF <- fread("./data/US_news_test_trigram_origin_term_freq.csv", 
                      encoding = "UTF-8")
dtUSNewsTTFTop <- dtUSNewsTTF[order(-rank(Count))][1:intTopTermRaw]
dtUSNewsTTFTop[, Source := "US_News"]

dtUSTwtTTF <- fread("./data/US_twitter_test_trigram_origin_term_freq.csv", 
                     encoding = "UTF-8")
dtUSTwtTTFTop <- dtUSTwtTTF[order(-rank(Count))][1:intTopTermRaw]
dtUSTwtTTFTop[, Source := "US_Twitter"]

dtUSAllTTFTop <- rbind(dtUSBlogTTFTop, dtUSNewsTTFTop, dtUSTwtTTFTop)
ggplot(data = dtUSAllTTFTop, 
       aes(x = Term, y = paste0(substr(Percent, 0, 6), "%"), fill = Source)) + 
    geom_bar(stat = "identity",  position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Top 10 Trigram - Raw Terms") + scale_fill_discrete(name="Data Source") +
    xlab("Trigram") + ylab("Weight")

# 3. How many unique words do you need in a frequency sorted dictionary 
# to cover 50% of all word instances in the language? 90%? 
dtUSBlogUTF_cumsum <- dtUSBlogUTF[order(-rank(Count))]
dtUSBlogUTF_cumsum[, Cum_Sum_Count := cumsum(Count)]
intUSBlogUTFCountSum <- last(dtUSBlogUTF_cumsum$Cum_Sum_Count)
dtUSBlogUTF_cumsum[, Cum_Percent := Cum_Sum_Count/intUSBlogUTFCountSum]
fUSBlog50Coverage <- nrow(dtUSBlogUTF_cumsum[Cum_Percent <= .5,])/
                     nrow(dtUSBlogUTF_cumsum)
fUSBlog90Coverage <- nrow(dtUSBlogUTF_cumsum[Cum_Percent <= .9,])/
                     nrow(dtUSBlogUTF_cumsum)
print(paste0("Top ", percent(fUSBlog50Coverage, digits = 4), 
             " of most frequent Unigram covers 50% of US blog content"))
print(paste0("Top ", percent(fUSBlog90Coverage), 
             " of most frequent Unigram covers 90% of US blog content"))


dtUSNewsUTF_cumsum <- dtUSNewsUTF[order(-rank(Count))]
dtUSNewsUTF_cumsum[, Cum_Sum_Count := cumsum(Count)]
intUSNewsUTFCountSum <- last(dtUSNewsUTF_cumsum$Cum_Sum_Count)
dtUSNewsUTF_cumsum[, Cum_Percent := Cum_Sum_Count/intUSNewsUTFCountSum]
fUSNews50Coverage <- nrow(dtUSNewsUTF_cumsum[Cum_Percent <= .5,])/
                     nrow(dtUSNewsUTF_cumsum)
fUSNews90Coverage <- nrow(dtUSNewsUTF_cumsum[Cum_Percent <= .9,])/
                      nrow(dtUSNewsUTF_cumsum)
print(paste0("Top ", percent(fUSNews50Coverage, digits = 4), 
             " of most frequent Unigram covers 50% of US News content"))
print(paste0("Top ", percent(fUSNews90Coverage), 
             " of most frequent Unigram covers 90% of US News content"))


dtUSTwtUTF_cumsum <- dtUSTwtUTF[order(-rank(Count))]
dtUSTwtUTF_cumsum[, Cum_Sum_Count := cumsum(Count)]
intUSTwtUTFCountSum <- last(dtUSTwtUTF_cumsum$Cum_Sum_Count)
dtUSTwtUTF_cumsum[, Cum_Percent := Cum_Sum_Count/intUSTwtUTFCountSum]
fUSTwt50Coverage <- nrow(dtUSTwtUTF_cumsum[Cum_Percent <= .5,])/
                    nrow(dtUSTwtUTF_cumsum)
fUSTwt90Coverage <- nrow(dtUSTwtUTF_cumsum[Cum_Percent <= .9,])/
                    nrow(dtUSTwtUTF_cumsum)
print(paste0("Top ", percent(fUSTwt50Coverage, digits = 4), 
             " of most frequent Unigram covers 50% of US Twitter content"))
print(paste0("Top ", percent(fUSTwt90Coverage), 
             " of most frequent Unigram covers 90% of US Twitter content"))

