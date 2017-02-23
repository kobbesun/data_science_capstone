library(hash)
library(tm)
library(data.table)
library(openNLP)
library(NLP)
library(ggplot2)

set.seed(226)
FILE_ENCODING = "UTF-8"


split_text <- function(input_file, sample_prob, sample_file, other_file, 
                       iteration_step = 1000, file_encoding = FILE_ENCODING) {
    if (file.exists(sample_file)) {
        file.remove(sample_file)
    }
    if (file.exists(other_file)) {
        file.remove(other_file)
    }

    conInput <- file(input_file, "r")
    conSample <- file(sample_file, "w", encoding = file_encoding)
    conOther <- file(other_file, "w", encoding = file_encoding)
    vInputLine <- readLines(conInput, n = iteration_step,
                            encoding = file_encoding)
    intLineLen <- length(vInputLine)
    vSampleLine <- NULL
    vOtherLine <- NULL
    
    while (intLineLen > 0) {
        vintBinom <- rbinom(n = intLineLen, size = 1, prob = sample_prob)
        for (i in 1:intLineLen) {
            if (vintBinom[i] == 1) {
                vSampleLine <- c(vSampleLine, vInputLine[i])
            } else {
                vOtherLine <- c(vOtherLine, vInputLine[i])
            }
        }
        writeLines(vSampleLine, con = conSample)
        writeLines(vOtherLine, con = conOther)
            
        vInputLine <- readLines(conInput, n = iteration_step,
                                encoding = file_encoding)
        intLineLen <- length(vInputLine)
        vSampleLine <- NULL
        vOtherLine <- NULL
    }
        
    
    close(conInput)
    close(conSample)
    close(conOther)
    
    return(vInputLine)
}



sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")

split_sent <- function(str_line) {
    annSentSpan <- annotate(str_line, sentence_token_annotator)
    return(as.String(str_line)[annSentSpan])
}


ngram_tokenizer <- function(doc, n_gram = 1) {
    unlist(lapply(ngrams(words(doc), n_gram), 
                  paste, collapse = " "), 
           use.names = FALSE)
}


unigram_tokenizer <- function(doc) {
    ngram_tokenizer(doc, n_gram = 1)
}


bigram_tokenizer <- function(doc) {
    ngram_tokenizer(doc, n_gram = 2)
}


trigram_tokenizer <- function(doc) {
    ngram_tokenizer(doc, n_gram = 3)
}


ngram_term_freq <- function(vcorp, tokenizer) {
    intLenCorp <- length(vcorp)
    vCtrl <- list(tokenize = tokenizer, tolower = FALSE,
                  removePunctuation = list(preserve_intra_word_dashes = TRUE),
                  #removePunctuation = TRUE,
                  removeNumbers = FALSE,
                  stopwords = FALSE,
                  stemming = FALSE,
                  dictionary = NULL,
                  #bounds = list(local = c(1, Inf)),
                  wordLengths = c(2, Inf))
    
    dtTermFreq <- data.table()
    for (i in 1:intLenCorp) {
        tryCatch({
            dtIter <- as.data.table(as.table(termFreq(vcorp[[i]], 
                                                      control = vCtrl)))
            setnames(dtIter, c("Term", "N"))
            dtTermFreq <- rbind(dtTermFreq, dtIter)
            
        }, warning = function(war) {
            #print(paste0("Warning on ", i, "th line in - ", 
            #             vcorp[[i]]$content))
            #print(paste0("Warning message - ", war))
            
        }, error = function(err) {
            #print(paste0("Error on ", i, "th line in - ", 
            #             vcorp[[i]]$content))
            #print(paste0("Erro message - ", err))
        })
    }
    
    return(dtTermFreq[, sum(N), by = "Term"])
    
}


# generate unigram, bigram, and trigram term frequence of the input file
generate_term_freq <- function(input_file, str_data_type, iteration_step = 1e4, 
                               file_encoding = FILE_ENCODING, lang = "en") {
    conInput <- file(input_file, "r")
    vInputLine <- readLines(conInput, n = iteration_step,
                            encoding = file_encoding)
    intLineLen <- length(vInputLine)
    dtUnigramTermFreq <- data.table()
    dtBigramTermFreq <- data.table()
    dtTrigramTermFreq <- data.table()
    
    intTenK <- 0
    while (intLineLen > 0) {
        print(paste0("File - ", input_file, " ", intTenK + 1, 
                     "*10K line processing starts..."))
        
        #ptm <- proc.time()
        
        vSent <- unlist(lapply(vInputLine, split_sent))
        print(paste0(intTenK + 1, "*10K line splits to ", length(vSent),
                    " sentences..."))
        print("create corpus starts...")
        vCorpSent <- VCorpus(VectorSource(vSent),
                             readerControl = list(language = lang))
        print("create corpus ends.")
        print("Unigram calculation starts...")
        dtUnigramTermFreq <- rbind(dtUnigramTermFreq, 
                                   ngram_term_freq(vCorpSent, 
                                                   unigram_tokenizer))
        print("Unigram calculation ends.")
        print("Bigram calculation starts...")
        dtBigramTermFreq <- rbind(dtBigramTermFreq, 
                                  ngram_term_freq(vCorpSent, 
                                                  bigram_tokenizer))
        print("Bigram calculation ends.")
        print("Triigram calculation starts...")
        dtTrigramTermFreq <- rbind(dtTrigramTermFreq, 
                                   ngram_term_freq(vCorpSent, 
                                                   trigram_tokenizer))
        print("Trigram calculation ends.")
        
        vInputLine <- readLines(conInput, n = iteration_step,
                                encoding = file_encoding)
        intLineLen <- length(vInputLine)
        
        print(paste0("File - ", input_file, " ", intTenK + 1, 
                     "*10K line processing ends."))
        intTenK <- intTenK + 1
        
        #print(proc.time() - ptm)
    }
    
close(conInput)

    write.csv(dtUnigramTermFreq, file =  paste0("./data/TermFreq_unigram_", 
                                                str_data_type, ".csv"))
    write.csv(dtBigramTermFreq, file =  paste0("./data/TermFreq_bigram_", 
                                                str_data_type, ".csv"))
    write.csv(dtTrigramTermFreq, file =  paste0("./data/TermFreq_trigram_", 
                                               str_data_type, ".csv"))
    
    return(list(dtUnigramTermFreq, dtBigramTermFreq, dtTrigramTermFreq))
}


percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

try_tolower <- function(str_char) {
    tryCatch({
        return(tolower(str_char))
    
    }, warning = function(war) {
        #print(paste0("Warning on term - ", str_char))
        #print(paste0("Warning message - ", war))
        return(paste0("^|", str_char))
        
    }, error = function(err) {
        #print(paste0("Error on term - ", str_char))
        #print(paste0("Error message - ", err))
        return(paste0("^|", str_char))
        
    })
}

term_in_set <- function(str_ngram, set_check_against, str_sep = " ") {
    tryCatch({
        vTerm <- strsplit(str_ngram, str_sep)
        vCheck <- sapply(vTerm, {function(x) x %in% set_check_against})
        #print(paste0("vCheck for ", vTerm, " - ", vCheck))
        return(any(vCheck, na.rm = TRUE))
    
        }, warning = function(war) {
        #print(paste0("Warning on term - ", str_char))
        #print(paste0("Warning message - ", war))
        return(FALSE)
        
    }, error = function(err) {
        #print(paste0("Error on term - ", str_char))
        #print(paste0("Error message - ", err))
        return(FALSE)
    })
}

filter_ngram <- function(str_ngram, int_n, str_sep = " ") {
    vTerm <- unlist(strsplit(str_ngram, str_sep))
    tryCatch({
        if (length(vTerm) != int_n) {
            return(FALSE)
        } else if (any(nchar(vTerm) == 0)) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }, warning = function(war) {
        return(FALSE)
        
    }, error = function(err) {
        return(FALSE)
        
    })
}


