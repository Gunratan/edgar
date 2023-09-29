#' Provides sentiment measures of EDGAR filings
#'
#' \code{getSentiment} computes sentiment measures of EDGAR filings
#'
#' getSentiment function takes CIK(s), form type(s), and year(s) as input parameters.  
#' The function first imports available downloaded filings in the local working directory 
#' 'Edgar filings_full text' created by \link[edgar]{getFilings} function; otherwise, 
#' it automatically downloads the filings which are not already been downloaded.
#' It then reads, cleans, and computes sentiment measures for these filings. 
#' The function returns a dataframe with filing information and sentiment measures.
#' According to SEC EDGAR's guidelines a user also needs to declare user agent. 
#' 
#' @usage getSentiment(cik.no, form.type, filing.year, useragent)
#' 
#' @param cik.no vector of CIK number of firms in integer format. Suppress leading 
#' zeroes from CIKs. Keep cik.no = 'ALL' if needs to download for all CIKs.
#'  
#' @param form.type character vector containing form type to be downloaded. 
#' form.type = 'ALL' if need to download all forms.  
#'
#' @param filing.year vector of four digit numeric year
#' 
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#' 
#' @return Function returns dataframe containing CIK number, company name, 
#' date of filing, accession number, and various sentiment measures. 
#' This function takes the help of Loughran-McDonald (L&M) sentiment 
#' dictionaries (https://sraf.nd.edu/textual-analysis/resources/) to 
#' compute sentiment measures of a EDGAR filing. Following are the 
#' definitions of the text characteristics and the sentiment measures:
#' 
#' file.size = The filing size of a complete filing on the EDGAR server in 
#' kilobyte (KB).
#' 
#' word.count = The total number of words in a filing text, excluding HTML 
#' tags and exhibits text.
#' 
#' unique.word.count = The total number of unique words in a filing text, 
#' excluding HTML tags and exhibits text.
#' 
#' stopword.count = The total number of stop words in a filing text, 
#' excluding exhibits text.
#' 
#' char.count = The total number of characters in a filing text, excluding 
#' HTML tags and exhibits text.
#' 
#' complex.word.count = The total number of complex words in the filing text. 
#' When vowels (a, e, i, o, u) occur more than three times in a word, then 
#' that word is identified as a complex word.
#' 
#' lm.dictionary.count = The number of words in the filing text that occur 
#' in the Loughran-McDonald (LM) master dictionary.
#' 
#' lm.negative.count = The number of LM financial-negative words in the 
#' document.
#' 
#' lm.positive.count = The number of LM financial-positive words in the 
#' document.
#' 
#' lm.strong.modal.count = The number of LM financial-strong modal words 
#' in the document.
#' 
#' lm.moderate.modal.count = The number of LM financial-moderate Modal 
#' words in the document.
#' 
#' lm.weak.modal.count = The number of LM financial-weak modal words in 
#' the document.
#' 
#' lm.uncertainty.count = The number of LM financial-uncertainty words 
#' in the document.
#' 
#' lm.litigious.count = The number of LM financial-litigious words in 
#' the document.
#' 
#' hv.negative.count = The number of words in the document that occur in 
#' the 'Harvard General Inquirer' Negative word list, as defined by LM. 
#' 
#' @examples
#' \dontrun{
#' library(future.apply)
#' plan(multisession, workers = availableCores()-1)
#' 
#' # if a progress update is desired
#' library(progressr)
#' handlers(global = TRUE)
#' 
#' senti.df <- getSentiment(cik.no = c('1000180', '38079'), 
#'                          form.type = '10-K', filing.year = 2006, useragent) 
#'                          
#' ## Returns dataframe with sentiment measures of firms with CIKs 
#' 1000180 and 38079 filed in year 2006 for form type '10-K'.
#' 
#' senti.df <- getSentiment(cik.no = '38079', form.type = c('10-K', '10-Q'), 
#'                          filing.year = c(2005, 2006), useragent)
#'}
#' @export
#' @importFrom XML htmlParse xpathSApply xmlValue
#' @importFrom progressr progressor handlers
#' @importFrom future.apply future_lapply

getSentiment <- function(cik.no, form.type, filing.year, useragent= "") {
    
  
  ### Check for valid user agent
  if(useragent != ""){
    # Check user agent
    bb <- any(grepl( "lonare.gunratan@gmail.com|glonare@uncc.edu|bharatspatil@gmail.com",
                     useragent, ignore.case = T))
    
    if(bb == TRUE){
      
      cat("Please provide a valid User Agent. 
      Visit https://www.sec.gov/os/accessing-edgar-data 
      for more information")
      return()
    }
    
  }else{
    
    cat("Please provide a valid User Agent. 
      Visit https://www.sec.gov/os/accessing-edgar-data 
      for more information")
    return()
  }
  
  
    output <- getFilings(cik.no, form.type, filing.year, quarter = c(1, 2, 3, 4), 
						 downl.permit = "y", useragent)
    
    if (is.null(output)){
      # cat("Please check the CIK number.")
      return()
    }
   	
    cat("Computing sentiment measures...\n")
    
    # load Loughran & McDonald Master Dictionary and import word lists
    LoadLMDictionary <- function() {
      
      data("LMMasterDictionary")
      
      uncertainty <- ifelse(LMMasterDictionary$uncertainty != 0, LMMasterDictionary$word, NA)
      uncertainty <- uncertainty[!is.na(uncertainty)]
      
      negative <- ifelse(LMMasterDictionary$negative != 0, LMMasterDictionary$word, NA)
      negative <- negative[!is.na(negative)]
      
      positive <- ifelse(LMMasterDictionary$positive != 0, LMMasterDictionary$word, NA)
      positive <- positive[!is.na(positive)]
      
      litigious <- ifelse(LMMasterDictionary$litigious != 0, LMMasterDictionary$word, NA)
      litigious <- litigious[!is.na(litigious)]
      
      strong.modal <- ifelse(LMMasterDictionary$modal == 1, LMMasterDictionary$word, NA)
      strong.modal <- strong.modal[!is.na(strong.modal)]
      
      moderate.modal <- ifelse(LMMasterDictionary$modal == 2, LMMasterDictionary$word, NA)
      moderate.modal <- moderate.modal[!is.na(moderate.modal)]
      
      weak.modal <- ifelse(LMMasterDictionary$modal == 3, LMMasterDictionary$word, NA)
      weak.modal <- weak.modal[!is.na(weak.modal)]
      
      harvard.iv <- ifelse(LMMasterDictionary$harvard_iv != 0, LMMasterDictionary$word, NA)
      harvard.iv <- harvard.iv[!is.na(harvard.iv)]
      
      lm.out <- list(LMMasterDictionary = LMMasterDictionary, uncertainty = uncertainty, 
                     negative = negative, positive = positive, 
                     litigious = litigious, strong.modal =strong.modal,
                     moderate.modal = moderate.modal, weak.modal = weak.modal,
                     harvard.iv = harvard.iv)
      
      return(lm.out)
    }
    
    
    ## Load LM dictionaries
    lm.dict <- LoadLMDictionary()

    p <- progressr::progressor(along = 1:nrow(output))
    
    output$file.size <- NA
    output$word.count <- NA
    output$unique.word.count <- NA
    output$stopword.count <- NA
    output$char.count <- NA
    output$complex.word.count  <- NA
    output$lm.dictionary.count <- NA
    output$lm.negative.count <- NA
    output$lm.positive.count <- NA
    output$lm.strong.modal.count <- NA
    output$lm.moderate.modal.count <- NA
    output$lm.weak.modal.count <- NA
    output$lm.uncertainty.count <- NA
    output$lm.litigious.count <- NA
    output$hv.negative.count <- NA
    
    results <- future.apply::future_lapply(
      X = 1:nrow(output),
      FUN = function(i) {
        
        dest.filename <-
          paste0(getwd(),"/",
                 "Edgar filings_full text/Form ",
                 gsub("/", "", output$form.type[i]),
                 "/",
                 output$cik[i],
                 "/",
                 output$cik[i],
                 "_",
                 gsub("/", "", output$form.type[i]),
                 "_",
                 output$date.filed[i],
                 "_",
                 output$accession.number[i],
                 ".txt"
          )
        
        # Read filing
        filing.text <- readLines(dest.filename)
        
        file.size <- round(file.info(dest.filename)$size/1024, 0)  ## file size in kilobyte (KB)
        
        # Extract data from first <DOCUMENT> to </DOCUMENT>
        tryCatch({
          filing.text <- filing.text[(grep("<DOCUMENT>", filing.text, ignore.case = TRUE)[1]):(grep("</DOCUMENT>", 
                                                                                                    filing.text, ignore.case = TRUE)[1])]
        }, error = function(e) {
          filing.text <- filing.text ## In case opening and closing DOCUMENT TAG not found, consider full web page
        })
        
        # See if 10-K is in XLBR or old text format
        if (any(grepl(pattern = "<xml>|<type>xml|<html>|10k.htm", filing.text, ignore.case = T))) {
            
            doc <- XML::htmlParse(filing.text, asText = TRUE, useInternalNodes = TRUE, addFinalizer = TRUE)
			
            f.text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
                XML::xmlValue)
            f.text <- iconv(f.text, "latin1", "ASCII", sub = " ")

        } else {
            f.text <- filing.text
        }
        
        # Preprocessing the filing text
        f.text <- gsub("\\n|\\t|,", " ", f.text)
        f.text <- paste(f.text, collapse=" ")
        f.text <- gsub("/s/", "", f.text, fixed = T)
        f.text <- gsub("'s ", "", f.text)
        f.text <- gsub("[[:punct:]]", "", f.text, perl=T)
        f.text <- gsub("[[:digit:]]", "", f.text, perl=T)
        f.text <- iconv(f.text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
        f.text <- tolower(f.text)
        f.text <- gsub("\\s{2,}", " ", f.text)
        
        ### Clean text and find number of total words
        text_words <- unlist(strsplit(f.text, " "))
        text_df <- data.frame(word = unlist(text_words), nchar = nchar(text_words))
        text_df <- text_df[text_df$nchar >=3, ]
        word.count <- nrow(text_df)   # Word count
        char.count <- sum(text_df$nchar) # Character count
        stopword.count <- sum(text_df$word %in% tm::stopwords("en")) # Number of stop words
        unique.word.count <- length(unique(trimws(text_df$word, "both"))) # Number of unique words
        
        # Complex word count
        syllables.counts <- sapply(regmatches(text_df$word, gregexpr("[aeiouy]", 
                                                                     text_df$word, ignore.case = T)), length)
        syllables.counts <- syllables.counts[syllables.counts >= 3]
        complex.word.count <- length(syllables.counts)
        
        ############################## Sentiment Measures #############################
        lm.master.dict <- data.frame(word = unique(lm.dict$LMMasterDictionary$word))
        lm.master.merge <- merge(text_df, lm.master.dict, by = "word")
        lm.dictionary.count <- nrow(lm.master.merge)
        
        # Loughran-McDonald Negative word proportion
        lm.negative.count <-  nrow(text_df[which(text_df$word %in% lm.dict$negative), ])

        # Loughran-McDonald positive word proportion
        lm.positive.count <-  nrow(text_df[which(text_df$word %in% lm.dict$positive), ])

        # Loughran-McDonald strong.modal word proportion
        lm.strong.modal.count <-  nrow(text_df[which(text_df$word %in% lm.dict$strong.modal), ])

        # Loughran-McDonald moderate.modal word proportion
        lm.moderate.modal.count <-  nrow(text_df[which(text_df$word %in% lm.dict$moderate.modal), ])
        
        # Loughran-McDonald weak.modal word proportion
        lm.weak.modal.count <-  nrow(text_df[which(text_df$word %in% lm.dict$weak.modal), ])
        
        # Loughran-McDonald uncertainty word proportion
        lm.uncertainty.count <-  nrow(text_df[which(text_df$word %in% lm.dict$uncertainty), ])
        
        # Loughran-McDonald litigious word proportion
        lm.litigious.count <-  nrow(text_df[which(text_df$word %in% lm.dict$litigious), ])
        
        # hv.negative.count
        hv.negative.count <-  nrow(text_df[which(text_df$word %in% lm.dict$harvard.iv), ])
        
        # Assign all the variables
        output$file.size[i] <- file.size
        output$word.count[i] <- word.count
        output$unique.word.count[i] <- unique.word.count
        output$stopword.count[i] <- stopword.count
        output$char.count[i] <- char.count
        output$complex.word.count [i] <- complex.word.count 
        output$lm.dictionary.count[i] <- lm.dictionary.count
        output$lm.negative.count[i] <- lm.negative.count
        output$lm.positive.count[i] <- lm.positive.count
        output$lm.strong.modal.count[i] <- lm.strong.modal.count
        output$lm.moderate.modal.count[i] <- lm.moderate.modal.count
        output$lm.weak.modal.count[i] <- lm.weak.modal.count
        output$lm.uncertainty.count[i] <- lm.uncertainty.count
        output$lm.litigious.count[i] <- lm.litigious.count
        output$hv.negative.count[i] <- hv.negative.count
        
        if (i %% 10 == 0) {p()}
        return(output)
    })
    
    output <- do.call(rbind, results)
    
    #names(output)[names(output) == 'status'] <- 'downld.status'
    output$status <- NULL
    output$quarter <- NULL
    output$filing.year <- NULL
    
    ## convert dates into R dates
    output$date.filed <- as.Date(as.character(output$date.filed), "%Y-%m-%d")

    return(output)
}

globalVariables('LMMasterDictionary')