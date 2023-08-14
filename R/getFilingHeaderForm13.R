#' Scrape EDGAR filing header information for From 13G/D
#'
#' \code{getFilingHeaderForm13} Extract EDGAR filing header information for From 13G/D
#'
#' getFilingHeaderForm13 function takes CIK(s), form types (SC 13G or SC 13D), 
#' and year(s) as input parameters. The function first imports available 
#' downloaded filings in local working directory Edgar filings_full text created by 
#' \link[edgar:getFilings]{getFilings} function; otherwise, it automatically downloads the 
#' filings which are not already been downloaded. It then parses all the important 
#' header information from filings. The function returns a data.frame with filing and 
#' header information. According to SEC EDGAR's guidelines a user also needs to 
#' declare user agent. The function uses the \link[future.apply:future_lapply]{future_lapply} 
#' function to extract information in parallel. Please set \link[future:plan]{plan} 
#' according to your needs before running the function. The progress bar
#' can be controlled with the \link[progressr]{progressr} package. See in particular 
#' \link[progressr:handlers]{handlers}
#' 
#'
#' @usage getFilingHeaderForm13(cik.no, form.type = c("SC 13G", "SC 13D"), 
#' filing.year, useragent, ...)
#'
#' @param cik.no vector of CIK(s) in integer format. Suppress leading
#' zeroes from CIKs. cik.no = 'ALL' conisders all the CIKs.
#'
#' @param form.type character vector containing form type to be downloaded.
#' form.type = c("SC 13G", "SC 13D") is the default.
#'
#' @param filing.year vector of four digit numeric year
#'
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#'
#' @param ... for passing arguments between functions
#'
#' @return Function returns dataframe containing the CIK number, company name,
#' date of filing, accession number, date of event, fiscal year end,
#' Standard Industrial Classification (SIC) code, Internal Revenue Code (IRS)
#' code, state of incorporation, business address, and mailing address.
#' The form type 13 contain multiple filers (i.e. the subject and the filer) hence the
#' output contains header information on both of them.
#'
#' @details The CUSIP number is for the subject (i.e. the same company for which the CIK was supplied).
#' To distinguish between subject and filer look at the filer.type column.
#' The subject.cusips are extracted as they are presented in the
#' filing. Some post-processing may be necessary. For instance, cusips for the same company sometimes have
#' varying lengths. When the event.period is NA it can often be set to the last day of the year.
#'
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
#' header.df <- getFilingHeaderForm13(cik.no = c('1000180', '38079'),
#'                          form.type = 'SC 13G', filing.year = 2006, useragent)
#'
#' header.df <- getFilingHeaderForm13(cik.no = '1000180', 'SC 13D',
#'                          filing.year = c(2005, 2006), useragent)
#'}
#' @importFrom stringr str_split
#' @importFrom anytime anydate
#' @importFrom XML htmlParse xpathSApply
#' @importFrom progressr progressor handlers
#' @importFrom future.apply future_lapply
#' @export

getFilingHeaderForm13 <-
  function(cik.no,
           form.type = c("SC 13G", "SC 13D"),
           filing.year,
           useragent = "",
           ...) {
    ### Check for valid user agent
    if (useragent != "") {
      # Check user agent
      bb <-
        any(
          grepl(
            "lonare.gunratan@gmail.com|glonare@uncc.edu|bharatspatil@gmail.com",
            useragent,
            ignore.case = T
          )
        )
      
      if (bb == TRUE) {
        cat(
          "Please provide a valid User Agent.
      Visit https://www.sec.gov/os/accessing-edgar-data
      for more information"
        )
        return()
      }
      
    } else{
      cat(
        "Please provide a valid User Agent.
      Visit https://www.sec.gov/os/accessing-edgar-data
      for more information"
      )
      return()
    }
    
    output <- getFilings(
      cik.no,
      form.type,
      filing.year,
      quarter = c(1, 2, 3, 4),
      downl.permit = "y",
      useragent
    )
    
    if (is.null(output)) {
      # cat('Please check the CIK number.')
      return()
    }
    
    cat("Scraping filing header information ...\n")
    
    output$quarter <- NULL
    output$filing.year <- NULL
    output$status <- NULL
    output$subject.cusip <- NA
    output$event.period <- NA
    
    p <- progressr::progressor(along = 1:nrow(output))

    ## Start scraping filing one by one and storing header information in main dataframe
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
      
      p(message = print(dest.filename))
      
      main.df <- output[i, ]
      
      #Get event date and cusip
      if (any(grepl(pattern = '<htm>|<html>|<html\\s', filing.text, ignore.case =
                    T))) {
        line <- grep("<HTML>|<html\\s", filing.text, ignore.case = T)
        line_end <- grep("</HTML>", filing.text, ignore.case = T)
        
        if(length(line_end) == 0){
          line <- grep("<text>|<text\\s", filing.text, ignore.case = T)
          line_end <- grep("</text>", filing.text, ignore.case = T)
        }
        
        filing.html <- filing.text[line:line_end]
        
        filing.html <-
          XML::htmlParse(
            filing.html,
            asText = TRUE,
            useInternalNodes = TRUE,
            addFinalizer = TRUE
          )
        filing.html <-
          XML::xpathSApply(
            filing.html,
            "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]",
            XML::xmlValue
          )
        
        filing.html <-
          iconv(filing.html, "latin1", "ASCII", sub = " ")
        
        filing.html <- gsub("\\t|\\n|\\s", " ", filing.html)
        #filing.html <- gsub("\\s{2,}", " ", filing.html)
        #filing.html <- gsub("^\\s{1,}", "", filing.html)
        # Check for empty lines and delete
        empty.lnumbers <- grep("^\\s*$", filing.html)
        
        if (length(empty.lnumbers) > 0) {
          filing.html <-
            filing.html[-empty.lnumbers]  ## Remove all lines only with space
        }
        
        event.period <- GetFilingEventDate(filing.html)
        
        cusip.no <- GetFilingCusip(filing.html)
      } else {
        
        filing.text <-
          iconv(filing.text, "latin1", "ASCII", sub = " ")
        
        filing.text <- gsub("\\t|\\n|\\s", " ", filing.text)
        #filing.text <- gsub("\\s{2,}", " ", filing.text)
        #filing.text <- gsub("^\\s{1,}", "", filing.text)
        # Check for empty lines and delete
        empty.lnumbers <- grep("^\\s*$", filing.text)
        
        if (length(empty.lnumbers) > 0) {
          filing.text <-
            filing.text[-empty.lnumbers]  ## Remove all lines only with space
        }
        event.period <- GetFilingEventDate(filing.text)
        
        cusip.no <- GetFilingCusip(filing.text)
      }
      
      main.df$subject.cusip <- cusip.no
      main.df$event.period <- event.period
      
      secheader_line <-
        grep("</SEC-HEADER>", filing.text, ignore.case = T)
      
      if (length(secheader_line) != 0) {
        secheader_line = secheader_line[1]
      }

      if (length(secheader_line) == 0) {
        secheader_line <-
          grep("</IMS-HEADER>", filing.text, ignore.case = T)

        if (length(secheader_line) != 0) {
          secheader_line <- secheader_line[1]
        }
      }

      if (length(secheader_line) == 0) {
        secheader_line <- grep("<DOCUMENT>", filing.text, ignore.case = T)

        if (length(secheader_line) != 0) {
          secheader_line <- secheader_line[1]
        } else {
          secheader_line <- 0
        }
      }
      
      if (length(secheader_line) != 0 & secheader_line > 8) {
        filing.text1 <- filing.text[1:(secheader_line - 1)]
        
        period.of.report <- GetConfirmedPeriodOfReport(filing.text1)
        
        filing.text1 <-
          filing.text1[grep("SUBJECT COMPANY|FILED BY|FILER", filing.text1,
                            ignore.case = T)[1]:length(filing.text1)]
        
        
        filing.text1 <- paste(filing.text1, collapse = "**00**")
        filing.text1 <- paste(filing.text1, "**00**")
        filing.text1 <- tolower(filing.text1)
        filing.text2 <- stringr::str_split(filing.text1, "(?=subject company)|(?=filed by)|(?=filer)")
        filing.text2 <- unlist(filing.text2)
        filing.text2 <- filing.text2[nzchar(filing.text2)]
        
        for (filer.no in 1:length(filing.text2)) {
          text <- filing.text2[filer.no]
          
          if (grepl("subject", text, ignore.case = T)) {
            filer.type <- "subject"
          } else {
            filer.type <- "filer"
          }
          
          header.df <-
            FilingHeaderSubFunc(text, filer.no, period.of.report, ...)
          header.df$filer.type <- filer.type
          
          if (filer.no == 1) {
            combined.df <- cbind(main.df, header.df)
          } else {
            combined.df <- rbind(combined.df, cbind(main.df, header.df))
          }
        }
        
      } else {
        period.of.report <- ""
        text <- ""
        filer.no <- NA
        header.df <-
          FilingHeaderSubFunc(text, filer.no, period.of.report)
        header.df$filer.type <- ""
        combined.df <- cbind(main.df, header.df)
        
      }
      
      combined.df[combined.df$filer.type == "filer", "subject.cusip"] <-
        NA_character_
      
      # check if all extracted states are valid state abbreviations
      states <-
        data.frame(state_abb = c(state.abb, "DC", "DC", "PR"),
                   state_name = tolower(
                     c(
                       state.name,
                       "District of Columbia",
                       "Washington D",
                       "Puerto Rico"
                     )
                   ))
      
      combined.df[!combined.df$state.of.incorp %in% tolower(states$state_abb), "state.of.incorp"] <-
        NA_character_
      
      combined.df[!combined.df$business.state %in% tolower(states$state_abb), "business.state"] <-
        NA_character_
      
      combined.df[!combined.df$mail.state %in% tolower(states$state_abb), "mail.state"] <-
        NA_character_
      
      return(combined.df)
  
    })

    main.output <- do.call(rbind, results)
 
    ## Make some columns uppercase
    vars <- c(
      "filer.company.name",
      "sic",
      "state.of.incorp",
      "business.street1",
      "business.street2",
      "business.city",
      "business.state",
      "mail.street1",
      "mail.street2",
      "mail.city",
      "mail.state"
    )
    
    main.output[vars] <- lapply(main.output[vars], toupper)
    
    ## Clean SIC CODE
    main.output$sic <- gsub("\\", "", main.output$sic, fixed = TRUE)
    main.output$sic <- gsub("'|/|,", "", main.output$sic)
    main.output$sic <-
      gsub(".*\\[(\\d{4})\\].*", "\\1", main.output$sic)
    main.output$sic <-
      gsub("\\[\\]", NA_character_, main.output$sic)
    main.output$sic[main.output$sic == "0000"] <- NA_character_

    ## convert dates into R dates
    main.output$date.filed <-
      as.Date(as.character(main.output$date.filed), "%Y-%m-%d")
    main.output$period.of.report <-
      anytime::anydate(as.character(main.output$period.of.report))
    main.output$event.period <-
      anytime::anydate(as.character(main.output$event.period))
    
    # clean cik and irs
    main.output$filer.cik <- as.numeric(main.output$filer.cik)
    main.output$irs[main.output$irs == "000000000"] <- NA_character_
    
    return(main.output)
  }

#' Scrape the event date from the filing text
#' @param filing.text the filing text
#' @return Returns date of the event that the filing covers
#' @importFrom stringr str_squish
#' @importFrom anytime anydate
#' @export

GetFilingEventDate <- function(filing.text) {
  event.period <- NA
  
  # Remove extra white space, the whitespace is good for cusip extraction but not for date extraction
  filing.text <- stringr::str_squish(filing.text)
  
  # check the most common text string
  if (any(
    grepl(
      "Date of Event Which Requires Filing of this Statement:",
      filing.text,
      ignore.case = T
    )
  )) {
    event.period <-
      filing.text[grep("Date of Event Which Requires Filing of this Statement",
                       filing.text,
                       ignore.case = T)]
    
    event.period <-
      stringr::str_squish(
        gsub(
          "Date of Event Which Requires Filing of this Statement:",
          "",
          event.period,
          ignore.case = T
        )
      )
  }
  
  if (length(event.period) > 1) {
    event.period <- event.period[1]
  }
  
  # if this fails check the next one
  if (is.na(event.period) &&
      any(grepl("\\(Date of Event|Date of Event", filing.text, ignore.case = T))) {
    line <-
      grep("\\(Date of Event|Date of Event", filing.text, ignore.case = T)
    
    if (length(line) > 1) {
      line <- line[1]
    }
    
    if (line < 5) {
      date.text <- filing.text[line]
      date.text <- gsub("-", "", date.text)
      date.text <- stringr::str_squish(date.text)
      event.period <-
        stringr::str_match(
          date.text,
          ".*?(\\b(\\d{1,2}/\\d{1,2}/\\d{4}|\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{2}|[A-Za-z\\.]+ \\d{1,2}, \\d{4})\\b).*?\\(Date (o|O)f (E|e)vent.*"
        )[3]
      
    } else {
      event.period <- filing.text[line - 1]
      if (grepl("-------|________", event.period, ignore.case = T)) {
        event.period <- filing.text[line - 2]
      }
    }
    
    event.period <- stringr::str_squish(event.period)
  }
  
  # come cleaning of the extracted text
  event.period <- gsub("\\[|\\]", "", event.period)
  event.period <- stringr::str_squish(event.period)
  
  # check if the extract text has a date format
  if (!is.na(event.period)) {
    event.period <-
      ifelse(is.na(anytime::anydate(event.period)),
             as.character(as.Date(event.period, format = "%m/%d/%y")),
             event.period)
  }
  
  # The above will not extract correctly if the date is below the identifier
  if (is.na(event.period) &&
      any(grepl("\\(Date of Event|Date of Event", filing.text, ignore.case = T))) {
    line <-
      grep("\\(Date of Event|Date of Event", filing.text, ignore.case = T)
    
    if (length(line) > 1) {
      line <- line[1]
    }
    
    event.period <- filing.text[line + 1]
    if (nchar(event.period) > 20) {
      event.period <- gsub("-", "", event.period)
      event.period <- stringr::str_squish(event.period)
      event.period <-
        stringr::str_match(
          event.period,
          ".*?(\\b(\\d{1,2}/\\d{1,2}/\\d{4}|\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{2}|[A-Za-z\\.]+ \\d{1,2}, \\d{4})\\b).*?\\(Date (o|O)f (E|e)vent.*"
        )[3]
    }
    
    if (!is.na(event.period)) {
      event.period <-
        ifelse(is.na(anytime::anydate(event.period)),
               as.character(as.Date(event.period, format = "%m/%d/%y")),
               event.period)
    }
    event.period <- gsub("\\[|\\]", "", event.period)
    event.period <- stringr::str_squish(event.period)
  }
  
  if (length(event.period) == 0 | is.na(event.period)) {
    event.period <- NA
  }
  
  return(event.period)
}

#' Scrape the CUSIP from the filing text
#' @param filing.text the filing text
#' @return Returns CUSIP of the Subject that the filing covers
#' @importFrom stringr str_squish str_detect str_remove_all str_match_all
#' @export

GetFilingCusip <- function(filing.text) {
  # set NA
  cusip.no <- NA_character_
  
  # check if there are any indentifiers in the text
  if (any(grepl("CUSIP|Item 2\\(e\\)", filing.text, ignore.case = T))) {
    # get relevant text
    cusip.text <-
      filing.text[grep("CUSIP|Item 2\\(e\\)", filing.text, ignore.case = T)]
    cusip.text <- gsub("-", " ", cusip.text)
    
    # CUSIP No and CUSIP number are the most frequent identifier
    cusip.no <-
      stringr::str_match_all(
        cusip.text,
        "(?i)\\b(?:CUSIP\\b\\s*(?:#|No|Number|\\s|\\:)|Item\\s2\\(e\\))[:.]*\\s*[:.]*(?!\\b(?:Number|Page|Cusip|13G|13D)\\b)(\\b[[:alnum:]\\s]{6,10}\\b)"
      )
    cusip.no <- sapply(cusip.no, "[", 2)
    
    # apply cleaning to make sure that an CUSIP number has been extracted
    cusip.no <-
      stringr::str_remove_all(
        cusip.no,
        "^[A-Za-z\\s]+$|\\b13G\\b|\\b13H\\b|\\bNo\\b|\\bCUSIP\\b|\\bNone\\b"
      )
    if (!any(is.na(cusip.no)) && any(nchar(cusip.no) > 10)) {
      cusip.no <- stringr::str_remove(cusip.no, "^US")
    }
    cusip.no <- gsub("[^[:alnum:]]", "", cusip.no)
    cusip.no <- gsub("^[A-Za-z\\s]+$", "", cusip.no)
    cusip.no <-
      gsub(
        "\\b[[:alnum:]]{1,5}\\b|\\b[[:alnum:]]{11,}\\b",
        "",
        cusip.no,
        perl = TRUE
      )
    cusip.no <- cusip.no[!is.na(cusip.no)]
    cusip.no <- cusip.no[!cusip.no == ""]
    # take the most frequent value
    cusip.no <- names(table(cusip.no))[which.max(table(cusip.no))]
    if (!is.null(cusip.no) && cusip.no == "") {
      cusip.no <- NA_character_
    }
    
    # if no CUSIP has been extracted, try to extract based on the location in the form
    if (is.null(cusip.no) &
        any(grepl("\\(CUSIP\\s+Number\\)|CUSIP\\s+No", filing.text, ignore.case = TRUE))) {
      line <-
        grep("\\(CUSIP\\s+Number\\)", filing.text, ignore.case = T)
      
      # clean lines
      if (length(line) == 0) {
        line <-
          grep("CUSIP\\s+Number", filing.text, ignore.case = T)
      }
      
      if (length(line) == 0) {
        line <-
          grep("CUSIP\\s+No", filing.text, ignore.case = T)
      }
      
      if (length(line) > 1) {
        line <- line[1]
      }
      
      cusip.no <- filing.text[line - 1]
      
      # Sometimes there is a different type of formatting
      if (length(cusip.no) != 0 && grepl("-------|________", cusip.no, ignore.case = T)) {
        cusip.no <- filing.text[line - 2]
      }
      
      # make sure that only something that looks like a CUSIP is extracted
      if (!any(is.na(cusip.no)) && any(nchar(cusip.no) > 15)) {
        cusip.no <- stringr::str_match(cusip.no, "\\s+((?![A-Za-z]+\\b)[[:alnum:]\\s]{6,10}\\b)")[2]
      }
      
      # apply cleaning to make sure that an CUSIP number has been extracted
      cusip.no <-
        stringr::str_remove_all(
          cusip.no,
          "\\b13G\\b|\\b13D\\b|\\bNo\\b|\\bCUSIP\\b|\\bNone\\b$"
        )
      cusip.no <- gsub("[^[:alnum:]]", "", cusip.no)
      cusip.no <- gsub("^[A-Za-z\\s]+$", "", cusip.no)
      
      # some include the country code in the CUSIP
      if (!any(is.na(cusip.no)) && any(nchar(cusip.no) > 10)) {
        cusip.no <- stringr::str_remove(cusip.no, "^US|^GB")
      }
      
      # apply cleaning to make sure that an CUSIP number has been extracted
      cusip.no <-
        gsub(
          "\\b[[:alnum:]]{1,5}\\b|\\b[[:alnum:]]{11,}\\b",
          "",
          cusip.no,
          perl = TRUE
        )
      cusip.no <- cusip.no[!is.na(cusip.no)]
      cusip.no <- cusip.no[!cusip.no == ""]
      
      if (length(cusip.no) == 0 ||
          is.na(cusip.no) || cusip.no == "NA") {
        cusip.no <- filing.text[line + 1]
      }
      
      if (grepl("-------|________|,", cusip.no, ignore.case = T)) {
        cusip.no <- NA_character_
      }
      
      cusip.no <-
        stringr::str_remove_all(
          cusip.no,
          "\\b13G\\b|\\b13D\\b|\\bNo\\b|\\bCUSIP\\b|\\bNone\\b"
        )
      cusip.no <- gsub("[^[:alnum:]]", "", cusip.no)
      cusip.no <- gsub("^[A-Za-z\\s]+$", "", cusip.no)
      
      if (!any(is.na(cusip.no)) && any(nchar(cusip.no) > 10)) {
        cusip.no <- stringr::str_remove(cusip.no, "^US")
      }
      cusip.no <-
        gsub(
          "\\b[[:alnum:]]{1,5}\\b|\\b[[:alnum:]]{11,}\\b",
          "",
          cusip.no,
          perl = TRUE
        )
      cusip.no <- cusip.no[!is.na(cusip.no)]
      cusip.no <- cusip.no[!cusip.no == ""]
      
      if (length(cusip.no) == 0 & line == 1) {
        cusip.text <-
          filing.text[grep("\\(CUSIP\\s+Number\\)", filing.text, ignore.case = T)]
        cusip.text <- gsub("-", " ", cusip.text)
        cusip.no <-
          stringr::str_match(cusip.text,
                             "(?i)\\(Title\\s+of\\s+Class\\s+of\\s+Securities\\)\\s+(\\b[[:alnum:]\\s]{6,10}\\b)\\s+(\\(\\bCUSIP\\b\\s+\\bNumber\\b\\))")[2]
        
        if (is.na(cusip.no)) {
          cusip.no <-
            stringr::str_match(cusip.text,
                               "(?i)\\(Title\\s+of\\s+Class\\s+of\\s+Securities\\)\\s+((?![A-Za-z]+\\b)[[:alnum:]]{6,10}\\b)")[2]
        }
        
        cusip.no <- gsub("[^[:alnum:]]", "", cusip.no)
        cusip.no <-
          gsub(
            "\\b[[:alnum:]]{1,5}\\b|\\b[[:alnum:]]{11,}\\b",
            "",
            cusip.no,
            perl = TRUE
          )
        cusip.no <- cusip.no[!is.na(cusip.no)]
        cusip.no <- cusip.no[!cusip.no == ""]
        if (length(cusip.no) == 0 || is.na(cusip.no) ||
            cusip.no == "" ||
            cusip.no == "NA" || nchar(cusip.no) > 10) {
          cusip.no <- NA_character_
        }
      }
    }
    
    cusip.no <- gsub("[^[:alnum:]]", "", cusip.no)
    cusip.no <- gsub("^[A-Za-z\\s]+$", "", cusip.no)
    
    if (length(cusip.no) == 0 || is.na(cusip.no) ||
        cusip.no == "" ||
        cusip.no == "NA" || nchar(cusip.no) > 10) {
      cusip.no <- NA_character_
    }
  }
  
  if (is.na(cusip.no) &
      any(grepl("^\\s*\\(e\\)|^\\s*e[:.]", filing.text, ignore.case = T))) {
    cusip.text <-
      filing.text[grep("^\\s*\\(e\\)|^\\s*e[:.]", filing.text, ignore.case = T)]
    cusip.text <- gsub("-", " ", cusip.text)
    cusip.no <-
      stringr::str_match(cusip.text,
                         "(^\\s*\\(e\\)|^\\s*e[:.]+)\\s*(\\b[[:alnum:]\\s]{6,10}\\b)")[3]
    cusip.no <- gsub("[^[:alnum:]]", "", cusip.no)
    cusip.no <- gsub("^[A-Za-z\\s]+$", "", cusip.no)
    
    cusip.no <-
      gsub(
        "\\b[[:alnum:]]{1,5}\\b|\\b[[:alnum:]]{11,}\\b",
        "",
        cusip.no,
        perl = TRUE
      )
    cusip.no <- cusip.no[!is.na(cusip.no)]
    cusip.no <- cusip.no[!cusip.no == ""]
    if (length(cusip.no) == 0 || is.na(cusip.no) ||
        cusip.no == "" ||
        cusip.no == "NA" || nchar(cusip.no) > 10) {
      cusip.no <- NA_character_
    }
  }

  cusip.no <- toupper(cusip.no)
  
  return(cusip.no)
}
globalVariables("cusip.no")