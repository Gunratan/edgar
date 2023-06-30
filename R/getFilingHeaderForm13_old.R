#' Scrape EDGAR filing header information for From 13G/D
#'
#' \code{getFilingHeaderForm13} Extract EDGAR filing header information for From 13G/D
#'
#' getFilingHeaderForm13 function takes CIK(s), form types (SC 13G or SC 13D), and year(s) as input parameters.
#' The function first imports available downloaded filings in local working directory
#' 'Edgar filings_full text' created by \link[edgar]{getFilings} function;
#' otherwise, it automatically downloads the filings which are not already been
#' downloaded. It then parses all the important header information from filings.
#' The function returns a data.frame with filing and header information. According
#' to SEC EDGAR's guidelines a user also needs to declare user agent.
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
#' varying lengths. When the period.of.report is NA it can often be set to the last day of the year.
#'
#'
#' @examples
#' \dontrun{
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
#' @export

getFilingHeaderForm13_old <-
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
    
    progress.bar <-
      txtProgressBar(min = 0,
                     max = nrow(output),
                     style = 3)
    
    output$quarter <- NULL
    output$filing.year <- NULL
    output$status <- NULL
    output$subject.cusip <- NA
    
    ## Start scraping filing one by one and storing header information in main dataframe
    main.output <- data.frame()
    
    for (i in 1:nrow(output)) {
      f.type <- gsub("/", "", output$form.type[i])
      
      dest.filename <-
        paste0(
          "Edgar filings_full text/Form ",
          f.type,
          "/",
          output$cik[i],
          "/",
          output$cik[i],
          "_",
          f.type,
          "_",
          output$date.filed[i],
          "_",
          output$accession.number[i],
          ".txt"
        )
      
      # Read filing
      filing.text <- readLines(dest.filename)
      
      main.df <- output[i, ]
      
      print(output$cik[i])
      
      #Get event date and cusip
      if (any(grepl(pattern = '<htm>|<html>|<html\\s', filing.text, ignore.case =
                    T))) {
        line <- grep("<HTML>|<html\\s", filing.text, ignore.case = T)
        line_end <- grep("</HTML>", filing.text, ignore.case = T)
        
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
        
        period.of.report <- GetFilingEventDate(filing.html)
        
        cusip.no <- GetFilingCusip(filing.html)
      } else {
        filing.text <- gsub("\\t|\\n|\\s", " ", filing.text)
        #filing.text <- gsub("\\s{2,}", " ", filing.text)
        #filing.text <- gsub("^\\s{1,}", "", filing.text)
        # Check for empty lines and delete
        empty.lnumbers <- grep("^\\s*$", filing.text)
        
        if (length(empty.lnumbers) > 0) {
          filing.text <-
            filing.text[-empty.lnumbers]  ## Remove all lines only with space
        }
        period.of.report <- GetFilingEventDate(filing.text)
        
        cusip.no <- GetFilingCusip(filing.text)
      }
      
      main.df$subject.cusip <- cusip.no
      
      secheader_line <-
        grep("</SEC-HEADER>", filing.text, ignore.case = T)
      
      if (length(secheader_line) != 0) {
        secheader_line = secheader_line[1]
      }
      #
      #      if (length(secheader_line) == 0) {
      #        secheader_line <-
      #          grep("</IMS-HEADER>", filing.text, ignore.case = T)
      #
      #        if (length(secheader_line) != 0) {
      #          secheader_line <- secheader_line[1]
      #        }
      #      }
      #
      #      if (length(secheader_line) == 0) {
      #        secheader_line <- grep("<DOCUMENT>", filing.text, ignore.case = T)
      #
      #        if (length(secheader_line) != 0) {
      #          secheader_line <- secheader_line[1]
      #        } else {
      #          secheader_line <- 0
      #        }
      #      }
      
      if (length(secheader_line) != 0 & secheader_line > 8) {
        filing.text1 <- filing.text[1:(secheader_line - 1)]
        
        filing.text1 <-
          filing.text1[grep("SUBJECT COMPANY|FILED BY", filing.text1,
                            ignore.case = T)[1]:length(filing.text1)]
        
        
        filing.text1 <- paste(filing.text1, collapse = "**00**")
        filing.text1 <- paste(filing.text1, "**00**")
        filing.text1 <- tolower(filing.text1)
        if (startsWith(filing.text1, "subject company")) {
          filing.text2 <- stringr::str_split(filing.text1, "(?=filed by)")
        } else {
          filing.text2 <-
            stringr::str_split(filing.text1, "(?=subject company)")
        }
        filing.text2 <- unlist(filing.text2)
        
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
        header.df <-
          FilingHeaderSubFunc(text, filer.no, period.of.report)
        combined.df <- cbind(main.df, header.df)
        
      }
      
      combined.df[!combined.df$filer.type == "subject", "subject.cusip"] <-
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
      
      main.output <- rbind(main.output, combined.df)
      
      # update progress bar
      setTxtProgressBar(progress.bar, i)
      
    }
    
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
    main.output$sic[main.output$sic == "0000"] <- NA_character_
    
    ## convert dates into R dates
    main.output$date.filed <-
      as.Date(as.character(main.output$date.filed), "%Y-%m-%d")
    main.output$period.of.report <-
      anytime::anydate(as.character(main.output$period.of.report))
    main.output$filer.cik <- as.numeric(main.output$filer.cik)
    
    # Close progress bar
    close(progress.bar)
    
    return(main.output)
  }