#' Scrape EDGAR filing header information
#'
#' \code{getFilingHeader} Extract EDGAR filing header information
#'
#' getFilingHeader function takes CIK(s), form type(s), and year(s) as input parameters.
#' The function first imports available downloaded filings in local woking directory
#' 'Edgar filings_full text' created by \link[edgar:getFilings]{getFilings} function;
#' otherwise, it automatically downloads the filings which are not already been
#' downloaded. It then parses all the important header information from filings.
#' The function returns a dataframe with filing and header information. According
#' to SEC EDGAR's guidelines a user also needs to declare user agent.
#' The function uses the \link[future.apply:future_lapply]{future_lapply} 
#' function to extract information in parallel. Please set \link[future:plan]{plan} 
#' according to your needs before running the function. The progress bar
#' can be controlled with the \link[progressr]{progressr} package. See in particular 
#' \link[progressr:handlers]{handlers} 
#'
#' @usage getFilingHeader(cik.no, form.type, filing.year, useragent,
#' marker = c("state or other jur", "state of inco", "incorporated under",
#'"state or jurisdiction", "state or other", "incorporated in",
#'"\\\\(state of", "organized under the laws of", "incorporation",
#'"jurisdiction"), nmax = 500, ...)
#'
#' @param cik.no vector of CIK(s) in integer format. Suppress leading
#' zeroes from CIKs. cik.no = 'ALL' conisders all the CIKs.
#'
#' @param form.type character vector containing form type to be downloaded.
#' form.type = 'ALL' if need to download all forms.
#'
#' @param filing.year vector of four digit numeric year
#'
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#'
#' @param marker a character string of text markers to use when looking for the
#' state of incorporation in free text of the report. Defaults to the following:
#' c("state or other jur", "state of inco", "incorporated under",
#' "state or jurisdiction", "state or other", "incorporated in", "\\(state of",
#' "organized under the laws of", "incorporation", "jurisdiction"). The function
#' also looks for text such as "a New York Corporation" to infer the state of
#' incorporation (New York, in this case).
#'
#' @param nmax the number of lines in the report to use when looking for the
#' state of incorporation in free text of the report. Defaults to 500
#'
#' @param ... for passing arguments between functions
#'
#' @return Function returns dataframe containing CIK number, company name,
#' date of filing, accession number, confirmed period of report, fiscal year end,
#' Standard Industrial Classification (SIC) code, Internal Revenue Code (IRS)
#' code, state of incorporation, business address, and mailing address. If a
#' filing contains multiple filers then output will contain header information on
#' all the filers in multiple rows.
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
#' header.df <- getFilingHeader(cik.no = c('1000180', '38079'),
#'                          form.type = '10-K', filing.year = 2006, useragent)
#'
#' header.df <- getFilingHeader(cik.no = '38079', c('10-K', '10-Q'),
#'                          filing.year = c(2005, 2006), useragent)
#'}
#' @export

getFilingHeader <-
  function(cik.no,
           form.type,
           filing.year,
           useragent = "",
           marker = c(
             "state or other jur",
             "state of inco",
             "incorporated under",
             "state or jurisdiction",
             "state or other",
             "incorporated in",
             "\\(state of",
             "organized under the laws of",
             "incorporation",
             "jurisdiction"
           ),
           nmax = 500,
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

    p <- progressr::progressor(along = 1:nrow(output))
    
    ## Start scraping filing one by one and storing header information in main dataframe
    results <- future.apply::future_lapply(
      X = 1:nrow(output),
      FUN = function(i) {
      f.type <- gsub("/", "", output$form.type[i])
      
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
      
      main.df <- output[i, ]
      
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
      
      if (length(secheader_line) != 0 & secheader_line > 18) {
        filing.text1 <- filing.text[1:(secheader_line - 1)]
        
        filing.text1 <- gsub("</FILER>", "", filing.text1)
        
        period.of.report <- GetConfirmedPeriodOfReport(filing.text1)
        
        filing.text1 <- gsub("\\t|\\n|\\s", " ", filing.text1)
        filing.text1 <- gsub("\\s{2,}", " ", filing.text1)
        
        
        filing.text1 <- filing.text1[grep("FILER", filing.text1,
                                          ignore.case = T)[1]:length(filing.text1)]
        
        
        filing.text1 <- paste(filing.text1, collapse = "**00**")
        filing.text1 <- paste(filing.text1, "**00**")
        filing.text1 <- tolower(filing.text1)
        filing.text2 <- strsplit(filing.text1, "filer")
        filing.text2 <- unlist(filing.text2)
        filing.text2 <- filing.text2[-1]
        
        
        for (filer.no in 1:length(filing.text2)) {
          text <- filing.text2[filer.no]
          header.df <-
            FilingHeaderSubFunc(text, filer.no, period.of.report, ...)
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
      
      # check if all extracted states are valid state abbreviations
      states <- data.frame(state_abb = c(state.abb, "DC", "DC", "PR"),
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
      
      # if state of incorporation is NA try to extract form metadata
      if (any(is.na(combined.df$state.of.incorp))) {
        if (any(grepl("<XBRL>", filing.text))) {
          documents <-
            grep("<DOCUMENT>", filing.text, ignore.case = T)
          
          documents_end <-
            grep("</DOCUMENT>", filing.text, ignore.case = T)
          
          # extract the first document
          state_text <-
            filing.text[(documents[1]):(documents_end[1])]
          text_start <-
            grep("<XBRL>", state_text, ignore.case = T)
          text_end <-
            grep("</XBRL>", state_text, ignore.case = T)
          if (length(text_start) == 1 & length(text_end) == 1) {
            state_text <- state_text[(text_start + 1):(text_end - 1)]
            state_text <-
              XML::htmlParse(
                state_text,
                asText = TRUE,
                useInternalNodes = TRUE,
                addFinalizer = FALSE
              )
            state.of.incorp <-
              XML::xpathSApply(
                state_text,
                '//*[@name="dei:EntityIncorporationStateCountryCode"]',
                XML::xmlValue
              )
            
            # convert to state abbreviations if not yet in abbreviation
            if ((!is.null(state.of.incorp) && any(!is.na(state.of.incorp))) && (length(state.of.incorp) != 0 && any(nchar(state.of.incorp) > 2))) {
              for (st in which(nchar(state.of.incorp) > 2)) {
                state.abb.match <- states$state_abb[match(tolower(state.of.incorp)[st], tolower(states$state_name))]
                state.of.incorp[st] <- state.abb.match
              }
            }
  
            if (length(state.of.incorp) == length(combined.df$state.of.incorp) & all(is.na(combined.df$state.of.incorp)) & all(!is.na(state.of.incorp))) {
              combined.df$state.of.incorp <- state.of.incorp
            }
            
            if (length(state.of.incorp) == length(combined.df$state.of.incorp) & any(is.na(combined.df$state.of.incorp))) {
              na_indices <- which(is.na(combined.df$state.of.incorp))
              combined.df[na_indices, "state.of.incorp"] <- state.of.incorp[na_indices]
            }
            
            if (length(state.of.incorp[!is.na(state.of.incorp)]) == length(combined.df$state.of.incorp) & any(is.na(combined.df$state.of.incorp))) {
              na_indices <- which(is.na(combined.df$state.of.incorp))
              combined.df[na_indices, "state.of.incorp"] <- state.of.incorp[na_indices]
            }
          }
        }
      }
      
      combined.df[!tolower(combined.df$state.of.incorp) %in% tolower(states$state_abb), "state.of.incorp"] <-
        NA_character_
      
      if (any(is.na(combined.df$state.of.incorp))) {
        if (any(grepl("<XBRL>", filing.text))) {
          documents <-
            grep("<DOCUMENT>", filing.text, ignore.case = T)
          
          documents_end <-
            grep("</DOCUMENT>", filing.text, ignore.case = T)
          
          # extract the first document
          state_text <-
            filing.text[(documents[1]):(documents_end[1])]
          text_start <-
            grep("<XBRL>", state_text, ignore.case = T)
          text_end <-
            grep("</XBRL>", state_text, ignore.case = T)
          if (length(text_start) == 1 & length(text_end) == 1) {
            state_text <- state_text[(text_start + 1):(text_end - 1)]
            state_text <-
              XML::htmlParse(
                state_text,
                asText = TRUE,
                useInternalNodes = TRUE,
                addFinalizer = FALSE
              )
            state_text <-
              XML::xpathSApply(state_text, "//div", XML::xmlValue)
            state_text <-
              iconv(state_text, "latin1", "ASCII", sub = " ")
            state_text <- state_text[1:nmax]
          }
          
        } else{
          state_text <- filing.text[1:nmax]
        }
        
        if (any(nchar(state_text) > 1) && any(is.na(combined.df$state.of.incorp))) {
          for (st in which(is.na(combined.df$state.of.incorp))) {
            combined.df$state.of.incorp[st] <- extractState(state_text, marker = marker)
          }
        }
      }
      
      if (i %% 10 == 0) {p()}
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
    main.output$sic[main.output$sic == "0000"] <- NA_character_
    
    ## convert dates into R dates
    main.output$date.filed <-
      as.Date(as.character(main.output$date.filed), "%Y-%m-%d")
    main.output$period.of.report <-
      as.Date(as.character(main.output$period.of.report), "%Y%m%d")

    # clean cik and irs
    main.output$filer.cik <- as.numeric(main.output$filer.cik)
    main.output$irs[main.output$irs == "000000000"] <- NA_character_
    
    return(main.output)
  }

#' Scrape the reporting period of the header from the filing text
#' @param header the filing text
#' @return Returns that period that the filing covers
#' @export

GetConfirmedPeriodOfReport <- function(header) {
  period.of.report <- NA_character_
  
  if (any(grepl("CONFORMED PERIOD OF REPORT", header, ignore.case = T))) {
    period.of.report <- header[grep("CONFORMED PERIOD OF REPORT",
                                    header, ignore.case = T)]
    
    period.of.report <- gsub("CONFORMED PERIOD OF REPORT:|\t|\\s",
                             "",
                             period.of.report,
                             ignore.case = T)
    
    if (length(period.of.report) == 0) {
      period.of.report <- NA_character_
    }
    
    if (length(period.of.report) > 1) {
      period.of.report <- paste(period.of.report, collapse = "_and_")
    }
  }
  return(period.of.report)
}
globalVariables("period.of.report")

#' Scrape the filer information filing text headers for standard documents
#'
#' @param text the filing text
#' @param filer.no the number of the filer, in some cases there are multiple filers
#' when run as part of the getFilingHeader function, it extracts information for all filers.
#' @param period.of.report the reporting period. When run as part of the getFilingHeader
#' function, it extracts this information from the text of the report
#' @param ... for passing arguments between functions
#' @return Returns a data.frame with the filing information extracted
#' @export
#' @importFrom qdapRegex rm_between
#' @importFrom stringi stri_extract_all_regex
#' @importFrom stringr str_squish


FilingHeaderSubFunc <-
  function(text, filer.no, period.of.report, ...) {
    ## Capture CONFORMED COMPANY NAME
    company.name <-
      qdapRegex::rm_between(text, "company conformed name",
                            "**00**", extract = TRUE)[[1]]
    
    if (!is.na(company.name)) {
      company.name <- str_squish(gsub(":", "", company.name))
    } else {
      company.name = NA_character_
    }
    
    ## Capture cik
    cik <- qdapRegex::rm_between(text, "central index key",
                                 "**00**", extract = TRUE)[[1]]
    
    if (!is.na(cik)) {
      cik <- str_squish(gsub(":", "", cik))
    } else {
      cik = NA_character_
    }
    
    ## Capture SIC code
    sic <-
      qdapRegex::rm_between(text,
                            "standard industrial classification",
                            "**00**",
                            extract = TRUE)[[1]]
    
    if (!is.na(sic)) {
      sic <- str_squish(gsub(":", "", sic))
    } else {
      sic = NA_character_
    }
    
    ## Capture IRS number
    irs <- qdapRegex::rm_between(text, "irs number", "**00**",
                                 extract = TRUE)[[1]]
    
    if (!is.na(irs)) {
      irs <- str_squish(gsub(":", "", irs))
    } else {
      irs = NA_character_
    }
    
    ## Capture state of incorporation
    state.of.incorp <-
      qdapRegex::rm_between(text, "state of incorporation",
                            "**00**", extract = TRUE)[[1]]
    
    if (!is.na(state.of.incorp)) {
      state.of.incorp <- str_squish(gsub(":", "", state.of.incorp))
    } else {
      state.of.incorp = NA_character_
    }
    
    ## Capture fiscal year end
    fiscal.yr.end <- qdapRegex::rm_between(text, "fiscal year end",
                                           "**00**", extract = TRUE)[[1]]
    
    if (!is.na(fiscal.yr.end)) {
      fiscal.yr.end <- str_squish(gsub(":", "", fiscal.yr.end))
    } else {
      fiscal.yr.end = NA_character_
    }
    
    ######## BUSINESS ADDRESS: ########
    business.addr <-
      qdapRegex::rm_between(text, "business address",
                            "mail address", extract = TRUE)[[1]]
    
    if (is.na(business.addr)) {
      business.addr <- stringi::stri_extract_all_regex(text,
                                                       "(business address).*?$")[[1]]
    }
    
    
    ## Street 1
    business.street1 <- qdapRegex::rm_between(business.addr,
                                              "street 1:", "**00**", extract = TRUE)[[1]]
    
    if (!is.na(business.street1)) {
      business.street1 <- str_squish(gsub(":", "", business.street1))
    } else {
      business.street1 = NA_character_
    }
    
    ## Street 2
    business.street2 <- qdapRegex::rm_between(business.addr,
                                              "street 2:", "**00**", extract = TRUE)[[1]]
    
    if (!is.na(business.street2)) {
      business.street2 <- str_squish(gsub(":", "", business.street2))
    } else {
      business.street2 = NA_character_
    }
    
    ## CITY
    business.city <- qdapRegex::rm_between(business.addr,
                                           "city:", "**00**", extract = TRUE)[[1]]
    
    if (!is.na(business.city)) {
      business.city <- str_squish(gsub(":", "", business.city))
    } else {
      business.city = NA_character_
    }
    
    ## state
    business.state <- qdapRegex::rm_between(business.addr,
                                            "state:", "**00**", extract = TRUE)[[1]]
    
    if (!is.na(business.state)) {
      business.state <- str_squish(gsub(":", "", business.state))
    } else {
      business.state = NA_character_
    }
    
    ## zip
    business.zip <- qdapRegex::rm_between(business.addr, "zip:",
                                          "**00**", extract = TRUE)[[1]]
    
    if (!is.na(business.zip)) {
      business.zip <- str_squish(gsub(":", "", business.zip))
    } else {
      business.zip = NA_character_
    }
    
    
    ######## MAIL ADDRESS: #########
    mail.addr <- stringi::stri_extract_all_regex(text,
                                                 "(mail address).*?$")[[1]]
    
    mail.addr <- paste0(mail.addr, "**00**")
    
    ## Street 1
    mail.street1 <- qdapRegex::rm_between(mail.addr, "street 1:",
                                          "**00**", extract = TRUE)[[1]]
    
    if (!is.na(mail.street1)) {
      mail.street1 <- str_squish(gsub(":", "", mail.street1))
    } else {
      mail.street1 = NA_character_
    }
    
    ## Street 2
    mail.street2 <- qdapRegex::rm_between(mail.addr, "street 2:",
                                          "**00**", extract = TRUE)[[1]]
    
    if (!is.na(mail.street2)) {
      mail.street2 <- str_squish(gsub(":", "", mail.street2))
    } else {
      mail.street2 = NA_character_
    }
    
    ## CITY
    mail.city <- qdapRegex::rm_between(mail.addr, "city:",
                                       "**00**", extract = TRUE)[[1]]
    
    if (!is.na(mail.city)) {
      mail.city <- str_squish(gsub(":", "", mail.city))
    } else {
      mail.city = NA_character_
    }
    
    ## state
    mail.state <-
      qdapRegex::rm_between(mail.addr, "state:", "**00**",
                            extract = TRUE)[[1]]
    
    if (!is.na(mail.state)) {
      mail.state <- str_squish(gsub(":", "", mail.state))
    } else {
      mail.state = NA_character_
    }
    
    ## zip
    mail.zip <- qdapRegex::rm_between(mail.addr, "zip:", "**00**",
                                      extract = TRUE)[[1]]
    
    if (!is.na(mail.zip)) {
      mail.zip <- str_squish(gsub(":", "", mail.zip))
    } else {
      mail.zip = NA_character_
    }
    
    ######## Former company name: #########
    former.name <-
      qdapRegex::rm_between(text, "former conformed name:",
                            "**00**", extract = TRUE)[[1]]
    
    if (all(!is.na(former.name))) {
      former.name <- toupper(former.name)
    } else {
      former.name = NA_character_
    }
    
    if (length(former.name) > 1) {
      former.name <- list(former.name)
    }
    
    name.change.date <-
      qdapRegex::rm_between(text, "date of name change:",
                            "**00**", extract = TRUE)[[1]]
    
    if (all(!is.na(name.change.date))) {
      name.change.date <- name.change.date
    } else {
      name.change.date = NA
    }
    
    if (length(name.change.date) > 1) {
      name.change.date <- list(name.change.date)
    }
    
    out <- data.frame(
      period.of.report = period.of.report,
      fiscal.yr.end = fiscal.yr.end,
      filer.no = filer.no,
      filer.company.name = company.name,
      filer.cik = cik,
      sic = sic,
      irs = irs,
      state.of.incorp = state.of.incorp,
      business.street1 = business.street1,
      business.street2 = business.street2,
      business.city = business.city,
      business.state = business.state,
      business.zip = business.zip,
      mail.street1 = mail.street1,
      mail.street2 = mail.street2,
      mail.city = mail.city,
      mail.state = mail.state,
      mail.zip = mail.zip,
      former.names = I(former.name),
      name.change.dates = I(name.change.date)
    )
    return(out)
  }

#' Extract State abbreviation from free text
#'
#' This function reads and parses state abbreviation from SEC Edgar
#' filings
#'
#'
#' @param state_text text in which a marker for state (e.g. 'state of incorporation')
#' @param marker is the state marker to detect
#' @return state abbreviation
#' @export
#' @importFrom stringr str_squish str_locate_all str_extract_all str_locate

extractState <- function(state_text, marker) {
  # creating dataset of states and abbreviations to extract
  
  states <- data.frame(state_abb = c(state.abb, "DC", "DC", "PR"),
                       state_name = tolower(
                         c(
                           state.name,
                           "District of Columbia",
                           "Washington D",
                           "Puerto Rico"
                         )
                       ))
  
  state_extract <- tolower(paste(states$state_name, collapse = "|"))
  
  state_text <-
    str_squish(paste(tolower(state_text), collapse = " "))
  
  marker <- tolower(marker)
  
  corporation <- paste("a", tolower(state.name), "corporation")
  corporation <- unname(sapply(corporation, function(str) {
    words <- strsplit(str, " ")[[1]]
    if (length(words) > 1 && startsWith(words[2], "a"))
      words[1] <- sub("\\ba\\b", "an", words[1])
    paste(words, collapse = " ")
  }))
  
  marker <- c(marker, corporation)
  
  # finding state marker location
  
  marker_locations <-
    sapply(marker, function(m)
      mean(unlist(str_locate(state_text, m))))
  state <- NULL
  
  if (any(!is.na(marker_locations))) {
    # extracting state closest to marker
    closest_distance <- Inf
    closest_state_index <- NA
    
    # loop over markers and extract the closest distance
    for (i in 1:length(marker_locations)) {
      marker_distance <-
        abs(rowMeans(str_locate_all(state_text, state_extract)[[1]]) - marker_locations[i])
      min_distance <- min(marker_distance, na.rm = TRUE)
      
      # only consider distances that are smaller than already defined distances
      # and that are closer than 100 characters from the marker
      if (!is.na(min_distance) &&
          min_distance < closest_distance && min_distance <= 100) {
        closest_distance <- min_distance
        closest_state_index <- which.min(marker_distance)
      }
    }
    
    if (!is.na(closest_state_index)) {
      state <-
        unlist(str_extract_all(state_text, state_extract))[closest_state_index]
    }
    
    state <- ifelse(state %in% states$state_name, state, NA)
  }
  
  # if state is found, return abbreviation
  
  if (length(state) != 0) {
    state_abb <- states$state_abb[states$state_name == state]
    
  } else{
    state_abb <- NA_character_
  }
  
  return(as.character(state_abb))
  
}
