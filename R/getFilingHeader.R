#' Scrape EDGAR filing header information
#'
#' \code{getFilingHeader} Extract EDGAR filing header information
#'
#' getFilingHeader function takes CIK(s), form type(s), and year(s) as input parameters.  
#' The function first imports available downloaded filings in local woking directory 
#' 'Edgar filings_full text' created by \link[edgar]{getFilings} function; 
#' otherwise, it automatically downloads the filings which are not already been 
#' downloaded. It then parses all the important header information from filings.
#' The function returns a dataframe with filing and header information. According 
#' to SEC EDGAR's guidelines a user also needs to declare user agent.
#' 
#' @usage getFilingHeader(cik.no, form.type, filing.year, useragent)
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
#' 
#' header.df <- getFilingHeader(cik.no = c('1000180', '38079'), 
#'                          form.type = '10-K', filing.year = 2006, useragent) 
#'               
#' header.df <- getFilingHeader(cik.no = '38079', c('10-K', '10-Q'), 
#'                          filing.year = c(2005, 2006), useragent)
#'}

getFilingHeader <- function(cik.no, form.type, filing.year, useragent="") {
    
    
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
    
    output <- getFilings(cik.no, form.type, filing.year, 
                         quarter = c(1, 2, 3, 4), downl.permit = "y", useragent)
    
    if (is.null(output)) {
        # cat('Please check the CIK number.')
        return()
    }
    
    cat("Scraping filing header information ...\n")
    
    progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
    
    output$quarter <- NULL
    output$filing.year <- NULL
    output$status <- NULL
     
    ####################### Define useful subfunctions  ###############

    GetConfirmedPeriodOfReport <- function(header) {
        period.of.report <- NA
        
        period.of.report = ""
        if (any(grepl("CONFORMED PERIOD OF REPORT", header, ignore.case = T))) {
            
            period.of.report <- header[grep("CONFORMED PERIOD OF REPORT", 
                                            header, ignore.case = T)]
            
            period.of.report <- gsub("CONFORMED PERIOD OF REPORT:|\t|\\s", 
                                     "", period.of.report, ignore.case = T)
            
            if (length(period.of.report) == 0) {
                period.of.report <- ""
            }
            
            if (length(period.of.report) > 1) {
                period.of.report <- paste(period.of.report, collapse = "_and_")
            }
        }
        period.of.report <- period.of.report
        return(period.of.report)
    }
    
    
    FilingHeaderSubFunc <- function(text, filer.no) {
        
        ## Capture CONFORMED COMPANY NAME
        company.name <- qdapRegex::rm_between(text, "company conformed name", 
                                              "**00**", extract = TRUE)[[1]]
        
        if (!is.na(company.name)) {
            company.name <- gsub(":", "", company.name)
        } else {
            company.name = ""
        }
        
        ## Capture cik 
        cik <- qdapRegex::rm_between(text, "central index key", 
                                     "**00**", extract = TRUE)[[1]]
        
        if (!is.na(cik)) {
            cik <- gsub(":", "", cik)
        } else {
            cik = ""
        }
        
        ## Capture SIC code
        sic <- qdapRegex::rm_between(text, "standard industrial classification",
                                     "**00**", extract = TRUE)[[1]]
        
        if (!is.na(sic)) {
            sic <- gsub(":", "", sic)
        } else {
            sic = ""
        }
        
        ## Capture IRS number
        irs <- qdapRegex::rm_between(text, "irs number", "**00**", 
                                     extract = TRUE)[[1]]
        
        if (!is.na(irs)) {
            irs <- gsub(":", "", irs)
        } else {
            irs = ""
        }
        
        ## Capture state of incorporation
        state.of.incorp <- qdapRegex::rm_between(text, "state of incorporation",
                                                 "**00**", extract = TRUE)[[1]]
        
        if (!is.na(state.of.incorp)) {
            state.of.incorp <- gsub(":", "", state.of.incorp)
        } else {
            state.of.incorp = ""
        }
        
        ## Capture fiscal year end
        fiscal.yr.end <- qdapRegex::rm_between(text, "fiscal year end",
                                               "**00**", extract = TRUE)[[1]]
        
        if (!is.na(fiscal.yr.end)) {
            fiscal.yr.end <- gsub(":", "", fiscal.yr.end)
        } else {
            fiscal.yr.end = ""
        }
        
        ######## BUSINESS ADDRESS: ########
        business.addr <- qdapRegex::rm_between(text, "business address", 
                                               "mail address", extract = TRUE)[[1]]
        
        if (is.na(business.addr)) {
            business.addr <- stringi::stri_extract_all_regex(text, 
                                                             "(business address).*?$")[[1]]
        }
        
        
        ## Street 1
        business.street1 <- qdapRegex::rm_between(business.addr, 
                                                  "street 1:", "**00**", extract = TRUE)[[1]]
        
        if (!is.na(business.street1)) {
            business.street1 <- gsub(":", "", business.street1)
        } else {
            business.street1 = ""
        }
        
        ## Street 2
        business.street2 <- qdapRegex::rm_between(business.addr, 
                                                  "street 2:", "**00**", extract = TRUE)[[1]]
        
        if (!is.na(business.street2)) {
            business.street2 <- gsub(":", "", business.street2)
        } else {
            business.street2 = ""
        }
        
        ## CITY
        business.city <- qdapRegex::rm_between(business.addr, 
                                               "city:", "**00**", extract = TRUE)[[1]]
        
        if (!is.na(business.city)) {
            business.city <- gsub(":", "", business.city)
        } else {
            business.city = ""
        }
        
        ## state
        business.state <- qdapRegex::rm_between(business.addr, 
                                                "state:", "**00**", extract = TRUE)[[1]]
        
        if (!is.na(business.state)) {
            business.state <- gsub(":", "", business.state)
        } else {
            business.state = ""
        }
        
        ## zip
        business.zip <- qdapRegex::rm_between(business.addr, "zip", 
                                              "**00**", extract = TRUE)[[1]]
        
        if (!is.na(business.zip)) {
            business.zip <- gsub(":", "", business.zip)
        } else {
            business.zip = ""
        }
        
        
        ######## MAIL ADDRESS: #########
        mail.addr <- stringi::stri_extract_all_regex(text, 
                                                     "(mail address).*?$")[[1]]
        
        mail.addr <- paste0(mail.addr, "**00**")
        
        ## Street 1
        mail.street1 <- qdapRegex::rm_between(mail.addr, "street 1:", 
                                              "**00**", extract = TRUE)[[1]]
        
        if (!is.na(mail.street1)) {
            mail.street1 <- gsub(":", "", mail.street1)
        } else {
            mail.street1 = ""
        }
        
        ## Street 2
        mail.street2 <- qdapRegex::rm_between(mail.addr, "street 2:",
                                              "**00**", extract = TRUE)[[1]]
        
        if (!is.na(mail.street2)) {
            mail.street2 <- gsub(":", "", mail.street2)
        } else {
            mail.street2 = ""
        }
        
        ## CITY
        mail.city <- qdapRegex::rm_between(mail.addr, "city:", 
                                           "**00**", extract = TRUE)[[1]]
        
        if (!is.na(mail.city)) {
            mail.city <- gsub(":", "", mail.city)
        } else {
            mail.city = ""
        }
        
        ## state
        mail.state <- qdapRegex::rm_between(mail.addr, "state:", "**00**", 
                                            extract = TRUE)[[1]]
        
        if (!is.na(mail.state)) {
            mail.state <- gsub(":", "", mail.state)
        } else {
            mail.state = ""
        }
        
        ## zip
        mail.zip <- qdapRegex::rm_between(mail.addr, "zip", "**00**", 
                                          extract = TRUE)[[1]]
        
        if (!is.na(mail.zip)) {
            mail.zip <- gsub(":", "", mail.zip)
        } else {
            mail.zip = ""
        }
        
        out <- data.frame(period.of.report = period.of.report, 
                          fiscal.yr.end = fiscal.yr.end, 
                          filer.no = filer.no, 
                          filer.company.name = company.name, 
                          filer.cik = cik, sic = sic, irs = irs, 
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
                          mail.zip = mail.zip)
        return(out)
    }
    
    
    ## Start scraping filing one by one and storing header information in main dataframe
    main.output <- data.frame()
    
    for (i in 1:nrow(output)) {
        
        f.type <- gsub("/", "", output$form.type[i])
        
        dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                                "/", output$cik[i], "/", output$cik[i], "_", 
                                f.type, "_", output$date.filed[i], 
                                "_", output$accession.number[i], ".txt")
        
        # Read filing
        filing.text <- readLines(dest.filename)
        
        main.df <- output[i, ]
        
        secheader_line <- grep("</SEC-HEADER>", filing.text, ignore.case = T)
        
        if (length(secheader_line) == 0) {
          
            secheader_line <- grep("<DOCUMENT>", filing.text, ignore.case = T)
            
            if (length(secheader_line) != 0) {
                secheader_line = secheader_line[1]
            } else {
                secheader_line = 0
            }
        }
        
        if (length(secheader_line) != 0 & secheader_line > 18) {
            
            filing.text <- filing.text[1:(secheader_line - 1)]
            
            filing.text <- gsub("</FILER>", "", filing.text)
            
            period.of.report <- GetConfirmedPeriodOfReport(filing.text)
            
            filing.text <- gsub("\\t|\\n|\\s", " ", filing.text)
            filing.text <- gsub("\\s{2,}", " ", filing.text)
            
            
            filing.text <- filing.text[grep("FILER", filing.text, 
                                            ignore.case = T)[1]:length(filing.text)]
            
            
            filing.text1 <- paste(filing.text, collapse = "**00**")
            filing.text1 <- tolower(filing.text1)
            filing.text2 <- strsplit(filing.text1, "filer")
            filing.text2 <- unlist(filing.text2)
            filing.text2 <- filing.text2[-1]
            
            
            for (filer.no in 1:length(filing.text2)) {
              
                text <- filing.text2[filer.no]
                header.df <- FilingHeaderSubFunc(text, filer.no)
                combined.df <- cbind(main.df, header.df)
                
            }
            
        } else {
          
            header.df <- FilingHeaderSubFunc("", filer.no = 1)
            combined.df <- cbind(main.df, header.df)
            
        }
        
        main.output <- rbind(main.output, combined.df)
        
        # update progress bar
        setTxtProgressBar(progress.bar, i)
        
    }
    
    ## Make some columns uppercase
    vars <- c("filer.company.name", "sic", "state.of.incorp", 
              "business.street1", "business.street2", "business.city", 
              "business.state", "mail.street1", "mail.street2", 
              "mail.city", "mail.state")
    
    main.output[vars] <- lapply(main.output[vars], toupper)
    
    ## Clean SIC CODE 
    main.output$sic <- gsub("\\", "", main.output$sic, fixed = TRUE)
    main.output$sic <- gsub("'|/|,", "", main.output$sic)
    main.output$sic <- regmatches(main.output$sic, 
                                  gregexpr("[[:digit:]]+", main.output$sic))[[1]]

    ## convert dates into R datee
    main.output$date.filed <- as.Date(as.character(main.output$date.filed), "%Y-%m-%d")
    main.output$period.of.report <- as.Date(as.character(main.output$period.of.report), 
                                            "%Y%m%d")
    main.output$filer.cik <- as.numeric(main.output$filer.cik)
    
    # Close progress bar
    close(progress.bar)
    
    return(main.output)
}
