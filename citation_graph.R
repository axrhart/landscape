# Install/load packages
if(!require(pacman)) install.packages("pacman"); library(pacman)
pacman::p_load(bibliometrix, future.apply, rlist, tidyverse)

# Custom functions
# by Ryan C. Thompson https://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
quotemeta <- function(string) {
    str_replace_all(string, "(\\W)", "\\\\\\1")
}

# Load raw data 
files <- c("raw data/savedrecs(1).bib",
           "raw data/savedrecs(2).bib",
           "raw data/savedrecs(3).bib",
           "raw data/savedrecs(4).bib",
           "raw data/savedrecs(5).bib")

raw_data <- convert2df(files, dbsource = "wos", format = "bibtex")

# Reduce data.frame to required information
raw_data %>%
    rownames_to_column() %>%
    transmute(doi = DI, wosid = UT, shortname = rowname, citation = CR) -> df.selected

# Expand citations
lapply(1:nrow(df.selected), function(row) {
    
    # Split citations at ;
    cttns <- str_split(df.selected[row, "citation"], "; ", simplify = T) %>%
        as.vector()
    
    # Return a df per row with a new row for each citation
    return(data.frame(doi = rep(df.selected[row, "doi"], length(cttns)),
                      wosid = rep(df.selected[row, "wosid"], length(cttns)),
                      source = rep(df.selected[row, "shortname"], length(cttns)),
                      citation = cttns,
                      stringsAsFactors = F))
    
}) %>%
    # Combine data.frames
    rlist::list.rbind() %>%
    
    # Remove NA citations
    filter(!is.na(citation)) -> df.expanded

# Match sourcename
future_lapply(1:nrow(df.expanded), function(row) {
    
    cat(round(row/nrow(df.expanded), 4)*100, "%\n")
    
    # Create temporary dictionary
    dict <- raw_data %>%
        rownames_to_column() %>%
        transmute(doi = DI, shortname = rowname) %>%
        rowwise() %>%
        
        # Replace doi NA with "" to avoid grepl errors
        mutate(doi = if_else(is.na(doi), "", doi)) %>%
        ungroup()
    
    # Get citation string
    target <- quotemeta(df.expanded[row, "citation"])
    
    if (sum(grepl(target, dict$doi)) > 0) {
        
        matches <- dict$shortname[grepl(target, dict$doi)]
        
        return(data.frame(doi = rep(df.expanded[row, "doi"], length(matches)),
                          wosid = rep(df.expanded[row, "wosid"], length(matches)),
                          source = rep(df.expanded[row, "source"], length(matches)),
                          citation = rep(df.expanded[row, "citation"], length(matches)),
                          target = matches,
                          likely = rep(NA, length(matches)),
                          stringsAsFactors = F))
        
    } else if (sum(grepl(target, dict$shortname)) > 0) {
        
        matches <- dict$shortname[grepl(target, dict$shortname)]
        
        return(data.frame(doi = rep(df.expanded[row, "doi"], length(matches)),
                          wosid = rep(df.expanded[row, "wosid"], length(matches)),
                          source = rep(df.expanded[row, "source"], length(matches)),
                          citation = rep(df.expanded[row, "citation"], length(matches)),
                          target = matches,
                          likely = rep(NA, length(matches)),
                          stringsAsFactors = F))   
        
    } else {
        
        distances <-  adist(dict$shortname, target) %>% 
            as.vector()
        
        min_dist <- min(distances, na.rm = T)
        
        matches <- dict$shortname[distances == min_dist]
        
        return(data.frame(doi = rep(df.expanded[row, "doi"], length(matches)),
                          wosid = rep(df.expanded[row, "wosid"], length(matches)),
                          source = rep(df.expanded[row, "source"], length(matches)),
                          citation = rep(df.expanded[row, "citation"], length(matches)),
                          target = matches,
                          likely = rep(1 - min_dist/max(distances, na.rm = T), length(matches)),
                          stringsAsFactors = F))  
    }
    

}, future.seed = 1337) -> x
