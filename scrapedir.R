library(rvest)
library(data.table)
# library(stringr)

baseurl <- "http://tel.directory.gov.hk/"
suffixurl <- "?accept_disclaimer=yes"

hasphonetable <- function(urlhtml) {
    return(length(html_nodes(urlhtml, ".row td")) > 0)
}

#teldir <- data.table(fullname = "Herg Gerh", posttitle = "nyaaaah", telno = "2859 4852", email = "a@a.com", department = "The poalis")[0]

textorbr <- function(htmlnode) {
    converttotext <- function(htmlchild) {
        if (xml_tag(htmlchild) == "br") {return("<br/>")} else {return(html_text(htmlchild))}
    }
    convertedtext <- lapply(html_children(htmlnode), converttotext)
    return(paste0(convertedtext, collapse=""))
}

teldirlist <- list()
scrape <- function(url, teldirlist) {
    urlhtml <- html(url)
    if (hasphonetable(urlhtml)) {
        department <- textorbr(html_node(urlhtml, "h1"))
        phonetable <- sapply(html_nodes(urlhtml, ".row td"), textorbr)
        phonetable <- data.table(matrix(phonetable, ncol=4, byrow=TRUE))
        setnames(phonetable, c("fullname", "title", "telno", "email"))
        phonetable[,department := department]
        teldirlist[[department]] <- phonetable
    }
    somelinks <- html_attr(html_nodes(urlhtml, "#tbl_dept_list a"), "href")
    morelinks <- html_attr(html_nodes(urlhtml, "#dept_list_lv2_outline a"), "href")
    links <- c(somelinks, morelinks)
    for (link in links) {
        print(link)
        teldirlist <- scrape(paste0(baseurl, link, suffixurl), teldirlist)
    }
    return(teldirlist)
}

                                        # initialurl <- "http://tel.directory.gov.hk/index_ENG.html?accept_disclaimer=yes"
initialurl <- "http://tel.directory.gov.hk/index_AFCD_ENG.html" 
teldirlist <- scrape(initialurl, teldirlist)
teldir <- rbindlist(teldirlist)
