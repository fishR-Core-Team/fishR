knitr::opts_chunk$set(
  echo=FALSE,
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)

## Helper Functions
prepare_db <- function(pkg) {
  tmp <- hsearch_db(pkg)
  tmpB <- tmp$Base %>%
    select(ID,Topic,Title,Package)
  tmpK <- tmp$Keywords %>%
    filter(Keyword=="datasets") %>%
    select(-Package)
  tmpC <- tmp$Concepts %>%
    filter(Concept!="Datasets available by data()") %>%
    select(-Package)
  tmp <- right_join(tmpB,tmpK,by="ID") %>%
    left_join(tmpC,by="ID",multiple="all") %>%
    select(Dataset=Topic,Description=Title,Concept,Package)
  tmp
}

makelink <- function(d,showPkg=TRUE) {
  base <- "https://fishr-core-team.github.io/"
  tmp <- case_when(
    d$Package=="FSAdata" ~ paste0("<a href='",base,"FSAdata/reference/",
                                  d$Dataset,".html'>",d$Dataset,"</a>"),
    d$Package=="FSA" ~ paste0("<a href='",base,"FSA/reference/",
                              d$Dataset,".html'>",d$Dataset,"</a>"),
    TRUE ~ d$Dataset)
  if (showPkg) tmp <- paste0(d$Package,"::",tmp)
  tmp
}

maketable <- function(db,tops=NULL,pkgs=NULL,showPkg=TRUE) {
  if (!is.null(tops) & !is.null(pkgs)) stop("Cannot use both 'tops' and 'pkgs'.")
  if (!is.null(pkgs)) { ## by package
    pkgs <- sort(pkgs)
    for (i in seq_along(pkgs)) {
      if (i>1) cat("&nbsp;\n\n")
      cat("#",pkgs[i])
      tmp <- db %>%
        filter(Package==pkgs[i]) %>%
        filter(!duplicated(Dataset)) %>%
        arrange(Dataset) %>%
        mutate(Dataset=makelink(.,showPkg)) %>%
        select(Dataset,Description) %>%
        knitr::kable()
      print(tmp)
      cat("\n")
    }
  } else if (!is.null(tops)) { ## by topic
    for (i in seq_along(tops)) {
      if (i>1) cat("&nbsp;\n\n")
      cat("#",tops[i])
      tmp <- db %>%
        filter(Concept==tops[i]) %>%
        filter(!duplicated(Dataset)) %>%
        arrange(Dataset,Package) %>%
        mutate(Dataset=makelink(.,showPkg)) %>%
        select(Dataset,Description) %>%
        knitr::kable()
      print(tmp)
      cat("\n")
    }
  } else{ ## by neither package nor topic
    tmp <- db %>%
      filter(!duplicated(Dataset)) %>%
      arrange(Dataset,Package) %>%
      mutate(Dataset=makelink(.,showPkg)) %>%
      select(Dataset,Description) %>%
      knitr::kable()
    print(tmp)
    cat("\n")
  }
}

# List of topics (these should be in the @Concepts of the dataset documentation)
topics <- c("Length Expansion","Length Conversion",
            "Age Comparison","Age-Length Key","Back-Calculation",
            "Length Frequency","Size Structure","Weight-Length",
            "Depletion","Removal","Capture-Recapture",
            "Mortality","Growth","Recruitment","Maturity",
            "Data Manipulation","Other")
