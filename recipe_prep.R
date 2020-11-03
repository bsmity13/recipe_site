#######################################X
#----Brian and Simona's Recipe Book----X
#------------R Markdown PDF------------X
#-------------Recipe Prep--------------X
#######################################X

#Load packages
library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)

#Set options
options(stringsAsFactors = FALSE)

#Chapters
chs <- c("breakfast", "lunch", "cocktail", "appetizer",
         "first", "main", "dessert")

#lapply to each chapter to process recipes
l <- lapply(chs, function(x) {
  #Get recipe paths
  rp <- list.dirs(paste0("recipes/", x), recursive = FALSE, full.names = TRUE)

  #lapply to each name to get recipe components
  r <- lapply(rp, function(x){
    #Return a list
    y <- list()
    
    #Read in ingredients
    ingred.file <- list.files(x, pattern = "ingred", full.names = TRUE)
    y$ingred <- read.csv(ingred.file) %>%
      mutate(ingredient = str_to_title(ingredient))
    #Capitalize column names
    names(y$ingred) <- str_to_title(names(y$ingred))
    
    #Read in steps
    steps.file <- list.files(x, pattern = "steps", full.names = TRUE)
    y$steps <- read.csv(steps.file)
    
    #Create steps string
    out <- character()
    for (j in 1:nrow(y$steps)){
      step <- y$steps[j,]
      text <- paste0(
        step$step,
        ifelse(is.na(step$substep), "", step$substep),
        ". ",
        step$text,
        "\n  \n  "
      )
      out <- paste0(out, text)
      #Replace "degrees" with "°"
      out <- gsub("degrees", "\u00B0", out)
      Encoding(out) <- "UTF-8"
      #Insert tilde in jalapeno
      out <- gsub("jalapeno", "jalape\u00F1o", out)
    }
    y$step.str <- out
    
    #Read image paths
    y$img <- list.files(x, pattern = "jpg", full.names = TRUE)
    
    #Read nice name
    path <- file.path(x, "nice.txt")
    y$nice <- read.csv(path, header = FALSE)$V1[1]
    
    #Return
    return(y)
  })

  #Return list
  return(r)
})

#Give main list elements chapter names
names(l) <- chs
