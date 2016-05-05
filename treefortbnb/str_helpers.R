library(reshape2)

removeNotes <- function(x){
  gsub( " *\\[.*?\\] *", "", x)
}

removeAccents <- function(string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

trim_spaces <- function (x){
  x <- gsub("\\s+", " ", x)
  gsub("^\\s+|\\s+$", "", x)
}

# dictionaryMatch <- function(inputStr,dict, empty=c("")){
#   dict <- unique(dict)
#   l <- lapply(inputStr, function(inputStr){
#     if(inputStr %in% empty){return(empty[1])}
#     inputStr <- tolower(inputStr)
#     inputStr <- removeAccents(inputStr)
#     dict_tmp <- tolower(dict)
#     dict_tmp <- removeAccents(dict_tmp)
#     tmp <- adist(inputStr, dict_tmp)
#     tmp <- as.vector(tmp)
#     dict[which.min(tmp)]
#   })
#   unlist(l)
# }


dictionaryMatch <- function(inputStr,dict, empty=c(""),maxDistance = 10){
  dict[is.na(dict)] <- ""
  l <- strsplit(paste(dict$name,dict$alternativeNames,sep="|"),"|",fixed = TRUE)
  names(l) <- paste0(seq_along(l),"_")
  nm <- unlist(l)
  id <- gsub("_[1-9]*","",names(nm))
  l <- data.frame(id = id,name = nm, stringsAsFactors = FALSE)
  l$id <- as.numeric(l$id)
  ids <- lapply(inputStr, function(str){
    #str <- "North Miami Beac - FL"
    #str <- "Portland - OR"
    if(str %in% empty){return(empty[1])}
    str <- tolower(str)
    str <- removeAccents(str)
    dict_tmp <- tolower(l$name)
    dict_tmp <- removeAccents(dict_tmp)
    tmp <- adist(str, dict_tmp)
    tmp <- as.vector(tmp)
    if(min(tmp) > maxDistance){
      out <- NA
    }else{
      index <- l$id[which.min(tmp)]
      #index <- which.min(tmp)
      #l$name[index]
      out <- as.character(dict$name[index])
    }
    out
  })
  ids <- unlist(ids)
  #dict$name[ids]
  ids
}

# idx <- match(x,codes$name)
# codes$id[idx]




