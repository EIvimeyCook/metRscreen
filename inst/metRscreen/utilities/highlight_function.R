#' @title highlight_text
#' @description Highlights text different colours
#' @param text text for searching
#' @param search search term

highlight_text<- function(text,search){
  
  colours <- c("#52D017","#DC143C","#AF33FF","#FFB133","#334DFF")
  
  for(j in 1:length(search)){
    if(nchar(search[[j]]) !=0){
      split_search <- strsplit(as.character(search),", ")[[j]]
      split_search <- gsub("\\*","[a-zA-Z]+",split_search)
      
      for(i in 1:length(split_search)){
        search_term <- paste0("(^|\\W)(",split_search[i],")(\\W|$)")
        text <- gsub(search_term, paste0("\\1<font color=\"",colours[j],"\"><b>\\2</b></font>\\3"),text, ignore.case = T)
      }
    }    
  }
  return(text)
}
