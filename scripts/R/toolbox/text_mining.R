
library(tm)

# retourne les n mots les plus présents dans une colonne texte
top_words_in_col <- function(char_col,n=20)
{
  f_mx <- DocumentTermMatrix(Corpus(VectorSource(char_col)),control = list(removePunctuation=TRUE))
  f_sums <- colSums(as.matrix(f_mx))
  return(head(-sort(-f_sums),n))
}


add_words_in_col <- function(data,column,word_list,prefix="")
{
  column<-eval(substitute(column),data, parent.frame())
  f_mx <- DocumentTermMatrix(Corpus(VectorSource(column)),control = list(removePunctuation=TRUE))
  w_mx <- f_mx[,word_list] %>% as.matrix()
  w_mx <- as.data.frame(w_mx>0)
  colnames(w_mx) <- paste0(prefix,colnames(w_mx))
  return(cbind(data,w_mx))
}

# cherche dans une colonne de text des nombres avec un suffix ou pas 
# et une fonction d'aggregation si un nombre apparait plsieurs fois
#
# ex : si dans une colonne text on a :
# "ballons élec école de 50 L à 100 L cuisine 500 L logt de 75 à 200 L au total 13 ballons"
# get_number_in_text(df$col,'L',max) -> 500
# get_number_in_text(df$col,'L',sum) -> 850
get_number_in_text <- function(text_col,suffix='',sumarise_fct=sum){
  matches = str_match_all(text_col,paste0('([0-9. ]+) ?',suffix))
  matches <- lapply(matches,function(d){
    if(nrow(d)==0)
      return(NA)
    return(as.numeric(d[,2]))
  })  
  return(as.numeric(lapply(matches,sumarise_fct)))
}
