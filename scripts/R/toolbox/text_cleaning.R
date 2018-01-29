library(tm)
library(stringr)

improve_text <- function(text) {
  # Amélioration finale du texte avec :
  # - retrait des accents
  # - passage en majuscule
  # - retrait des mots usuels en anglais et français (of, to, de, ...)
  # - retrait des url
  # - retrait des chiffres
  # prend en entrée une chaine de caractère ou un vecteur de caractère
  library(tm)
  # retrait des accents
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  # Passage en majuscule
  text <- toupper(text)
  # retrait ponctuation
  text = gsub("[[:punct:]]", "", text)
  # retrait chiffres
  text = gsub("[[:digit:]]", "", text)
  # remove links
  text = gsub("http\\w+", "", text)
  # retrait des mots 'usuels'
  text <- removeWords(text,stopwords("english"))
  text <- removeWords(text,stopwords("french"))
  return(text)
}


try_to_lower <- function(x) {
  # Passage en miniscule du texte quand la fonction tolower
  # renvoie une erreur.

  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

unaccent <- function(text) {
  # Fonction pour retirer les accents
  # Prend en entrée une chaine de caractère
  # ou un vecteur de caractère
  # retourne la même chaine de caractère sans accent
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}


# fonction qui renomme les colonnes d'un dataframe selon la nomenclature qu'on s'est plus ou moins fixée
clean_colnames <- function(data){
  # lower case
  d <- tolower(colnames(data))
  # TODO : certains acronymes en majuscules ?

  # clean special character
  d <- gsub("+","_plus_",d,fixed=T) # + converti en _p_ ?
  d <- gsub("[\r\n_ \\-]+","_",d)
  # clean french accents
  d <- unaccent(d)

  # on garde que les chiffres les lettres et _
  # tout le reste à vide
  d <- gsub("[^0-9a-zA-Z_]+","",d)

  # on écrit num et nb de la même façon partout
  d <- gsub("^(nb|nbr|nbre|nombre)_","nb_",d)
  d <- gsub("^n(um|umero)?_","num_",d)

  # remove leading numbers
  d <- gsub('^([0-9].*)','_\\1',d)

  colnames(data) <- d

  return(data)
}


clean_text_cols <- function(data)
# fonction qui enlève tous les caractères qui posent problème à l'intérieur des colonnes
# permet de faire des csv propres
{

  clean_text <- function(col){
    # on remplace les retours à la ligne et les ; par des espaces
    return(gsub("[\r\n;]+"," ",col))
  }

  data[,colnames(Filter(is.character,data))] <- apply(Filter(is.character,data), 2, clean_text)

  return(data)
}
