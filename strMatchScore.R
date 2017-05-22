# sample Call of the function to apply to a df field, adding a new field:
#   df$newPrimary = sapply(df$textField, strMatchScore ,  listofLists , 1)
#   df$newSecondary = sapply(df$textField, strMatchScore ,  listofLists , 2)


strMatchScore <- function(textToMatch, listOfLists, output=1) {
  #output: 1 will return first highest-scoring match (default), 2 returns remaining matches with max score beyond the 1st
  #remove puncutation, extra spaces
  cleantxt <- gsub( '[[:punct:]]', " ", textToMatch ) #remove punctuation
  cleantxt <- gsub( "\\s+", " ", cleantxt ) #remove extra spaces
  
  mlen <- length (unlist (strsplit (cleantxt, " ") )) #will throw error if factor
  z <- matrix(nrow=0,ncol=mlen)
  
  for (y in listOfLists) { x = matrix( as.numeric( as.list( unlist( strsplit (toupper( cleantxt), " "))) %in% y ), ncol = mlen)
                           z <- rbind(z, x) }
  #z is a matrix with 1's and 0's.
  #  Each col is a word in the sentence/text you are matching against categories
  #  Each row is a category that each has its own list of keywords
  if ( sum(z) > 0) {  #if there are any matching terms
    weight <- apply( z, 2, sum) #weight by num of times word matched a category (more categories = less unique/important word)
    z.wt <- z %*% diag( 1/weight , nrow=length(weight)) #create weighted matrix (word matches divided by the number of categories it matched)
    z.score <- apply( z.wt, 1, sum, na.rm=T) #vector of numbers with category scores
    # to get as a matrix, can use t(t(z.score))  [the first t() creates a matrix, the second transposes to have each cat as a row like original data]
    z.maxindex <- which( z.score == max(z.score) ) #returns index position of max score item(s)
    z.names <- names( listOfLists[z.maxindex] ) #convert index to string label
    if ( length (z.names ) > 1) { #if tied match scores
      primary <- z.names[1]
      secondary <- paste( z.names[2:length(z.names)] , collapse = "; " )
    }
    else { #if only 1 top score
      primary <- z.names
      secondary <- ""
    }
  }
  else { #if no matching terms
    primary <- ""
    secondary <- ""
  }
  
  #function var3 - output.  1=primary, 2=secondary
  if ( toupper( output) == 1) {
    return(primary)
  }
  else if (toupper ( output) == 2) {
    return(secondary) 
  }
   
}