################################################
#####   USING LISTS for TEXT PROCESSING    #####
################################################

## Lists can combine elements of different modes.

## Text Concordance

# Pertains to web search and text mining
# Write findwords() which determines which words are
# in a text file and locations

# Input file is testconcord.txt:
#----------------------------------------------------------------------
# The [1] here means that the first item in this line of output is
# item 1. In this case, our output consists of only one line (and one
# item), so this is redundant, but this notation helps to read
# voluminous output that consists of many items spread over many
# lines. For example, if there were two rows of output with six items
# per row, the second row would be labeled [7].
#----------------------------------------------------------------------

# To find words, we replace all nonletter characters with blanks
# and get rid of punctuation and capitalization. We are not 
# using string functions for this purpose

# The new file is testconcorda.txt:
#----------------------------------------------------------------------
# the    here means that the first item in this line of output is
# item   in this case  our output consists of only one line  and one
# item  so this is redundant  but this notation helps to read
# voluminous output that consists of many items spread over many
# lines  for example  if there were two rows of output with six items
# per row  the second row would be labeled
#----------------------------------------------------------------------

# word 'item' occupies the 7th, 14th, and 27th
# word positions in the file

## Program findwords
findwords <- function(tf) {
  # read in the words from the file, 
  # into a vector of mode character
  # txt is vector of string variables
  txt <- scan(tf,"")
  # initialize local variable wl
  wl <- list() 
  for (i in 1:length(txt)) {
    wrd <- txt[i]  # i-th word in input file
    # when i=4, wrd="that"; wl[["that"]] does not exist yet
    # so wl[["that"]]=NULL so can concatenate it. 
    # wl[["that"]] becomes one element vector (4). When
    # i=40, wl[["that"]] will become (4,40)
    wl[[wrd]] <- c(wl[[wrd]],i)
  } 
  return(wl)
}

# We call it:
# input "testconcorda.txt"
j <- findwords(file.choose())

# returned list has one component per word
# with word's components showing positions
# within file where that word occurs

# list is most appropriate structure for this
# explain what is going on in the for loop

# Accessing List Components and Values
# If list components do have tags, you
# can obtain them with names()
names(j)

# unlist() turns it into a vector
# in this case a vector of character strings
# so you can retrieve the values
ulj <- unlist(j);ulj
class(ulj)

# But if we start with numbers
# we end up with numbers

z <- list(a=5,b=12,c=13);z
y <- unlist(z);y
class(y)

# Mixed case
w <- list(a=5,b="xyz");w
wu <- unlist(w)
class(wu)
wu

# R chooses the lowest common denominator:
# character strings....there is a precedence
# structure with unlist: "Vectors are coerced 
# to the highest type of components in the
# hierarchy NULL < raw< logical < integer
# < real < complex < character < list <
# expression: pairlists are treated as lists.

# However, note that R did give each element of 
# vector wu a name, we can remove by setting=NULL

names(wu) <- NULL
wu

# or can remove directly with unname()

wu <- unlist(w)
wu
wun <- unname(wu)
wun

### Applying functions to list
# Function lapply() works like apply() but on lists
# calling specified function of each component of
# list (or vector coerced to a list) and returns
# another list.

lapply(list(1:3,25:29),median)

# Sometimes list returned by lapply() could be
# simplified to a vector or matrix...this is 
# what sapply() does (simplified [l]apply)

sapply(list(1:3,25:29),median)

#### EXTENDED EXAMPLE: Text Concordance, Continued
j

# Would be nice to sort the returned list

## Program alphawl
# sorts wrdlst, the output of findwords() 
# alphabetically by word
alphawl <- function(wrdlst) {
  # words are names of components, can extract:
  nms <- names(wrdlst) # the words
  # sort the words
  sn <- sort(nms)  # same words in alpha order
  # return rearranged version  
  # Note use of single brackets, not double
  # because are not subsetting the list
  return(wrdlst[sn])  
}

# Try it:
alphawl(j)

# Works fine

# We can sort by word frequency similarly:
## Program freqwl
# orders the output of 
# findwords() by word frequency
freqwl <- function(wrdlst) {
  # sapply will return a vector of word frequencies
  freqs <- sapply(wrdlst,length)  
  # order() more direct than sort()
  # order() returns indices of a sorted vector
  return(wrdlst[order(freqs)])
}

# example of what order() does, very handy
x <- c(12,5,13,8)
order(x)

# We try it. Output indicates x[2] is smallest 
# element in x, x[4] is second smallest, etc
freqwl(j)

# We can also plot most important words

# input "nyt.txt"
nyt.txt <- findwords(tolower(file.choose()))

# look at it
nyt.txt

## Program nytplot
ssnyt <- freqwl(nyt.txt);ssnyt
nwords <- length(ssnyt);nwords
freqs9 <- sapply(ssnyt[round(0.9*nwords):nwords],length)
barplot(freqs9, main="FREQUENCY  OF  NEW  YORK  TIMES  WORDS  ABOUT  R")
