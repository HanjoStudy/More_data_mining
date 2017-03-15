##############################################
##### CHARACTER AND STRING MANIPULATION  #####
##############################################

# R contains a full complement of functions 
# which can manipulate character data.

# R is a statistical language but character
# strings are surprisingly important as well. 
# Ranging from birth dates stored in medical 
# research data files to textmining apps,
# character data is quite prominent in R 
# programs. Accordingly, R has a number of
# string-manipulation utilities.

#### An Overview of String-Manipulation Functions

# We go over basic here, omitting some arguments
# and we do not cover all string manipulation
# functions in R

# Basics of Character Data

# Character values in R can be stored as 
# scalars, vectors, or matrices, or they
# can be columns of a data frame or elements 
# of a list. 

# When applied to objects like this, length()
# function will report the number of character 
# values in the object, not the number of 
# characters in each string. 

# To find the number of characters in a 
# character value, the nchar function can be 
# used. Like most functions in R, nchar is 
# vectorized. 

##### nchar()

# nchar(x) returns the length of a string x. 

nchar("South Pole")

# note: a blank space is a character. Also, note
# the results of nchar() will be unpredictable 
# if x is not in character mode. For example:

nchar(NA)

# is 2

nchar(factor("abc"))

# returns an error

# For more consistent results on nonstring objects,
# use Hadley Wickham's stringr package on CRAN.

# Names of the fifty states in U.S. can be
# found in the vector state.name. 

# To find the lengths of the states' names:

nchar(state.name)
length(state.name)

##### Displaying and Concatenating 
##### Character Strings

# Character values are displayed when their 
# name is types or when passed to the print() 
# function. However, it is often more 
# convenient to print or display these 
# objects directly without the subscripts 
# that the print function provides. 

# The cat() function combines character values 
# and prints them to the screen or a file 
# directly, coercing its arguments to 
# character values:

##### cat()

x = 7
y = 10

cat('x should be greater than y, but x =',
    x,'and y =',y,'\n')

# Note use of a newline (\n). cat() always prints 
# a newline when encounters a newline character.

# With multiple strings passed to cat, or when 
# argument is a vector of character strings, 
# the fill= argument can be used to insert
# newlines into the output string. 

# If fill= set to TRUE, the value width option 
# is used to determine linesize, although cat()
# will not insert newlines into individual 
# elements input:

cat('Long strings can','be displayed over',
    'several lines using','the fill= argument',
    fill=40)

cat('Long strings can','be displayed over',
    'several lines using','the fill= argument',
    fill=20)

# fill=10 does not truncate longer strings
cat('Long strings can','be displayed over',
    'several lines using','the fill= argument',
    fill=10)

often cat() preferred to paste because paste
# returns [1] indexes and quote marks "...."
# cat() does not so cat() better to write
# output from programs:

cat("North","and","South","Poles")

# use of cat() in program
(x_list <- seq(1, 9, by=2))

# initialize to zero
sum_x <- 0

# keep summing through vector
for (x in x_list){
  sum_x <- sum_x+x
  cat("The current loop element is",x,"\n")
  cat("The cumulative total is",sum_x,"\n")
}

sum(x_list)

# paste() function allows more control over
# character values are concatenated. In its 
# simplest usage, this function will accept 
# an unlimited number of scalars, and join 
# them together, separating each scalar with
# a space by default. 

##### paste()

# paste(...) concatenates several strings, 
# returning the result in one long string:

paste("North","Pole")

# default space put between 'North' and 'Pole'

paste("North","Pole",sep="")

# sep = "" means no space

paste("North","Pole",sep=".")

paste("North","and","South","Poles")

# often cat() preferred to paste because paste
# returns [1] indexes and quote marks "...."
# cat() does not so cat() better to write
# output from programs:

# Can use a character string other than a 
# space with sep= argument. paste() also
# converts any non-character object to mode
# character:
paste('one',2,'three',4,'five')

# paste() inserts one space as separator
# by default.

# collapse= to specify character string
# to insert between each element, but
# only when joining elements in a vector:
paste(c('one','two','three','four'),
      collapse=' hello ')

# collapse works, coerces integers to char:
paste(c(7,1,13,22),
      collapse=' hello ')

# note collapse= must be used in above case,
# sep= has no effect when applied to a vector:
paste(c('one','two','three','four'),
      sep='---',collapse=' hello ')

# collapse does not work with indiv elements:
paste(7,1,13,22,collapse=' hello ')

# note collapse= does not work when applied
# to individual character elements to join:
paste('one','two','three','four',
      sep='-&-',collapse=' hello ')

# When multiple arguments passed, paste()
# vectorizes the operation, recycling shorter 
# elements when necessary, so can generate
# variable names with a common prefix:
paste('X',1:5,sep='')
paste(c('X','Y'),1:5,sep='')

# can use sep= to control what is in between
# each set of values that are combined, and
# collapse= to specify to use when joining
# those individual values:
paste(c('X','Y'),1:5,
      sep='*',collapse='|')

# same sort of operations can be applied
# to multiple arguments to paste:
paste(c('X','Y'),1:5,'^',
      c('a','b'),sep='_',collapse='|')

# by omitting collapse= argument, individual
# pasted pieces are returned separately
# rather than a single string:
paste(c('X','Y'),1:5,'^',
      c('a','b'),sep=' ')

# returns same but as one long string:
paste(c('X','Y'),1:5,'^',
      c('a','b'),collapse='|')

##### sprintf()

# sprintf(...) assembles a formatted string from 
# the provided parts:

i <- 8
s <- sprintf("the square of %d is %d",i,i^2)
s

# the function prints to the string s. What are we
# we printing? The function says to first print 
# "the square of" and then print the value
# of i and then the square of i.

##### Working with Parts of Character Strings

# Cannot access individual characters of
# character strings through subscripting. 

# substring() function can be used either 
# to extract parts of character strings or 
# to change the values of parts of character 
# strings.

# substring() also accepts a first= argument
# specifying the first character of the 
# desired substring, and a last= argument
# specifying the last character. 

# Otherwise, last= defaults to a large number
# so specifying just a start= value will 
# operate from that character to the end of
# the string. 

# substring() is vectorized, operating on 
# multiple string elements at once:
substring(state.name,2,6)

# Notice in the case of strings that have fewer
# characters than specified in the last=
# argument (Ohio or Texas), substring() returns
# as many characters as it finds with no 
# padding provided.

# Vectorization takes place for the first= and 
# last= arguments as well as for character
# vectors passed to subscript. 

mystring = 'dog cat duck'

# point here is that substring gets vectorized
# goes from one string to several
substring(mystring,c(1,5,9),c(3,7,12))

# Can find locations of particular characters 
# within a character string, but the string
# must first be converted to a character vector 
# with the individual characters.

# Can do by passing a vector consisting of all 
# the characters to be processed as both the 
# first= and last= arguments, and then applying
# which to the result:
  
state = 'Mississippi'
# use nchar() to figure out how long string is:
end = nchar(state);end
# is a good 'trick':
ltrs = substring(state,1:end,1:end);ltrs
which(ltrs == 's') 

# The assignment form of substring() allows 
# replacement of selected portions of character
# strings, but substring will only replace parts
# of the string with values that have the same
# number of characters; if a string that's 
# shorter than the implied substring is provided, 
# none of original string is overwritten:

# only replaces 3 characters, not 5:
mystring = 'dog cat duck'
substring(mystring,5,7) = 'tiger'
mystring

# only replaces one character, not 3:
mystring = 'dog cat duck'
substring(mystring,5,7) = 'b'
mystring

##### Breaking Apart Character Strings

##### strsplit()

# strsplit(x,split) splits a string x into an 
# R list of substrings based on identified
# splitting character in second argument:

strsplit("6-16-2011",split="-")

# strsplit() function can use a character 
# string or regular expression to divide
# a character string up into smaller pieces. 

# First argument is character string to break 
# up, second argument is character value or
# regular expression to be used to break up 
# the string.

# strsplit() always returns a list:
## strsplit divides a character string
sentence = 'R is a free software environment for statistical computing'
nchar(sentence)
parts = strsplit(sentence,' ')
parts
class(parts)

# can access thru first element (only) of list:
length(parts)
length(parts[[1]])

# if input to strsplit is vector of character 
# strings, can use sapply to process results 
# for each of the strings:
some.text = c('R is a free software environment for statistical computing',
              'It may be compiled and run on many UNIX machines')
result = strsplit(some.text,' ')
result
class(result)
# will return # elements in each component:
sapply(result,length)

# if structure of output unimportant, all of
# split parts can be combined with unlist():
allparts = unlist(result)
class(allparts)
allparts

# Because strsplit can accept regular 
# expressions to decide where to split a
# a character string, can handle many situations
# for example, can find multiple empty spaces:
str = 'one  two   three four'
strsplit(str,' ')

# using an empty string as splitting character
# strsplit() can return a list of individual 
# characters from vector of character strings:

# By using a regular expression representing
# one or more blanks (+ modifier) we can extract
# only the nonempty strings:
strsplit(str,' +')

# Using empty string as splitting character,
# can return list of individual characters from
# vector of character strings:
words = c('one two','three four')
words
strsplit(words,'') 

##### Regular Expressions

# Regular expressions are a method of expressing 
# patterns in character values which can then be
# used to extract parts of strings or to modify 
# those strings in some way. Regular expressions
# are supported in the R functions: strsplit(),
# grep(), sub(), and gsub(), as well as in the 
# regexpr() and gregexpr() functions which are
# the main tools for working with regular 
# expressions in R.

# The backslash character (\) is used in regular
# expressions to signal that certain characters
# with special meaning in regular expressions 
# should "escape" and be treated as normal
# characters. 

# So two backslash characters must be entered
# into an input string anywhere that special 
# characters need to be escaped.

# Although the double backslash will display 
# when the string is printed, nchar() or cat()
# can verify that only a single backslash is 
# actually included in the string.

# For example, in regular expressions, a period 
# (.) is ordinarily matched by any single char-
# acter. To create a regular expression that 
# would match file names with an extension of 
# ".txt", we could use a regular expression:

# is really 8 characters
expr = '.*\\.txt'

# only counts 7
nchar(expr)

# only 'sees' 7:
cat(expr,'\n')

# Single backslashes, like those which are part 
# of a newline character (\n), will be seen
# correctly inside of regular expressions. One 
# way to avoid need for quotes or double 
# backslashes is to use the readline() function 
# to enter regular expressions into R.

expr = readline()
# type in concole .*\.txt and then hit enter
# sees 7 characters
nchar(expr)

expr = readline()
# type in concole .*\\.txt and then hit enter
# sees 8 characters
nchar(expr)

##### Basics of Regular Expressions

## SEE SLIDES

# Regular expressions are simply character 
# strings so they can be manipulated like 
# any other character strings.

# vertical bar (|) used in regular expressions 
# to express alternation. To create a regular
# expression that would be matched by several 
# different strings, we can combine the strings
# using the bar as a separator:

strs = c('chicken','dog','cat')
expr = paste(strs,collapse='|')
expr

# Variable expr could now be used as a regular 
# expression to match any of the words in the
# original vector.

##### Using Regular Expressions

##### Regular Expressions (Can be very complex)

# When dealing with string-manipulation functions 
# in programming languages, the notion of regular 
# expressions sometimes arises. In R, you must 
# pay attention to this point when using the 
# string functions grep(), grepl(), regexpr(),
# gregexpr(), sub(), gsub(), and strsplit().

# A regular expression is like a wild card. 
# It's shorthand to specify broad classes of 
# strings. For example, the expression "[au]" 
# refers to any string that contains either of 
# the letters a or u. You could use it like this:

grep("[au]",c("Equator",
              "North Pole",
              "South Pole"))

# elements 1 and 3 of ("Equator","North Pole",
# "South Pole")-that is, "Equator" and "South 
# Pole"-contain either an a or a u.

# A period (.) represents any single character: 

grep("o.e",c("Equator",
             "North Pole",
             "South Pole"))

# searches for three-character strings in which 
# an o is followed by any single character, which 
# is in turn followed by an e. Here is an example 
# of the use of two periods to represent any pair 
# of characters:

grep("N..t",c("Equator",
              "North Pole",
              "South Pole"))

# we searched for four-letter strings consisting 
# of an N, followed by any pair of characters, 
# followed by a t.

# A period is an example of a metacharacter, 
# which is a character that is not taken literally.
# For example, if a period appears in the first 
# argument of grep(), it doesn't actually mean 
# a period; it means any character.

# if you actually want to search for a period 
# using grep() can use this naive approach:

grep(".",c("abc","de","f.g"))

# result should have been 3, not (1,2,3). This 
# call failed because periods are metacharacters. 
# You need to escape the metacharacter nature of 
# the period by using a backslash:

grep("\\.",c("abc","de","f.g"))

# why do we use two backslashes? Because the
# backslash itself must be escaped, which is 
# accomplished by another backslash.

## THIS SHOWS HOW COMPLEX A REGULAR EXPRESSION
## CAN BECOME ! ! 

## grep()

# Can use grep() with regular expression
# or without regular expressions

# grep() function accepts a regular expression 
# and a character string or vector of character
# strings, and returns the indices of those 
# elements which are matched. 

# argument value=TRUE argument will return the 
# actual strings which matched the expression
# instead of the indices.

##### grep()

# The call grep(pattern,x) searches for a 
# specified substring pattern in vector x of
# strings. If x has n elements-that is, it 
# contains n strings-then grep(pattern,x) 
# returns a vector of length up to n. Each 
# element of this vector will be the index 
# in x at which a match of pattern as a 
# substring of x[i]) was found.

# grep returns index

grep("Pole",c("Equator",
              "North Pole",
              "South Pole"))

# found matches in second and third vector element

# grepl (for logical) returns TRUE or FALSE

grepl("Pole",c("Equator",
               "North Pole",
               "South Pole"))

# found matches in second and third vector element

# to return the matching value of the entire
# string where the match is found (instead of index) 
# use argument value=TRUE

grep("Pole",c("Equator",
              "North Pole",
              "South Pole"),value=TRUE)


# using grep() without regular expressions:

install.packages("DAAG")
library(DAAG)
data(Cars93)
nrow(Cars93)
head(Cars93[,1:8])

# to output column indices of variables in
# Cars93 beginning with 'MPG' we can say
grep('MPG',names(Cars93))

# to output variable names in Cars93 cols 1-8
# beginning with 'MPG' we can use value=TRUE
grep('MPG',names(Cars93),value=TRUE)

# to create data frame with just these variables
head(Cars93[,grep('^MPG',names(Cars93[,1:8]))])

str1 = c('The R Foundation',
         'is a not for profit organization',
         'working in the public interest')

grep('profit',str1)

# If string passed to grep has no match in any
# inputs, grep returns empty numeric vector
str2 = c(' It was founded by the members',
         'of the R Core Team in order',
         'to provide support for the R project')

grep('profit',str2)

# any() function tests whether string occurs:
any(grep('profit',str1))
any(grep('profit',str2))

# using grep() with regular expressions

# One important use of grep is to extract a set 
# of variables from a data frame based on their
# names. For example, the LifeCycleSavings data 
# frame contains two variables with information 
# about the percentage of population less than
# 15 years old (pop15) or greater than 75 years 
# old (pop75). 

head(LifeCycleSavings)

# Since both of variables begin with the 
# string "pop", we can find their indices
# or values using grep:
grep('^pop', names(LifeCycleSavings))
grep('^pop', names(LifeCycleSavings), value=TRUE)

# Can create a data frame with just these 
# variables, we can use the output of grep
# as a subscript:

# getting column indices with 'pop':
head(LifeCycleSavings[,grep('^pop',names(LifeCycleSavings))])

# To find regular expressions without regard to 
# the case (upper or lower), use 
# ignore.case=TRUE argument: 

# search for string dog without case-specificty:

inp = c('run dog run',
        'work doggedly',
        'CAT AND DOG')
inp

# ignores case:
grep('\\<dog\\>',inp,ignore.case=TRUE)

##### regexpr()

# regexpr(pattern,text) finds the character 
# position of only the first instance of
# pattern within text:
?regexpr
regexpr("uat","Equator")

# "uat" did indeed appear in "Equator," 
# starting at character position 3.

##### gregexpr()

# gregexpr(pattern,text) is the same as regexpr(), 
# but it finds all instances (not just the first )
# of pattern:

gregexpr("iss","Mississippi")

# it also returns the length of each match

# finds that "iss" appears twice in "Mississippi," 
# starting at character positions 2 and 5.

# regexpr() and gregexpr() functions tell where 
# regular expressions were found. If there is
# no match, value of -1 is returned.

# The output attribute match.length also provides 
# information about exactly which characters 
# were involved in the match.

# regexpr() provides information about first 
# match only in string.

# gregexpr() returns information about 
# all matches found in string.

# regexpr() always returns a vector with -1 
# in positions for no match.

# substr() will extract the strings that matched
# after calculating the ending position from
# regexpr() output and the match.length attribute:

tst = c('one x7 two b1',
        'three c5 four b9',
        'five six seven',
        'a8 eight nine')
tst

wh = regexpr('[a-z][0-9]',tst)
wh
?substring
# can find and extract the matching 
# characters with match.length and substring:
res=substring(tst, wh, 
              wh+attr(wh,'match.length')-1)
res

# no match in third string
# can remove non-matches with
res[res != '']

# gregexpr() output is similar to regexpr() 
# except (like strsplit()), gregexpr() always
# returns result in the form of a list
# gregexpr() also returns all matches, 
# not just first
wh1 = gregexpr('[a-z][0-9]',tst)
wh1

# can further process results from gregexpr()
# using substring() for each element of 
# output list

res1 = list()
res1
class(res1)
for(i in 1:length(wh1))
  res1[[i]] = substring(tst[i],
                        wh1[[i]],
                        wh1[[i]] +
                          attr(wh1[[i]],
                               'match.length')-1)
res1

# another way to process output is mapply()
# first mapply argument is a function that 
# accepts multiple arguments; remaining
# arguments are vectors of = lengths whose
# elements will be passed to that function 
# one at a time
getexpr <- function(str,greg)substring(str,greg,greg+attr(greg,'match.length')-1)


# now call mapply with the two vectors:
res2 = mapply(getexpr,tst,wh1)
res2

# mapply uses the input strings as names 
# in the output

##### EXTENDED EXAMPLE: Testing a Filename 
##### for a Given Suffix

# Suppose we wish to test for a specified suffix 
# in a filename. We might, for instance want
# to find all HTML files (those with suffix .html, 
# .htm, and so on). Here is R script:

testsuffix <- function(fn,suff) {
  parts <- strsplit(fn,".",fixed=TRUE)
  nparts <- length(parts[[1]])
  return(parts[[1]][nparts] == suff)
}

# We test it:
?strsplit
testsuffix("x.abc","abc")

testsuffix("x.abc","ac")

testsuffix("x.y.abc","ac")

testsuffix("x.y.abc","abc")

# How does the function work? First note that 
# the call to strsplit() on line 2 returns a
# list consisting of one element (because fn 
# is a one-element vector)-a vector of strings.

# For example, calling testsuffix("x.y.abc",
# "abc") will result in parts being a list 
# consisting of a three-element vector with 
# elements x, y, and abc. We then pick up the 
# last element and compare it to suff.

# A key aspect is the argument fixed=TRUE. 
# Without it, the splitting argument '.' 
# (called split in the list of strsplit()'s 
# formal arguments) would have been treated
# as a regular expression. Without setting 
# fixed=TRUE, strsplit() would have just
# separated all the letters.

?strsplit

# Of course, we could also escape the period, 
# as follows:

testsuffix <- function(fn,suff) {
  parts <- strsplit(fn,"\\.")
  nparts <- length(parts[[1]])
  return(parts[[1]][nparts] == suff)
}

# still works:
testsuffix("x.y.abc","abc")

# Here's any alternative approach, more
# complex, but a decent example:

testsuffix <- function(fn,suff) {
  # nchar() gives the string length
  ncf <- nchar(fn)
  # determine where the period would 
  # start if suff is the suffix in fn
  dotpos <- ncf - nchar(suff) + 1
  # now check that suff is there
  return(substr(fn,dotpos,ncf)==suff)
}

# We look at the call to substr() here, 
# again with fn = "x.ac" and suff = "abc". 
# In this case, dotpos will be 1, which 
# means there should be a  period at the
# first character in fn if there is an abc 
# suffix. The call to substr() then becomes
# substr("x.ac",1,4), which extracts the 
# substring in character positions 1 through 
# 4 of x.ac. That substring will be x.ac, 
# which is not abc, so the filename's suffix 
# is found not to be the latter.


##### EXTENDED EXAMPLE: Forming Filenames

# Suppose we want to create five files, 
# q1.pdf through q5.pdf, consisting of
# histograms of 100 random N(0,i2) variates. 
# We could execute the following script:

for (i in 1:5) {
  fname <- paste("q",i,".pdf")
  pdf(fname)
  hist(rnorm(100,sd=i))
  dev.off()
}

# The  point is the string manipulation we 
# use to create the filename 'fname'. 

# The paste() function concatenates the 
# string "q" with the string form of
# the number i. For example, when i = 2, 
# the variable fname will be q 2 .pdf.

# However, that isn't quite what we want. 
# We want to remove the spaces. One solution
# is to use the 'sep' argument, specifying an 
# empty string for the separator:

for (i in 1:5) {
  fname <- paste("q",i,".pdf",sep="")
  pdf(fname)
  hist(rnorm(100,sd=i))
  dev.off()
}

# Or could use the sprintf() function, 
# borrowed from C:

for (i in 1:5) {
  fname <- sprintf("q%d.pdf",i)
  pdf(fname)
  hist(rnorm(100,sd=i))
  dev.off()
}

# For floating-point quantities, note also 
# the difference between %f and %g formats:

sprintf("abc%fdef",1.5)

# [1] "abc1.500000def"

sprintf("abc%gdef",1.5)

# [1] "abc1.5def"

# The %g format eliminated 
# the superfluous zeros.

## String Utilities are used extensively in
## edtdbg debugging tool as we will see in
## that session.

##### Substitutions and Tagging

# substitute text based on regular expressions:
# use sub() and gsub() functions; each accepts:
# 1) a regular expression;
# 2) string of what will be substituted 
#    for regular expression;
# 3) string (or strings) to operate on;

# sub() changes only the first occurrence
# gsub() changes all occurrences
?gsub
# can be used used in financial reports:
values = c('$11,317.35',
           '$11,234.51',
           '$11,275.89',
           '$11,278.93',
           '$11,294.94')
values

# to use as numbers, must remove commas and $ signs
# before as.numeric() function can be used

# want a regular expression to find either 
# commas or $ signs so we use a character 
# class passed to gsub() with an empty
# substitution pattern, providing values
# which can be converted to numbers:
as.numeric(gsub('[$,]','',values))

# when use substitution, can use feature 
# known as tagging. Example: In financial
# reports, often surround negative numbers
# with parentheses.....this is a problem for
# R....they prevent R from recognizing them
# as negative numbers (are unescaped 
# parentheses). 

# We can tag the number in parentheses 
# using a regular expression, and substitute
# the value by preceding it with a minus sign.

# Note difference between literal parentheses 
# (preceded by \\) and the parentheses used 
# for tagging:
values = c('75.99','(20.30)','55.20')
as.numeric(gsub('\\(([0-9.]+)\\)',
                '-\\1',values))

# To extract just tagged pattern from a 
# regular expression we can use regular
# expression beginning and end characters
# (^ and $, respectively) to account for 
# all nontagged characters in the string,
# and thus just specify the tagged expression
# for the substitution string

# EXAMPLE: We want to extract any 
# value preceded by 'value='
str = 'report: 17 value=12 time=2:00'
sub('value=([^ ]+)','\\1',str)

# we expand the regular expression 
# to include all unwanted parts
# and substitution extracts only 
# what we want
sub('^.*value=([^ ]+).*$','\\1',str)

# or could use regexpr() or gregexpr() 
# to find location of match, and apply
# sub() or gsub() to the extracted parts:

# same result as previous example:
str = 'report: 17 value=12 time=2:00' 
greg = gregexpr('value=[^ ]+',str)[[1]]
sub('value=([^ ]+)','\\1', 
    substring(str,greg,greg 
              + attr(greg, 'match.length') - 1))