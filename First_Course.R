datafile <- file.path(datapath, "urban.csv.gz")
urban<-read_csv(datafile)
urban<-read_csv(datafile, col_types='cccdc')
urban<-read_csv(datafile, col_types='cccd-')
urban<-read_csv(datafile, col_types='cccd-', n_max=100)


object.size(plants)


wc_4 <- worldcup %>% 
  select(Time, Passes, Tackles, Saves) %>%
  summarize(Time = mean(Time),
            Passes = mean(Passes),
            Tackles = mean(Tackles),
            Saves = mean(Saves)) %>%
  gather(var, mean) %>%mutate(mean=round(mean,1))
  
  
  
# | In this question, you will again continue to build on the data cleaning you started in the last two questions. I opened a new R script for you
# | with the previous steps completed. As a next step, you now need to create a new column called agecat that divides a person's age into three broad
# | categories (Under 15, 15 to 50, Over 50). To do this, add a function from `dplyr` or `tidyr` to the pipe chain in the script. Your goal is to
# | re-create the example output shown in the comments of the script. When you are ready to submit your script, save the script and type `submit()`,
# | or type `reset()` to reset the script to its original state.


titanic_4 <- titanic %>% 
  select(Survived, Pclass, Age, Sex) %>%
  filter(!is.na(Age)) %>%
  mutate(agecat = cut(Age, breaks = c(0, 14.99, 50, 150), 
                      include.lowest = TRUE,
                      labels = c("Under 15", "15 to 50",
                                 "Over 50"))) %>%group_by(Pclass, agecat, Sex)%>%
                      summarise(N=n(), survivors=sum(Survived==1))%>%mutate(perc_survived=100*survivors/N)%>%ungroup()
					  
#######################					  
### Regular Expressions
#######################

paste("Square", "Circle", "Triangle")
paste("Square", "Circle", "Triangle", sep = "+")
paste0("Square", "Circle", "Triangle")


shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)


two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")


# You can also collapse all of the elements of a
# vector of strings into a single string by specifying the collapse argument:
paste(shapes, collapse = " ")


nchar("Supercalifragilisticexpialidocious")


cases <- c("CAPS", "low", "Title")
tolower(cases)
toupper(cases)



regular_expression <- "a"
string_to_search <- "Maryland"
grepl(regular_expression, string_to_search)

regular_expression <- "u"
string_to_search <- "Maryland"
grepl(regular_expression, string_to_search)

grepl("land", "Maryland")

grepl("ryla", "Maryland")

grepl("Marly", "Maryland")

grepl("dany", "Maryland")


head(state.name)

# "."
# The first metacharacter that we’ll discuss is ".". The
# metacharacter that only consists of a period represents any character other than a new line
# (we’ll discuss new lines soon). Let’s take a look at some examples using the peroid regex:


grepl(".", "Maryland")
#[1] TRUE
grepl(".", "*&2[0+,%<@#~|}")
#[1] TRUE
grepl(".", "")
#[1] FALSE

grepl("a.b", c("aaa", "aab", "abb", "acadb"))
# [1] FALSE TRUE TRUE TRUE


# You can specify a regular expression that contains a certain number of characters or
# metacharacters using the enumeration metacharacters. 
# The + metacharacter indicates that one or more of the preceding expression should be present
# The * indicates that zero or more of the preceding expression is present. Let’s take a look at some examples using these metacharacters:

# Does "Maryland" contain one or more of "a" ?
grepl("a+", "Maryland")
##[1] TRUE

# Does "Maryland" contain one or more of "x" ?
grepl("x+", "Maryland")
#[1] FALSE
# Does "Maryland" contain zero or more of "x" ?
grepl("x*", "Maryland")
#[1] TRUE


# You can also specify exact numbers of expressions using curly brackets {}. For example
# "a{5}" specifies “a exactly five times,” 
# "a{2,5}" specifies “a between 2 and 5 times,” and
# "a{2,}" specifies “a at least 2 times.” Let’s take a look at some examples:


# Does "Mississippi" contain exactly 2 adjacent "s" ?
grepl("s{2}", "Mississippi")
#[1] TRUE
# This is equivalent to the expression above:
grepl("ss", "Mississippi")
#[1] TRUE


# Does "Mississippi" contain between 2 and 3 adjacent "s" ?
grepl("s{2,3}", "Mississippi")
#[1] TRUE
# Does "Mississippi" contain between 2 and 3 adjacent "i" ?
grepl("i{2,3}", "Mississippi")
#[1] FALSE


# Does "Mississippi" contain between 2 adjacent "iss" ?
grepl("(iss){2}", "Mississippi")
#[1] TRUE

# Does "Mississippi" contain between 2 adjacent "ss" ?
grepl("(ss){2}", "Mississippi")
#[1] FALSE

# Does "Mississippi" contain the pattern of an "i" followed by
# 2 of any character, with that pattern repeated three times adjacently?
grepl("(i.{2}){3}", "Mississippi")
#[1] TRUE

# In the last three examples I used parentheses () to create a capturing group. A capturing
# group allows you to use quantifiers on other regular expressions. In the last example I first
# created the regex "i.{2}" which matches i followed by any two characters (“iss” or “ipp”).
# I then used a capture group to to wrap that regex, and to specify exactly three adjacent
# occurrences of that regex.




# You can specify sets of characters with regular expressions, some of which come built in,
# but you can build your own character sets too. First we’ll discuss the built in character sets:
# words ("\\w"), digits ("\\d"), and whitespace characters ("\\s"). Words specify any letter,
# digit, or a underscore, digits specify the digits 0 through 9, and whitespace specifies line
# breaks, tabs, or spaces. Each of these character sets have their own compliments: not words
# ("\\W"), not digits ("\\D"), and not whitespace characters ("\\S"). Each specifies all of the
# characters not included in their corresponding character sets. 

grepl("\\w", "abcdefghijklmnopqrstuvwxyz0123456789")
#[1] TRUE

grepl("\\d", "0123456789")
#[1] TRUE

# "\n" this regex for a new line and "\t" is the regex for a tab
grepl("\\s", "\n\t ")
#[1] TRUE

grepl("\\d", "abcdefghijklmnopqrstuvwxyz")
#[1] FALSE

grepl("\\D", "abcdefghijklmnopqrstuvwxyz")
#[1] TRUE

grepl("\\w", "\n\t ")
#[1] FALSE




# You can also specify specific character sets using straight brackets []. For example a character
# set of just the vowels would look like: "[aeiou]". You can find the complement to a specific
# character by putting a carrot ˆ after the first bracket. For example "[ˆaeiou]" matches all
# characters except the lowercase vowels. You can also specify ranges of characters using a
# hyphen - inside of the brackets. For example "[a-m]" matches all of the lowercase characters
# between a and m, while "[5-8]" matches any digit between 5 and 8 inclusive. Let’s take a look
# at some examples using custom character sets:
grepl("[aeiou]", "rhythms")
#[1] FALSE
grepl("[^aeiou]", "rhythms")
#[1] TRUE
grepl("[a-m]", "xyz")
#[1] FALSE
grepl("[a-m]", "ABC")
#[1] FALSE
grepl("[a-mA-M]", "ABC")
#[1] TRUE


# Putting two
# backslashes before a punctuation mark that is also a metacharacter indicates that you are
# looking for the symbol and not the metacharacter meaning. For example "\\." indicates
# you are trying to match a period in a string. Let’s take a look at a few examples:

grepl("\\+", "tragedy + time = humor")
#[1] TRUE
grepl("\\.", "http://www.jhsph.edu/")
#[1] TRUE


# There are also metacharacters for matching the beginning and the end of a string which
# are "ˆ" and "$" respectively. Let’s take a look at a few examples:

grepl("^a", c("bab", "aab"))
#[1] FALSE TRUE

grepl("b$", c("bab", "aab"))
#[1] TRUE TRUE

grepl("^[ab]+$", c("bab", "aab", "abc"))
#[1] TRUE TRUE FALSE



# The last metacharacter we’ll discuss is the OR metacharacter ("|"). The OR metacharacter
# matches either the regex on the left or the regex on the right side of this character. A few
# examples:
grepl("a|b", c("abc", "bcd", "cde"))
#[1] TRUE TRUE FALSE

grepl("North|South", c("South Dakota", "North Carolina", "West Virginia"))
#[1] TRUE TRUE FALSE


start_end_vowel <- "^[AEIOU]{1}.+[aeiou]{1}$"
vowel_state_lgl <- grepl(start_end_vowel, state.name)
head(vowel_state_lgl)
#[1] TRUE TRUE TRUE FALSE FALSE FALSE
state.name[vowel_state_lgl]
#[1] "Alabama" "Alaska" "Arizona" "Idaho" "Indiana" "Iowa"
#[7] "Ohio" "Oklahoma"





# Metacharacter Meaning
# . Any Character
# \w A Word
# \W Not a Word
# \d A Digit
# \D Not a Digit
# \s Whitespace
# \S Not Whitespace
# [xyz] A Set of Characters
# [ˆxyz] Negation of Set
# [a-z] A Range of Characters
# ˆ Beginning of String
# $ End of String
# \n Newline
# + One or More of Previous
# * Zero or More of Previous
# ? Zero or One of Previous
# Either the Previous or the Following
# {5} Exactly 5 of Previous
# {2, 5} Between 2 and 5 or Previous
# {2, } More than 2 of Previous


grepl("[Ii]", c("Hawaii", "Illinois", "Kentucky"))
# [1] TRUE TRUE FALSE


grep("[Ii]", c("Hawaii", "Illinois", "Kentucky"))
# [1] 1 2






# The sub() function takes as arguments a regex, a “replacement,” and a vector of strings. This
# function will replace the first instance of that regex found in each string.
sub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky"))
#[1] "Hawa1i" "1llinois" "Kentucky"


# The gsub() function is nearly the same as sub() except it will replace every instance of the
# regex that is matched in each string.
gsub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky"))
#[1] "Hawa11" "1ll1no1s" "Kentucky"




# The strsplit() function will split up strings according to the provided regex. If strsplit() is
# provided with a vector of strings it will return a list of string vectors.
two_s <- state.name[grep("ss", state.name)]
two_s
[1] "Massachusetts" "Mississippi" "Missouri" "Tennessee"
strsplit(two_s, "ss")
[[1]]
[1] "Ma" "achusetts"
[[2]]
[1] "Mi" "i" "ippi"
[[3]]
[1] "Mi" "ouri"
[[4]]
[1] "Tenne" "ee"


# The "stringr" Package

# The str_extract() function returns the sub-string of a string that matches the providied
# regular expression.

library(stringr)
state_tbl <- paste(state.name, state.area, state.abb)
head(state_tbl)
# [1] "Alabama 51609 AL" "Alaska 589757 AK" "Arizona 113909 AZ"
# [4] "Arkansas 53104 AR" "California 158693 CA" "Colorado 104247 CO"
str_extract(state_tbl, "[0-9]+")
# [1] "51609" "589757" "113909" "53104" "158693" "104247" "5009"
# [8] "2057" "58560" "58876" "6450" "83557" "56400" "36291"
# [15] "56290" "82264" "40395" "48523" "33215" "10577" "8257"
# [22] "58216" "84068" "47716" "69686" "147138" "77227" "110540"
# [29] "9304" "7836" "121666" "49576" "52586" "70665" "41222"
# [36] "69919" "96981" "45333" "1214" "31055" "77047" "42244"
# [43] "267339" "84916" "9609" "40815" "68192" "24181" "56154"
# [50] "97914"




# The str_order() function returns a numeric vector that corresponds to the alphabetical
# order of the strings in the provided vector.
head(state.name)
# [1] "Alabama" "Alaska" "Arizona" "Arkansas" "California"
# [6] "Colorado"
str_order(state.name)
# [1] 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
# [24] 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
# [47] 47 48 49 50
head(state.abb)
# [1] "AL" "AK" "AZ" "AR" "CA" "CO"
str_order(state.abb)
# [1] 2 1 4 3 5 6 7 8 9 10 11 15 12 13 14 16 17 18 21 20 19 22 23
# [24] 25 24 26 33 34 27 29 30 31 28 32 35 36 37 38 39 40 41 42 43 44 46 45
# [47] 47 49 48 50


# The str_pad() function pads strings with other characters which is often useful when the
# string is going to be eventually printed for a person to read.

str_pad("Thai", width = 8, side = "left", pad = "-")
#[1] "----Thai"
str_pad("Thai", width = 8, side = "right", pad = "-")
#[1] "Thai----"
str_pad("Thai", width = 8, side = "both", pad = "-")
#[1] "--Thai--"



# The str_to_title() function acts just like tolower() and toupper() except it puts strings into
# Title Case.

cases <- c("CAPS", "low", "Title")
str_to_title(cases)
# [1] "Caps" "Low" "Title"


# The str_trim() function deletes whitespace from both sides of a string.
to_trim <- c(" space", "the ", " final frontier ")
str_trim(to_trim)
#[1] "space" "the" "final frontier"



# The str_wrap() function inserts newlines in strings so that when the string is printed each
# line’s length is limited.
pasted_states <- paste(state.name[1:20], collapse = " ")
cat(str_wrap(pasted_states, width = 80))
# Alabama Alaska Arizona Arkansas California Colorado Connecticut Delaware Florida
# Georgia Hawaii Idaho Illinois Indiana Iowa Kansas Kentucky Louisiana Maine
# Maryland
cat(str_wrap(pasted_states, width = 30))
# Alabama Alaska Arizona
# Arkansas California Colorado
# Connecticut Delaware Florida
# Georgia Hawaii Idaho Illinois
# Indiana Iowa Kansas Kentucky
# Louisiana Maine Maryland


# The word() function allows you to index each word in a string as if it were a vector.
a_tale <- "It was the best of times it was the worst of times it was the age of wisdom it was the ag\
e of foolishness"
word(a_tale, 2)
#[1] "was"
word(a_tale, end = 3)
#[1] "It was the"
word(a_tale, start = 11, end = 15)
#[1] "of times it was the"


###http://tidytextmining.com/


### Memory
library(pryr)
mem_used()
#72 MB

# First, you might consider removing a few very large objects in your workspace. You can see
# the memory usage of objects in your workspace by calling the object_size() function.


# The object_size() function will print the number of bytes (or kilobytes, or megabytes) that a
# given object is using in your R session. If you want see what the memory usage of the largest
# 5 objects in your workspace is, you can use the following code.
library(magrittr)
sapply(ls(), function(x) object.size(get(x))) %>% sort %>% tail(5)
# worldcup denver check_tracks ext_tracks miami
# 61424 222768 239848 1842472 13121608

mem_change(rm(check_tracks, denver, b))



# The .Machine object in R (found in the base package) can give you specific details about how
# your computer/operation system stores different types of data.

str(.Machine)



# # Task DBI Function
# # Create a new driver object for an instance of a database dbDriver
# # Connect to database instance dbConnect
# # Find available tables in a connected database instance dbListTables
# # Find available fields within a table dbListFields
# # Query a connected database instance dbSendQuery
# # Pull a data frame into R from a query result dbFetch
# # Jointly query and pull data from a database instance dbGetQuery
# # Close result set from a query dbClearResult
# # Write a new table in a database instance dbWriteTable
# # Remove a table from a database instance dbRemoveTable
# # Disconnect from a database instance dbDisconnect


# # WEEK 4
## Q1
df2<-read_csv("daily_SPEC_2014.csv.bz2")
df%>%filter(`State Name`=="Wisconsin" & `Parameter Name`=="Bromine PM2.5 LC")%>%summarize(avg = mean(`Arithmetic Mean`, na.rm=TRUE))

## Q2
df%>%group_by(`Parameter Name`,`State Name`, `Site Num`, `Date Local`)%>%summarize(avg = mean(`Arithmetic Mean`, na.rm=TRUE))%>%ungroup()%>%arrange(desc(avg))


## Q3
df%>% filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
    group_by(`Site Num`,`County Code`,`State Code`) %>%
    summarise(avg = mean(`Arithmetic Mean`)) %>%ungroup()%>%arrange(desc(avg))
	
	
## Q4
df%>% filter(`Parameter Name` == "EC PM2.5 LC TOR" & `State Name` %in% c("California", "Arizona") ) %>%
    group_by(`State Name`) %>%
    summarise(avg = mean(`Arithmetic Mean`)) %>%ungroup()%>%arrange(desc(avg))%>%mutate(ddd=lag(avg), d=avg-ddd, d2=diff(avg))
	
## Q5
df%>% filter(`Parameter Name` == "OC PM2.5 LC TOR" & Longitude< -100 ) %>%
    summarise(median_value = median(`Arithmetic Mean`)) 
	
	
## Q6
df<-read_excel("aqs_sites.xlsx")
df%>%filter(`Land Use`=="RESIDENTIAL" & `Location Setting`=="SUBURBAN")%>%select(`Site Number`)%>%summarize(length(unique(`Site Number`)), n=n())

