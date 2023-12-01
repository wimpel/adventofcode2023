library(stringi)
library(stringr)
library(magrittr)

input <- readLines("./data/day1.txt")
#########
# part 1
#########
#extract first and last digit, paste together, set to numeric and then sum
sum(as.numeric(sapply(str_extract_all(input, pattern = "[0-9]"), function(x) paste0(x[1], x[length(x)]))))

#########
# part 2
#########
# lookup vector with left >> right replacent names
digits <- c(
  "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5, "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9, setNames(nm = 1:9)
)
# one   two three  four  five   six seven eight  nine     1     2     3     4     5     6     7     8     9 
#   1     2     3     4     5     6     7     8     9     1     2     3     4     5     6     7     8     9 

# lookup vector right >> left
reverse.digits <- digits
# do not forget to reverse the names! one >> eno
names(reverse.digits) <- stringi::stri_reverse(names(digits))
# eno   owt eerht  ruof  evif   xis neves thgie  enin     1     2     3     4     5     6     7     8     9 
#   1     2     3     4     5     6     7     8     9     1     2     3     4     5     6     7     8     9 

# function to find a digit based in an input vector and a named lookup vector
find_digit <- function(input, lookup_vector) {
  # we are looking in the names of the vector, create a regex pattern , using | = OR
  regex.pattern <- paste0(names(lookup_vector), collapse = "|")
  # what 'name' do we find first in out input
  found <- input %>% stringr::str_extract(regex.pattern)
  # get the value corres[onding with the name]
  lookup_vector[found]
}

sum(find_digit(input, digits) * 10 + find_digit(stringi::stri_reverse(input), reverse.digits))
