library(data.table)
library(stringr)
input <- readLines("./data/day02.txt")
#########
# part 1
#########
# convert to data.table
DT <- as.data.table(input)
# set friendly name
setnames(DT, "input")
# craete game id
DT[, game := .I]
# remove the game part from the input string
DT[, input := gsub(".*: ", "", input)]
# split the draw bij game
DT[, paste0("g", 1:length(tstrsplit(DT$input, ";"))) := tstrsplit(input, ";")]
# get names of draw columns
cols <- grep("^g[0-9]+?", names(DT), value = TRUE)
# melt to long format
DT <- melt(DT, measure.vars = cols, na.rm = TRUE, variable.name = "draw")
# red values red/blue/green
DT[, blue := as.numeric(str_extract(value, pattern = "(\\d+) blue", group = 1))]
DT[, red := as.numeric(str_extract(value, pattern = "(\\d+) red", group = 1))]
DT[, green := as.numeric(str_extract(value, pattern = "(\\d+) green", group = 1))]
 
#final answer
sum(unique(DT[!game %in% DT[red > 12 | green > 13 | blue > 14, game], game]))

#########
# part 2
#########
# melt previous set to long format by color
DT2 <- melt(DT, measure.vars = c("red", "green", "blue"), na.rm = TRUE, variable.name = "color", value.name = "colorn")
# summarise by game and het max number by color, product and sum
sum(DT2[, .(max(colorn)), keyby = .(game, color)][, .(prod(V1)), by = .(game)]$V1)
