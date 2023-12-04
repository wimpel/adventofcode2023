library(data.table)
DT <- fread("./data/day04.txt", sep = ":", header = FALSE, col.names = c("id", "input"))
#########
# part 1
#########
# split card id from numbers
DT[, c("win", "card") := tstrsplit(input, " | ", fixed = TRUE)]
# split winning numbers and card numbers
DT.win <- DT[, .(id, win)]
DT.card <- DT[, .(id, card)]
DT.win[, paste0("w", 1:length(tstrsplit(DT.win$win, " +"))) := tstrsplit(win, " +", type.convert = TRUE)]
DT.card[, paste0("c", 1:length(tstrsplit(DT.card$card, " +"))) := tstrsplit(card, " +", type.convert = TRUE)]
# melt to long
DT.win <- melt(DT.win, id.vars = "id", measure.vars = patterns("^w[0-9]"), value.name = "win", na.rm = TRUE)
DT.card <- melt(DT.card, id.vars = "id", measure.vars = patterns("^c[0-9]"), value.name = "card", na.rm = TRUE)
# join
DT.card[DT.win, win := i.win, on = .(id = id, card = win)]
# calculate card value
sum(DT.card[!is.na(win), 1 * 2^(.N-1), by = .(id)]$V1)
#########
# part 2
#########
DT[, .(id)]
DT <- DT[, .(id)]
DT[, matches := 0]
DT[DT.card[!is.na(win), .N, keyby = .(id)], matches := i.N, on = .(id)]

vec <- c(rep(1,nrow(DT)))

for (i in seq.int(vec)) {
  card = i
  matches = DT$matches[card]
  from = card + 1
  to = card + matches
  aantal <- vec[card]
  if (matches > 0) vec[from:to] <- vec[from:to] + aantal
}

sum(vec, na.rm = TRUE)
