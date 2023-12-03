
input <- readLines("./data/day03.txt")
#input <- readLines("./data/day03_test.txt")

#########
# part 1
#########
library(stringr)
m <- matrix(unlist(strsplit(input, "")), nrow = length(input))
# melt tolong format
DT <- setDT(reshape2::melt(m))
setnames(DT, c("col", "row", "celwaarde"))
# 8 buurcellen
setkey(DT, row, col)
DT[, cel := paste0("c", col, "r", row)]
# buurcellsn
DT[, `:=`(b1 = paste0("c", col-1, "r", row-1),
          b2 = paste0("c", col, "r", row-1),
          b3 = paste0("c", col+1, "r", row-1),
          b4 = paste0("c", col-1, "r", row),
          b5 = paste0("c", col+1, "r", row),
          b6 = paste0("c", col-1, "r", row+1),
          b7 = paste0("c", col, "r", row+1),
          b8 = paste0("c", col+1, "r", row+1))]
# melt naar long format
DT2 <- melt(DT, measure.vars = patterns("^b"), variable.name = "buur", value.name = "buurcel")
# niet bestaande buurcellen verwijderen
DT2 <- DT2[buurcel %in% DT$cel,]
# ophalen waarde van de buurcel
DT2[DT, buurcelwaarde := i.celwaarde, on = .(buurcel = cel)]
# buurcellen met speciale waarde
buren.special <- DT2[!grepl("[0-9.]", buurcelwaarde), cel]
# naast special
final <- DT[, .(col, row, celwaarde, cel)]
final[, telt := 0]
final[cel %in% buren.special, telt := 1]
final[, getal := 0]
final[celwaarde %in% as.character(0:9), getal := 1]
final[, group_id := rleid(getal)]
final <- final[getal == 1,]
# summarise
part1 <- final[, .(getal = as.numeric(paste0(celwaarde, collapse = "")),
          specialbuur = sum(telt)),
      by = .(row, group_id)]
# part 1 answer
sum(part1[specialbuur > 0, getal])

#########
# part 2
#########
asterisk <- DT2[buurcelwaarde == "*", ]
setkey(asterisk, buurcel)
asterisk[, sterId := rleid(buurcel)]
final[, naastSter := 0]
final[asterisk, naastSter := i.sterId, on = .(cel)]
part2 <- final[, .(getal = as.numeric(paste0(celwaarde, collapse = "")),
                   specialbuur = sum(telt),
                   naastSter = paste0(unique(naastSter), collapse = ";")),
               by = .(row, group_id)]
part2[, paste0("s", 1:length(tstrsplit(part2$naastSter, ";"))) := tstrsplit(naastSter, ";", type.convert = TRUE)]
part2 <- melt(part2, measure.vars = patterns("^s[0-9]"), na.rm = TRUE, value.name = "ster", variable.name = "sterId")
part2 <- part2[ster > 0, ]
sum(part2[, if(.N>1) prod(getal), by=ster]$V1)
