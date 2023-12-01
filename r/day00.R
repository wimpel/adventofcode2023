text <- "abhij
gcgfe
fdekh
edcji"

library(data.table)
library(reshape2)
# read puzzle input
input <- fread(text = text, sep = "", header = FALSE)
input <- fread("./data/slingers.txt", sep = "", header = FALSE)
# split to characters and create matrix
m <- matrix(data = unlist(tstrsplit(input$V1, "")), nrow = nrow(input))
# melt to long format
DT <- as.data.table(reshape2::melt(m))
setnames(DT, c("row", "col", "letter"))
DT[, celid := .I]
DT[, celnaam := paste0("r",row,"c",col)]
# maak een lookup tabel met letters
lookup <- data.table(letter = letters[1:26], waarde = 1:26)
# koppel letterwaarde aan cel
DT[lookup, celwaarde := i.waarde, on = .(letter = letter)]
# vind de 4 buurcellen
DT[, buurnaam1 := paste0("r", row + 1, "c", col)]
DT[, buurnaam2 := paste0("r", row - 1, "c", col)]
DT[, buurnaam3 := paste0("r", row, "c", col + 1)]
DT[, buurnaam4 := paste0("r", row, "c", col - 1)]
# melt naar lomng format
DT.long <- data.table::melt(DT, measure.vars = patterns("^buurnaam"), value.name = "buurnaam")
# rijen met een buurnaam die niet bestata mogen weg
DT.long <- DT.long[buurnaam %in% DT$celnaam, ]
# koppelen gegevens van de buur
DT.long[DT, `:=`(buurletter = i.letter,
                 buurid = i.celid,
                 buurwaarde = i.celwaarde), 
        on = .(buurnaam = celnaam)]
# welke buren hebben ene +1 waarde?


# knopen
vert <- DT[, .(celid,letter,celwaarde)]
# verbindingen
nodes <- DT.long[celwaarde == (buurwaarde - 1), .(from = celid, to = buurid)]

library(igraph)
g <- graph_from_data_frame(nodes, directed = FALSE, vertices = vert)
plot(g, vertex.label = V(g)$letter) 

#hoeveel slingers = components
count_components(g)

t(t(table(components(g)$csize)))

# mag alles ijn lengte 1 knippen

