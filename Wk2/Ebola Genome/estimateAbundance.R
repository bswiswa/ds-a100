# check working directory and make sure
# estimateAbundance.R and KM034562v1.fa are in that directory
# getwd()
# setwd("Ebola Genome")
# get genome and avoid title with header = T
ebola.genome <- read.table("KM034562v1.fa", header = T)

estimateAbundance <- function(genome = ebola.genome, nucleotide = c("A", "T", "C", "G")){
  # first convert to character one long string
  genome.str <- paste(ebola.genome[,1], collapse = '')
  
  # split into individual nucleotides and return it as a vector
  ebola.nucleotides <- unlist(strsplit(genome.str, split = ''))
  # summarize the data
  nucleotides.table <- table(ebola.nucleotides)
  
  # plot barplots
  if(length(nucleotides.table) > 1){ 
    barplot(nucleotides.table, 
            col = rainbow(length(nucleotides.table)),
            main = "Abundance of Nucleotides\n in the Ebola Virus Genome",
            xlab = "Nucleotide",
            ylab = "Abundance"
            )
  }
  
  return(nucleotides.table)
}

# sample run
estimateAbundance(,c("T", "G", "A", "C"))
