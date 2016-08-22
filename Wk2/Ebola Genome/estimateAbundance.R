# get genome and avoid title with header = T
ebola.genome <- read.table("KM034562v1.fa", header = T)

estimateAbundance <- function(genome = ebola.genome, nucleotide = c("A", "T", "C", "G")){
  abundances <- vector("numeric",length(nucleotide))
  names(abundances) <- nucleotide
  
  # first convert to character vector
  genome.rows <- as.vector(ebola.genome[,1], mode = "character")
  
  genome.str <- vector("character")
  # join into one string
  for(i in 1:length(genome.rows)){
    genome.str <- paste(genome.str,genome.rows[i], collapse = '')
  }
  
  # split into individual nucleotides and return it as a vector
  ebola.nucleotides <- unlist(strsplit(genome.str, split = NULL))
  
  for(i in 1:length(nucleotide)){
    abundances[i] = sum(ebola.nucleotides == nucleotide[i])
  }
  
  # plot barplots
  if(length(abundances) > 1){ 
    barplot(abundances, 
            col = rainbow(length(abundances)),
            main = "Abundance of Nucleotides in Ebola Virus Genome",
            xlab = "Nucleotide",
            ylab = "Abundance")
  }
  
  return(abundances)
}

estimateAbundance(,c("T", "G", "A"))
