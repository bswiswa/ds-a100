#setting path to work directory
currentDir = getwd()
currentDir
newDir <- paste(currentDir, "/Wk1", sep = '/')
newDir
setwd(newDir)
getwd()

dir()
load("zikaGenome.RData")
# size of the viral genome
genomeSize = 10000
# the 4 possible bases found in the genome
bases = c("A","T","G","C")
# The rate at which these 4 bases occur in nature for viruses
basePrevalence = c(0.3, 0.4, 0.1, 0.2)
Ab <- 0.3
Tb <- 0.4
Gb <- 0.1
Cb <- 0.2
probs <- 
#List of motifs.
motifs = list(
  "AATCG", 
  "TTGCA", 
  c("TTACC","TTTCC","TTGCC","TTCCC"), 
  "CCGTA",
  c("ATACGA","ATTCGA","ATGCGA","ATCCGA"),
  "AATGCC"
)

#home work write a function getkmerAtPosition = function(x, k, argString){
x = position,
k = length of monomer
kmer = monomer
#solution
getKmerAtPosition <- function(x,k,argStr){
  kmer <- paste(argStr[x:(x+k-1)], collapse = '')
  return (kmer)
}

z <- getKmerAtPosition(1,5,genome)
z <- c(1:5)
z[c(1:5)]
z <- c(1999:2010)
z
