# DS-A100 practice question:
# Biotech Inc. asks your help in identifying the motif that is most over-represented in 
# the viral genome and therefore likely to be the culprit. They have narrowed it down to
# one of 6 possible sequences: AATCG, ATNCGA, TTGCA, TTNCC, AATGCC or CCGTA.
# N is a wildcard representing any nucleotide (any of A, T, G, or C).
# Additionally they tell you that the prevalence (rate of occurance in nature) of each 
# nucleotide in viral genomes is as follows: A with probability of 0.3, T with probability of 0.4,
# G with probability of 0.1, C with probability of 0.2. They additionally provide you with the
# zika virus genome which is 10000 nucleotides long.

#get genome data
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
# List of motifs.
motifs = list(
    "AATCG", 
    "TTGCA", 
    c("TTACC","TTTCC","TTGCC","TTCCC"), 
    "CCGTA",
    c("ATACGA","ATTCGA","ATGCGA","ATCCGA"),
    "AATGCC"
  )


# function to extract all five base length motifs
fivebases <- function(charVector){
    vlength <- length(charVector) - 5
    fiveb <- vector(length = vlength)
    
     for(i in 1:vlength){
         fiveb[i] <- paste(charVector[i:(i+4)],collapse = '')
       }
   return (fiveb)
}

# function to extract all six base length motifs
sixbases <- function(charVector){
  vlength <- length(charVector) - 6
  sixb <- vector(length = vlength)
  
  for(i in 1:vlength){
    sixb[i] <- paste(charVector[i:(i+5)],collapse = '')
  }
  return (sixb)
}

# store five and six base lengths into variables
fiveb <- fivebases(genome)
sixb <- sixbases(genome)

# create empty vector to store motif counts in
allstats <- vector("numeric",length = 5 )

# create function to count all motif groups found in zika genome
statslist <- function(five,six){
      allstats[1] <- sum(five == "AATCG")
      allstats[2] <- sum(six == "ATACGA" | six == "ATTCGA" | six == "ATCCGA" | six == "ATGCGA"  )
      allstats[3] <- sum(five == "TTACC" |five == "TTGCC" | five == "TTCCC" | five == "TTTCC")
      allstats[4] <- sum(six == "AATGCC")
      allstats[5] <- sum(five == "CCGTA")
 
      return (allstats)     
}


bases = c("A","T","G","C")
# The rate at which these 4 bases occur in nature for viruses
basePrevalence = c(0.3, 0.4, 0.1, 0.2)
Ab <- 0.3
Tb <- 0.4
Gb <- 0.1
Cb <- 0.2
# calculate the natural probabilities of these genomes in the order
# AATCG, ATNCGA, TTNCC, AATGCC, CCGTA
probs <- c(Ab*Ab*Tb*Cb*Gb, 
(Ab*Tb*Ab*Cb*Gb*Ab + Ab*Tb*Tb*Cb*Gb*Ab + Ab*Tb*Cb*Cb*Gb*Ab + Ab*Tb*Gb*Cb*Gb*Ab),
(Tb*Tb*Ab*Cb*Cb + Tb*Tb*Gb*Cb*Cb + Tb*Tb*Cb*Cb*Cb + Tb*Tb*Tb*Cb*Cb),
Ab*Ab*Tb*Gb*Cb*Cb, Cb*Cb*Gb*Tb*Ab)

# store counts of the 4 motif groups as found in the zika virus
s <- statslist(fiveb,sixb)
# calculate the probabilities of the zika virus motifs by dividing the
# respective motif counts with the total motifs of n length
num_fiveb <- length(fiveb)
num_sixb <- length(sixb)
listprob <- s/c(num_fiveb, num_sixb, num_fiveb, num_sixb, num_fiveb)

# create vector to store the normal probability of a motif next to its
# zika virus probability
comp_prob <- vector("numeric", length = 10)

for(i in 1:5){
    comp_prob[(2*i-1)] = probs[i]
    comp_prob[(2*i)] = listprob[i]
}

# give names for each probability
names(comp_prob)<- c("nAATCG","zAATCG","nATNCGA","zATNCGA","nTTNCC",
                     "zTTNCC","nAATGCC","zAATGCC","nCCGTA", "zCCGTA") 
# plot normal and zika virus motif probabilities
barplot(comp_prob, beside = T,
        col = c("grey", "darkgrey", "red",
                "darkred", "khaki", "darkkhaki", 
                "orange", "darkorange", "cyan", "darkcyan" ),
        xlab = "Motifs",
        ylab = "Probability",
        main = "Comparison of natural(n)\nand zika(z) motif probabilities",
        ylim = c(0,0.007))
# it may be better to show the enrichment percentage
# so we create an enrichment vector and plot it
enrichment <- c((comp_prob[2]*100/comp_prob[1]),
                (comp_prob[4]*100/comp_prob[3]),
                (comp_prob[6]*100/comp_prob[5]),
                (comp_prob[8]*100/comp_prob[7]),
                (comp_prob[10]*100/comp_prob[9]))
names(enrichment) <- c("AATCG","ATNCGA", "TTNCC", "AATGCC", "CCGTA")
barplot(enrichment, col = rainbow(5),
        xlab = "Motif",
        ylab = "Enrichment Percentage",
        main = "Comparison of enrichment percentages",
        ylim = c(0,1000))

