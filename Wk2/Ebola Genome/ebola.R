# find enriched motifs in the ebola virus. 
# motifs can be of length 4, 5, 6, and 7
# Solution is based of the zika.R file
#fix ebola.nucleotides and basePrevalence back
# get ebola genome
ebola.genome <- read.table("KM034562v1.fa", header = T)

# break it into single nucleotides
genome.str <- paste(ebola.genome[,1], collapse = "")
ebola.nucleotides <- unlist(strsplit(genome.str,split = ""))

# size of the viral genome
genomeSize = length(ebola.nucleotides)
# the 4 possible bases found in the genome
bases = c("A","T","G","C")
# calculate base prevalence in genome
base.counts <- sapply(bases, function(x) return(sum(ebola.nucleotides == x)))
basePrevalence <- round((base.counts/sum(base.counts)),2)

findEnrichedKmers <- function(k, p){ 
  # get the null distribution by sampling random genomes
  all.possible.kMers <- getAllKMersOfSize(k)
  nullSamples = sampleManyGenomeKmerCounts(k)
  # save all the kmers with their counts in a list

  motifNullSamples <- t(nullSamples)
  
  # data frame of null samples (helps to group the numbers together)
  df.motifNullSamples <- as.data.frame(motifNullSamples)
  
  # get the distribution Probability Mass Function (pmf) PDF if continuous
  # for the null for this motif pmf for each kmer stored in a list
  
  pmf <- lapply(df.motifNullSamples, function(x) table(x)/length(x))
  
  #get the kmer vector
  allKmers = getAllKMersOfSize(k)
  #count the kmers in the zika virus
  ebolaKmerCounts = getKmerFreqVector(k,ebola.nucleotides,allKmers)
  #Get the count for all motifs
  ebolaMotifCount = ebolaKmerCounts
  # make list of matrices
  pmf.list <- lapply(names(ebolaMotifCount), function(x) t(as.matrix(pmf[[x]])))
  # make list of vectors
  pmf.list.vectors <- lapply(pmf.list, as.vector)
  # name the vectors
  for(i in 1:length(pmf.list.vectors)){
    names(pmf.list.vectors[[i]]) <- colnames(pmf.list[[i]])
  }
  
  #get the probability of observing a value at least this extreme by random chance
  pVals <- vector(mode = "numeric", length = length(pmf.list.vectors))
  for(i in 1:length(pmf.list.vectors)){
    pVals[i] <- sum(pmf.list.vectors[[i]][as.numeric(names(pmf.list.vectors[[i]])) >= ebolaMotifCount[i]])
  }
  # give names to p-values
  names(pVals) <- names(ebolaMotifCount)
  #print the p-value
  relevantMotifs <- names(pVals)
  significantMotifs <- names(pVals[pVals <= p])
  cat("Number of enriched motifs before FDR adjustment = ")
  cat(length(significantMotifs))
  cat("\n")
  cat(head(significantMotifs, 12))
  cat("\n")
  # adjust for FDR
  qValue <- p.adjust(pVals, method = "hochberg")
  names(qValue) <- names(pVals)
  significantMotifsQ <- names(qValue[qValue <= p])
  cat("Number of enriched motifs after FDR adjustment = ")
  cat(length(significantMotifsQ))
  cat("\n")
  cat(head(significantMotifsQ, 12))
  cat("\n")
  return (significantMotifsQ)
}



#Define a function to get the kmer at the position
getKmerAtPosition = function(x, k,argString){
  #subset all the nucleotides from the position to k positions beyond it
  kmerLetters = argString[x+0:(k-1)]
  #paste the letters together
  kmer = paste(kmerLetters,collapse="")
  return(kmer)
}

#get all kmers in argString
getKmerVector=function(k, argString){
  #get all the positions of the string to scan
  positions = 1:(length(argString)-k+1)
  #Apply the getKmerAtPosition function to all positions in the string
  kmers = sapply(positions, FUN = getKmerAtPosition, k,argString)
  #the kmers are the result
  return(kmers)
}

#get a vector of all k-mers of size k
getAllKMersOfSize=function(k){
  #make a vector of all possible Kmers there are 4^k possibilities
  allKmers = rep(0, 4^k) # vector with 4^k zeros
  #initialize the unique kmers all to 1 through k and replace each position with the letter
  initialization = paste(1:k, collapse='')
  #initialize all kmers to this value
  names(allKmers) = rep(initialization,length(allKmers))
  #create an index for each kmer
  ind = 0:(4^k-1)
  #loop over all kmers
  for(ii in 1:k){
    #get the modulus of the index
    modVal = ind%%4
    #if us the modulus of the index to assign a value to position ii in the initialization
    names(allKmers)[modVal == 0] = gsub(ii,"A",names(allKmers)[modVal == 0])
    names(allKmers)[modVal == 1] = gsub(ii,"T",names(allKmers)[modVal == 1])
    names(allKmers)[modVal == 2] = gsub(ii,"G",names(allKmers)[modVal == 2])
    names(allKmers)[modVal == 3] = gsub(ii,"C",names(allKmers)[modVal == 3])
    #divide the indices by 4 to go to the next position
    ind = floor(ind/4)
  }
  return(allKmers)
}

getKmerFreqVector = function(k,genome, allKmers){
  #get the kmer vector for this sample (all kmers in the genome)
  kmerVector = getKmerVector(k, genome)
  #count Kmers from this genome
  kmerFreq = table(kmerVector)
  #map them to all Kmers so they are comparable
  kmerTablePiece = allKmers
  kmerTablePiece[names(kmerFreq)]=kmerFreq
  #return the kmer counts
  return(kmerTablePiece)
}

#get the sample kmer vector for a single kmer size
getSampleForKmer = function(x, k, allKmers){
  # Create a sampled viral genome
  nullGenome = sample(x=bases, size = genomeSize, prob = basePrevalence, replace=TRUE)
  # get their kmer frequencies
  nullKmerTablePiece = getKmerFreqVector(k,nullGenome, allKmers)
  #return kmer frequencies
  return(nullKmerTablePiece)
}

#define a function to get the null distribution of kmers
sampleManyGenomeKmerCounts=function(k){
  #get all possible kmers of this size to map too
  allKmers = getAllKMersOfSize(k)
  #sample 1000 random genomes and count kmers for each
  sampleKmers= sapply(1:1000, FUN = getSampleForKmer, k, allKmers)
  #return the frequency of Kmers in each of these 1000 sample genomes
  return(sampleKmers)
}


