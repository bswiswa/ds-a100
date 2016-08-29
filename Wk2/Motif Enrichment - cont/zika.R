# make sure zika.R and zikaGenome.RData are in the current working directory

#Load the genome for the homework
load(file = "zikaGenome.RData")

# size of the viral genome
genomeSize = 10000
# the 4 possible bases found in the genome
bases = c("A","T","G","C")
# The rate at which these 4 bases occur in nature for viruses
basePrevalence = c(0.3, 0.4, 0.1, 0.2)

#List of motifs.
motifs = list(
  "AATCG", 
  "TTGCA", 
  c("TTACC","TTTCC","TTGCC","TTCCC"), 
  "CCGTA",
  c("ATACGA","ATTCGA","ATGCGA","ATCCGA"),
  "AATGCC"
)

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

#get the motif
motif = motifs[[1]]
#get the length of the motif
k = nchar(motif[1])
#get the kmers from the zika genome
zikaKmerVector = getKmerVector(k,genome)
#get the counts
zikaKmerCounts = table(zikaKmerVector)
#Get the count for this motif
zikaMotifCount = sum(zikaKmerCounts[motif])
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

getKmerFreqVector = function(genome, allKmers){
  #get the kmer vector for this sample (all kmers in the genome)
  kmerVector = getKmerVector(k,genome)
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
  nullKmerTablePiece = getKmerFreqVector(nullGenome, allKmers)
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

#calculate pvalues for each motif
#reserve a pValue vector for storing the result
pVals = rep(-1, length(motifs))
#add names to the vector corresponding to its motif
names(pVals) = c("AATCG", "TTGCA","TTNCC","CCGTA","ATNCGA","AATGCC")
#for each motif
for(ii in 1:length(motifs)){
  #test motif
  motif = motifs[[ii]]
  #get the length of the motif
  k = nchar(motif[1])
  #get the null distribution by sampling random genomes
  nullSamples = sampleManyGenomeKmerCounts(k)
  #select the kmers for the motif
  motifNullSamples = colSums(nullSamples[motif,,drop=FALSE])
  #get the distribution Probability Mass Function (pmf) PDF if continuous for the null for this motif
  pmf=table(motifNullSamples)/length(motifNullSamples)
  #get the kmer vector
  allKmers = getAllKMersOfSize(k)
  #count the kmers in the zika virus
  zikaKmerCounts = getKmerFreqVector(genome,allKmers)
  #Get the count for this motif
  zikaMotifCount = sum(zikaKmerCounts[motif])
  #get the probability of observing a value at least this extreme by random chance
  pVals[ii] = sum(pmf[as.numeric(names(pmf))>=zikaMotifCount])	
  #print the p-value
  print(pVals[ii])
}

png("pmf.png")
plot(pmf)
dev.off()
