#This script demonstrate how to plan for experiments using two R Libraries
#Ref. Chat-GPT 10/7/2024
#
# Plan Fractional Factorial Design
#
# Install the package if not already installed
# install.packages("FrF2")
library(FrF2)
# Generating a fractional factorial design for 10 factors and 64 runs
design <- FrF2(nruns = 64, nfactors = 10)

# View the design
print(design)
#
# Generate a Plackett-Burman design with specific number of runs and factors
nruns <- 12    # Number of runs (should be a multiple of 4)
nfactors <- 11 # Number of factors

# Generate the design without randomizing
pb_design <- pb(nruns = nruns, nfactors = nfactors, randomize = FALSE)

# Print the design
print(pb_design)
#
# Generating a Plackett-Burman design with 36 runs
#
pb(nruns=36, nfactors = 31)
#
# 
#install daewr as another package
#This function only works for nruns=12 or nruns=20, or nruns=24
#
library(daewr)
PBDes(nruns=12, nfactors=10, randomize=FALSE)
