# This runs custom-r-venn.R on several lists of lists to demonstrate how named
# variables can be used to label plots. See also r_filter.ol for a simpler
# example that ignores names and just filters a table of BLAST hits.

r = "examples/scripts/custom-r-venn.R"

# named list of unnamed args
# should call them 'input1', 'input2', 'input3'
lol = [[1, 2, 3], [3, 4, 5], [5, 6, 7]]
plot1 = run_script r lol

# named list of named args
# should call them 's1', 's2', 'genomes', 's4'
s1 = [1,2,3,4,5]
s2 = [2,3,4]
genomes = [5,6,7]
s4 = [1,6,7]
lol2 = [s1, s2, genomes, s4]
plot2 = run_script r lol2

# unnamed list of named args
# should call them 's1', 's2', 'genomes'
plot3 = run_script r [s1, s2, genomes]

# these plots are still untyped; ortholang has no idea they should be .png files
# you'll need to rename them yourself from .untyped -> .png to open them
result = [plot1, plot2, plot3]
