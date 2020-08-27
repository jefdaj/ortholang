# named list of unnamed args
# should call them 'input1', 'input2', 'input3'
# broken so far
lol = [[1, 2, 3], [3, 4, 5], [5, 6, 7]]
plot1 = run_script "my-r-script.R" lol

# named list of named args
# should call them 's1', 's2', 's3', 's4'
# broken so far
s1 = [1,2,3,4,5]
s2 = [2,3,4]
genomes = [5,6,7]
s4 = [1,6,7]
lol2 = [s1, s2, genomes, s4]
plot2 = run_script "my-r-script.R" lol2

# unnamed list of named args
# should call them 's1', 's2', 'genomes'
# this one works
plot3 = run_script "my-r-script.R" [s1, s2, genomes, s4]

result = [plot1, plot2, plot3]
