# TODO these should be equivalent, right? (except for names)

lol = [[1, 2, 3], [3, 4, 5], [5, 6, 7]]
plot1 = run_script "my-r-script.R" lol

s1 = [1,2,3,4,5]
s2 = [2,3,4]
s3 = [5,6,7]
lol2 = [s1, s2, s3]
plot2 = run_script "my-r-script.R" lol2

result = plot2
