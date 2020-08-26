lol = [[1, 2, 3], [3, 4, 5], [5, 6, 7]]
mvd = load_script "my-r-script.R"
result = run_script mvd [lol]
