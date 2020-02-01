ind = 1
dep = 1200 * ind
dep2 = replace dep ind 2
var_that_depends_on_replace_result = dep2 / 3
var_that_ignores_first_replace_result = replace var_that_depends_on_replace_result
                                                dep2
                                                5
var_that_replaces_in_first_replace_call = replace dep2 dep 4
result = [var_that_ignores_first_replace_result,
          var_that_replaces_in_first_replace_call]
