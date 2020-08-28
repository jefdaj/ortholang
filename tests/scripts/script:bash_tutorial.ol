# custom-bash-script.sh expects a str, num, num.list, and str.list in that
# order, and returns a text description of what was in its arguments.
#
# There's nothing in particular you can do with this type of text in OrthoLang,
# other than return it in the main result.

s = "a string"
n = 1.4e7
nl = [1,2,3,100]
sl = ["another string", "and another", "and a third"]

txt = run_script "examples/scripts/custom-bash-script.sh" [s, n, nl, sl]

result = txt
