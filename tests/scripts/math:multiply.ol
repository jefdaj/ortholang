# This is how multiplication would normally look for a user:
r1 = 2 * 3.5

# But it's implemented by transforming the binary operator to this:
r2 = multiply [2, 3.5]

# This should be [], meaning both versions come up with 7:
result = [7] ~ [r1, r2]
