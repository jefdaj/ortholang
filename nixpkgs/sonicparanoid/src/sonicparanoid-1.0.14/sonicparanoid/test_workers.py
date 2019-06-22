#run the main test functions in the workers.py module
import workers

debug = True

workers.info()

# test alignment and alignment and parsing
workers.test_mmseqs_1pass(debug=debug)
