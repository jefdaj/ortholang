#run the main test functions in the workers.py module
import mmseqs_parser_cython as parser

debug = True


parser.test_parser(debug)


# test alignment and alignment and parsing
#print(mmseqs_parser_c.length_difference_check(100, 80, 0.25))
#mmseqs_parser_c.length_difference_check_c(100, 25, 0.25)
#print(mmseqs_parser_c.length_difference_check(75, 85, 0.25))
