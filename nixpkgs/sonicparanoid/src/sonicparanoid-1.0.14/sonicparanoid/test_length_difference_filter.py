#run the main test functions in the length_difference_filter.py module
import sys
import length_difference_filter as lendiffilter

### MAIN ###
def main():
    debug = True
    # print module info
    lendiffilter.info()

    # check difference length calculation
    #lendiffilter.test_length_difference_check(debug)

    # extract paralogs
    lendiffilter.test_extract_paralogs_single_core(debug)

if __name__ == "__main__":
    main()
