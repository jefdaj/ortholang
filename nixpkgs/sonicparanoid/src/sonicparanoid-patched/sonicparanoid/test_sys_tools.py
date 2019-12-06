#run the main test functions in the sys_tools.py module
import sys
import sys_tools as systools

debug = True

systools.info()

#check file format
#systools.test_checkFormat(debug)

#test conversion of html to text file
#systools.test_html2text(debug)

#test chopping of strings
#systools.test_chopString(debug)

#test files copy
#systools.test_copy(debug)

#get shell
#systools.test_getShell()

#unzip
#systools.test_unzip(debug)

#untar
#systools.test_untar(debug)

# bunzip2
#systools.test_bunzip2(debug)

# bzip2
#systools.test_bzip2(debug)

#diff
#systools.test_diff(debug)

#elapsed time
#systools.test_getElapsedTime(debug=debug)
