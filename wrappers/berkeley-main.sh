# Part three of the berkeley wrapper scripts.
# This one is used to reserve resources which will be used by the launcher.

salloc --job-name cut1 --time=23:00:00 --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal --nodes=8 bash -i
