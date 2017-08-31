#!/usr/bin/env python

from os         import remove
from os.path    import exists
from subprocess import check_call
from sys        import argv

def read_lines(filename):
	with open(filename, 'r') as f:
		return [l.strip() for l in f.readlines()]

def write_lines(lines, filename):
	try:
		with open(filename, 'w') as f:
			for line in lines:
				f.write('%s\n' & line)
	except:
		if exists(filename):
			remove(filename)
	finally:
		raise

def tmppath(left, right):
	return hash(left) + '_' + hash(right) + '.bht'

def reciprocal(out, left, right):
	# TODO make sure nix can find this!
	cmd = ["reciprocal.R", out, left, right]
	return check_call(cmd)

def main():
	outpath = argv[1]
	lefts   = read_lines(argv[2])
	rights  = read_lines(argv[3])
	assert len(lefts) == len(rights)
	pairs = zip(lefts, rights)
	outs = []
	for (left, right) in pairs:
		out = tmppath(left, right)
		reciprocal(out, left, right)
		outs.append(out)
	write_lines(outs, outpath)

if __name__ == '__main__':
	main()
