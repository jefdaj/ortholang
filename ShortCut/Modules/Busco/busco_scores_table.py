#!/usr/bin/env python3

# TODO fromGeneric paths before calling
# TODO need to do a separate .out file for consistency?
# TODO any need to store the lineage HMMs used here too?
# TODO check that we fixEmpties before calling this
# TODO put back the header?

from sys import argv
from re  import findall
from os.path import basename

def read_bur(burpath):
    with open(burpath, 'r') as f:
        txt = f.read()
    fasta  = basename(findall('for file (.*)\n', txt)[0])
    scores = findall(':([0-9\.]{1,5})', txt)
    return (fasta, scores)

def write_table(outpath, lines):
    with open(outpath, 'w') as f:
        if len(lines) == 0:
            f.write('<<emptybut>>')
        else:
            # f.write('\t'.join(('fasta', 'C', 'S', 'D', 'F', 'M', 'n')) + '\n')
            for burpath, scores in lines:
                f.write('\t'.join([burpath] + scores) + '\n')

def main(outpath, burpaths):
    lines = []
    for burpath in burpaths:
        line = read_bur(burpath)
        lines.append(line)
    write_table(outpath, lines)

if __name__ == '__main__':
    outpath = argv[1]
    burpaths = argv[2:]
    main(outpath, burpaths)
