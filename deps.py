import re
import os
import sys
import gv
import os, os.path
import getopt
import argparse

# Import pygraph
from pygraph.classes.graph import graph
from pygraph.classes.digraph import digraph
from pygraph.algorithms.searching import breadth_first_search
from pygraph.readwrite.dot import write

gr = graph()

pruned = ['node_modules']

deps={}

def prune(path):
    fields = path.split('/')
    for item in pruned:
        if item in fields:
            return True
    return False


def analyze(path):
    path_fields = path.split('/')
    nodename = "%s/%s" % (path_fields[-2], path_fields[-1])

    f = open(path, 'r')
    f_str = f.read()
    f.close()

    gr.add_node(nodename)

    matched = re.findall('[\w\.]+\s*=\s*require\s*"((models|controllers)\/\w+)"', f_str)

    for item in matched:
        try:
            deps[nodename].append(item[0])
        except KeyError:
            deps[nodename] = []
            deps[nodename].append(item[0])


# See http://docs.python.org/2/library/argparse.html#module-argparse
# or getopts
def main(argv):
    outputfile = "deps.png"

    parser = argparse.ArgumentParser(description='Find coffeescript dependencies')
    parser.add_argument('dirs', metavar='dir', nargs='+',
                        help='directories to process')

    parser.add_argument('-o', '--output', help='output file')
    parser.add_argument('-t', action='store_true', help='show dependencies in text mode')

    args = parser.parse_args()

    if args.output is not None:
        outputfile = args.output

    for root, dirs, files in os.walk(args.dirs[0]):
        for f in files:
            fullpath = os.path.join(root, f)
            if os.path.splitext(fullpath)[1] == '.coffee':
                if not prune(fullpath):
                    analyze(fullpath)


    for item in deps:
        for node in deps[item]:
            gr.add_edge((item, "%s.coffee" % (node)))


    if args.t:
        for item in deps:
            print item
            for node in deps[item]:
                print "\t %s.coffee" % (node)



    # Draw as PNG
    dot = write(gr)
    gvv = gv.readstring(dot)
    gv.layout(gvv,'dot')
    gv.render(gvv,'png', outputfile)


if __name__ == "__main__":
    main(sys.argv[1:])
