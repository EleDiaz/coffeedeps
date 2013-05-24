import re
import os
import sys
import gv
import os, os.path
import getopt

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
    #    print "Adding %s" % (nodename)
    gr.add_node(nodename)

    matched = re.findall('[\w\.]+\s*=\s*require\s*"((models|controllers)\/\w+)"', f_str)

    for item in matched:
        try:
            deps[nodename].append(item)
        except KeyError:
            deps[nodename] = []
            deps[nodename].append(item)

# See http://docs.python.org/2/library/argparse.html#module-argparse
# or getopts
def main(argv):
    outputfile = "deps.png"

    for root, dirs, files in os.walk(sys.argv[1]):
        for f in files:
            fullpath = os.path.join(root, f)
            if os.path.splitext(fullpath)[1] == '.coffee':
                if not prune(fullpath):
                    analyze(fullpath)

    for item in deps:
        #        print item
        for node in deps[item]:
            # print node[0]
            # print "\t %s.coffee" % (node[0])
            gr.add_edge((item, "%s.coffee" % (node[0])))
            #        print


    # Draw as PNG
    dot = write(gr)
    gvv = gv.readstring(dot)
    gv.layout(gvv,'dot')
    gv.render(gvv,'png', outputfile)


if __name__ == "__main__":
    main(sys.argv[1:])
