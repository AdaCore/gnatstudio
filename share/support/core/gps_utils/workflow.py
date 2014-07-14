"""
   Driver execute while generating mono-chain program
   from generators (the parameter), and send execution result
   (how the promise is fullfilled) back to the generator.
"""
import sys


def driver(w):
    def go(gowith=None):
        # before the iteration ends, do:
        try:
            # if there's returned feedback from promise.solve
            # tell the generator about it
            if gowith is not None:
                p = w.send(gowith)
            else:
                # otherwise goto the next step
                p = w.next()

            # if none is get instead of a promise,
            # go ahead
            if p is None:
                go()
            else:
                p.then(go)
        # when hits the end, exit
        except StopIteration:
            # print sys.exc_info()[0]
            return
    go()
