"""
   Driver execute while generating mono-chain program
   from generators (the parameter), and send execution result
   (how the promise is fullfilled) back to the generator.
"""

import sys
import GPS


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


# The following are decorators for workflows(generators)
def make_action_from_workflow(name, category="General",
                              description="", criteria=None):
    """
       Decorator that wrap a workflow and generate an action

       name: string, name of the returning action
       category: category as action object required
       description: string, description of the result action
       criteria: function that returns boolean, workflow is executed only when
                 it returns True

       The input of the decorator = expecting argument for returning wrap
       = a workflow = a generator that can be driven by driver()
    """
    # the wrapper function to be returned by decorator
    def wrap(workflow):
        # create action with given name
        action = GPS.Action(name)

        def drive():
            if (criteria is None) or criteria():
                w = workflow()
                driver(w)
            else:
                GPS.Console("Messages").write("Criteria doesn't meet.\n")

        # modify the action
        action.create(drive, "", category, description)

        return action

    return wrap


def make_button_for_action(button_name, button_id):
    """
       Decorator that wraps an action and make a button for it on the toolbar

       button_name: name of the button
       button_id: id of the button (kept by GPS)

       The input of the decorator = expecting argument for returning wrap
       = an action
    """
    def wrap(action):
        # create on_click function for the button from the action
        def on_click(button):
            action.execute_if_possible()

        # create the button and append it to the toolbar
        b = GPS.Button(button_id, button_name, on_click)
        GPS.Toolbar().append(b)

        return action

    return wrap
