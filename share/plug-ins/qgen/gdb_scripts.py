import os
import gdb

watchdog_dict = {}
# context => Qgen_Logpoint
logpoint_dict = {}
# filename => Boolean
processed_logfiles = {}
# number of iterations on the model compute_function
global_log_hit = 1


class Utils:
    separator = None

    @staticmethod
    def write_log_header(filename):
        with open(filename, 'w') as f:
            # The output file contains an html table
            # where each block is associated with its value
            f.write("""<!DOCTYPE html>
            <!--For the links to work this file has to be opened inside\
the Matlab browser-->
            <html>
            <head>
            <meta name="viewport" content="width=device-width,\
 initial-scale=1">
            <style>
            input[type="checkbox"] {
            position: absolute;
            opacity: 0;
            }
            input[type="checkbox"]:focus + label {
            color: black;
            background-color: wheat;
            }
            input[type="checkbox"]:hover + label {
            background-color: wheat;
            }
            label {
            position: relative;
            display: block;
            cursor: pointer;
            background: #c69;
            color: white;
            padding: .5em;
            border-bottom: 1px solid white;
            }

            section {
            height: 0;
            transform: scaleY(0);
            transform-origin: top;
            transition: transform .2s all;
            overflow: hidden;
            }

            input[type=checkbox]:checked + label + section{
            transform: scaleY(1);
            height: auto;
            }

            p {
            padding: 0 2em;
            }

            .togglebox {
            margin: 0 auto;
            width: 50%;
            border: 1px solid #c69;
            }

            table, td, th {
            border: 1px solid #ddd;
            }
            table {
            border-collapse: collapse;
            width: 100%;
            }
            th, td {
            text-align: left;
            padding: 8px;
            }
            tr:nth-child(even){background-color: #f2f2f2}
            th {
            background-color: #ff9933;
            color: black;
            }
            </style>
            </head>
            <body>
""")

    @staticmethod
    def write_log_footer(filename):
        with open(filename, 'a') as f:
            f.write("""
            </body>
            </html>""")

    @staticmethod
    def set_variable(var, val):
        """
        Sets a variable to a given value
        :param string var: the name of the variable to set
        :param string val: the desired value
        """

        if not Utils.separator:
            lang = gdb.execute("show language", to_string=True).rsplit(
                "\"")[-2].rsplit(" ", 1)[-1]
            if lang == "ada":
                Utils.separator = ":="
            else:
                Utils.separator = "="

        gdb.execute("set variable %s %s %s" % (var, Utils.separator, val))


class Watchpoint_Add (gdb.Command):
    """
    QGen Debugger watchpoint handling command called qgen_watchpoint
    """

    def __init__(self):
        super(Watchpoint_Add, self).__init__(
            "qgen_watchpoint", gdb.COMMAND_NONE
        )

    def invoke(self, args, from_tty):
        """
        Takes a variable name and desired value.
        The command will set a watchpoint on the variable name.
        """
        args = gdb.string_to_argv(args)
        symbol = args[0].split('/')  # Remove the "context/" part
        context = symbol[0]
        symbol = symbol[-1].strip()
        if len(args) == 2:
            # If there is not watchdog for that context, create one
            if not watchdog_dict.get(context):
                watchdog_dict[context] = Watchpoint_Watchdog(
                    context, gdb.BP_BREAKPOINT)

            wp = watchdog_dict[context].watchpoint_dict.get(symbol)
            if not wp:
                try:
                    wp = Qgen_Watchpoint(
                        symbol, args[1], gdb.BP_WATCHPOINT
                    )
                    watchdog_dict[context].watchpoint_dict[symbol] = wp
                    Utils.set_variable(symbol, args[1])
                except RuntimeError:
                    # If the current scope is not the context one
                    # store the value to set the watchpoint later
                    watchdog_dict[context].watchpoint_dict[symbol] = args[1]
            else:
                # If the watchpoint already exists just update the value
                wp[0].value = args[1]
                Utils.set_variable(symbol, args[1])

Watchpoint_Add()


class Watchpoint_Delete (gdb.Command):

    def __init__(self):
        super(Watchpoint_Delete, self).__init__(
            "qgen_delete_watchpoint", gdb.COMMAND_NONE
        )

    def invoke(self, args, from_tty):

        args = gdb.string_to_argv(args)

        if len(args) == 1:
            symbol = args[0].split('/')  # Remove the "context/" part
            context = symbol[0]
            symbol = symbol[-1].strip()

            if watchdog_dict.get(context):
                wp = watchdog_dict[context].watchpoint_dict.get(symbol)
                if wp:
                    del watchdog_dict[context].watchpoint_dict[symbol]
                    gdb.execute("delete %s" % wp.number)
                    # Delete watchdog breakpoint if there is no more watchpoint
                    # associated to it
                    if not watchdog_dict[context].watchpoint_dict:
                        gdb.execute(
                            "delete %s" % watchdog_dict[context].number)
                        del watchdog_dict[context]

Watchpoint_Delete()


class Qgen_Delete_Logpoint(gdb.Command):

    def __init__(self):
        super(Qgen_Delete_Logpoint, self).__init__(
            "qgen_delete_logpoint", gdb.COMMAND_NONE
        )

    def invoke(self, args, from_tty):
        # Args are symbol and context
        args = gdb.string_to_argv(args)
        symbol = args[0]
        context = args[1]
        bp = logpoint_dict.get(context, None)

        if bp and bp.symbols.get(symbol) is not None:
            del bp.symbols[symbol]

Qgen_Delete_Logpoint()


class Qgen_Set_Logpoint(gdb.Command):

    def __init__(self):
        super(Qgen_Set_Logpoint, self).__init__(
            "qgen_logpoint", gdb.COMMAND_NONE
        )

    def invoke(self, args, from_tty):
        # Args are symbol, context, blockname, filename, file:line
        # and model_name
        args = gdb.string_to_argv(args)
        symbol = args[0]
        context = args[1]
        gdb.write("Adding %s \n" % context)
        if logpoint_dict.get(context):
            bp = logpoint_dict[context]
        else:
            bp = Qgen_Logpoint(args[4], gdb.BP_BREAKPOINT)
            logpoint_dict[context] = bp

        # We remove the dataport part from the blockname because
        # it is not traceable in Matlab
        # symbol => (blockname, filename, model_name)
        bp.symbols[symbol] = (args[2].rsplit('/', 1)[0], args[3], args[5])

Qgen_Set_Logpoint()


class Watchpoint_Watchdog (gdb.Breakpoint):

    def __init__(self, spec, ty):
        super(Watchpoint_Watchdog, self).__init__(spec, ty, internal=True)
        self.watchpoint_dict = {}

    def stop(self):
        for symbol, wp in self.watchpoint_dict.iteritems():
            # The watchpoint was never added because not in the scope,
            # and is just the value to set instead
            if wp.__class__ != Qgen_Watchpoint:
                del self.watchpoint_dict[symbol]
                wp = Qgen_Watchpoint(symbol, wp, gdb.BP_WATCHPOINT)
                self.watchpoint_dict[symbol] = wp

            # The watchpoint has been deleted, add it back
            else:
                self.watchpoint_dict[symbol].delete()
                del self.watchpoint_dict[symbol]
                self.watchpoint_dict[symbol] = Qgen_Watchpoint(
                    symbol, wp.value, gdb.BP_WATCHPOINT)

            # A watchpoint will not trigger if the value written
            # is equal to its previous value.
            # By forcing the value at the beginning
            # of the scope we ensure that the desired value will be set
            # or force the watchpoint to trigger
            Utils.set_variable(symbol, wp.value)
        return False


class Qgen_Logpoint (gdb.Breakpoint):

    def __init__(self, spec, ty):
        super(Qgen_Logpoint, self).__init__(spec, ty, internal=True)
        # A dict association a symbol to a (blockname, filename, model_name)
        self.symbols = {}
        self.hit = 0

    def stop(self):
        logfiles_hit = []
        copy_logfiles = processed_logfiles.copy()
        self.hit += 1
        global global_log_hit
        # If we visited a log breakpoint twice we need to end the table
        if self.hit > 1 and self.hit > global_log_hit:
            global_log_hit = self.hit
            for filename in copy_logfiles.iterkeys():
                processed_logfiles[filename] = False
                with open(filename, 'a') as f:
                    f.write("""         </tbody>
            </table>
          </section>
""")

        for symbol, (blockname,
                     filename, model_name) in self.symbols.iteritems():
            if not os.path.exists(filename):
                Utils.write_log_header(filename)

            with open(filename, 'a') as f:
                open_table = processed_logfiles.get(filename, False)
                if not open_table:
                    f.write("""
<div class="togglelist">
  <input id="toggle%d" type="checkbox" name="toggle" />
  <label for="toggle%d">Iteration %d</label>
  <section id=:"content%d">
            <table>
            <thead>
            <tr>
            <th>Block</th>
            <th>Value</th>
            </tr>
            </thead>
            <tbody>""" % (global_log_hit, global_log_hit,
                          global_log_hit, global_log_hit))
                if filename not in logfiles_hit:
                    processed_logfiles[filename] = True
                    logfiles_hit.append(filename)
                f.write("""<tr>
                    <td><a href="matlab:open_system('%s');\
 hilite_system('%s')">%s</a></td>
                    <td>%s</td>
                    </tr>
                    """ % (model_name, blockname, blockname,
                           gdb.execute(
                               "p %s" % symbol, to_string=True).rsplit(
                                   '= ', 1)[-1].split()[0]))
                # We split to get right side of the value and split again in
                # case gdb displays more information (e.g. language changed)
        return False


class Qgen_Watchpoint (gdb.Breakpoint):

    def __init__(self, spec, value, ty):
        super(Qgen_Watchpoint, self).__init__(spec, ty, internal=True)
        self.var = spec
        self.value = value

    def stop(self):
        try:
            Utils.set_variable(self.var, self.value)
            gdb.write("{0} set to {1}\n".format(self.var, self.value))
        except:
            pass
        return False
