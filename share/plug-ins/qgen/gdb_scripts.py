watchdog_dict = {}


class Utils:
    separator = None

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
        Takes an optional watchpoint number, variable name and desired value
        If there is no watchpoint number the command will set a watchpoint on
        the variable name and print the number of the watchpoint set
        """
        args = gdb.string_to_argv(args)
        symbol = args[0].split('/')  # Remove the "context/" part
        context = symbol[0]
        symbol = symbol[-1].strip()

        if len(args) == 2:
            if not watchdog_dict.get(context):
                watchdog_dict[context] = (Watchpoint_Watchdog(
                    context, gdb.BP_BREAKPOINT
                ), 0)

            wp = watchdog_dict[context][0].watchpoint_dict.get(symbol)
            if not wp:
                try:
                    wp = Qgen_Watchpoint(
                        symbol, args[1], gdb.BP_WATCHPOINT
                    )
                    watchdog_dict[context][0].watchpoint_dict[symbol] = (wp, 0)
                    Utils.set_variable(symbol, args[1])
                except RuntimeError:
                    watchdog_dict[context][0].watchpoint_dict[symbol] \
                        = (args[1], 0)
            else:
                wp[0].value = args[1]

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
                wp = watchdog_dict[context][0].watchpoint_dict.get(symbol)[0]
                if wp:
                    del watchdog_dict[context][0].watchpoint_dict[symbol]
                    gdb.execute("delete %s" % wp.number)
                    # Delete watchdog breakpoint if there is no more watchpoint
                    # associated to it
                    if not watchdog_dict[context][0].watchpoint_dict:
                        gdb.execute(
                            "delete %s" % watchdog_dict[context][0].number
                        )
                        del watchdog_dict[context]

Watchpoint_Delete()


class Watchpoint_Action (gdb.Command):

    def __init__(self):
        super(Watchpoint_Action, self).__init__(
            "qgen_breakpoint_action", gdb.COMMAND_NONE
            )

    def invoke(self, args, from_tty):
        for context, (bp, hit) in watchdog_dict.iteritems():
            if bp.hit_count > hit:
                watchdog_dict[context] = (bp, bp.hit_count)
                for symbol, (wp, whit) in bp.watchpoint_dict.iteritems():

                    # The watchpoint was never added because not in the scope,
                    # add it now
                    if wp.__class__ != Qgen_Watchpoint:
                        value = wp
                        del bp.watchpoint_dict[symbol]
                        bp.watchpoint_dict[symbol] = (Qgen_Watchpoint(
                            symbol, value, gdb.BP_WATCHPOINT
                        ), 0)

                    # The watchpoint has been deleted, add it back
                    elif not wp.is_valid():
                        del bp.watchpoint_dict[symbol]
                        bp.watchpoint_dict[symbol] = (Qgen_Watchpoint(
                            symbol, wp.value, gdb.BP_WATCHPOINT
                        ), 0)

            for symbol, (wp, whit) in bp.watchpoint_dict.iteritems():
                if wp.hit_count > whit:
                    bp.watchpoint_dict[symbol] = (wp, wp.hit_count)
                    Utils.set_variable(wp.var, wp.value)

Watchpoint_Action()


class Watchpoint_Watchdog (gdb.Breakpoint):

    def __init__(self, spec, ty):
        super(Watchpoint_Watchdog, self).__init__(spec, ty)
        self.watchpoint_dict = {}


class Qgen_Watchpoint (gdb.Breakpoint):

    def __init__(self, spec, value, ty):
        super(Qgen_Watchpoint, self).__init__(spec, ty)
        self.var = spec
        self.value = value
