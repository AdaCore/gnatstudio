import re


class Diagram_Utils(object):

    # Using regexp lookarounds to assert that the previous or next
    # character is not a '/'
    slash_split = re.compile("(?<!/)/(?!/)")

    @staticmethod
    def block_split(s, count=0, backward=False):
        """
        Splits a block name in the hilite format whithout spliting
        escaped slashes which are doubled
        :param string s: The block name to split
        :param integer count: The number of splits to realize
        :param boolean backward: The direction of the split
        """

        res = re.split(Diagram_Utils.slash_split, s)

        # Merge the strings after count if necessary
        length = len(res)

        if count != 0 and count < length:
            if backward:
                last = length - count
                res[0:last] = ['/'.join(res[0:last])]
            else:
                res[count:length] = ['/'.join(res[count:length])]

        return res

    @staticmethod
    def highlight_breakpoints(viewer, diag, map,
                              bp_blocks, previous_breakpoints):
        for b in previous_breakpoints:
            Diagram_Utils.set_block_style(
                viewer, diag, map, b, bp_blocks, 'style_if_breakpoint')

    @staticmethod
    def update_signal_style(signal_attributes, viewer, diag):
        for itid, sig in signal_attributes.iteritems():
            item = diag.get_item(itid)
            if item:
                label = getattr(item, "label", None)
                sig.compute_style()
                if sig.style is not None:
                    viewer.diags.set_item_style(
                        label, label.data.get(
                            'style_if' + sig.style))
                else:
                    viewer.diags.set_item_style(label, None)

    @staticmethod
    def set_block_style(viewer, diag, map, bp, bp_blocks, style=None):
        """
        Update the style field of an item to the given style name
        or reset it if no style is supplied
        :param bp: a DebuggerBreakpoint object
        :param style: a string representing the key containing
        the style to set
        """
        if bp.type == "breakpoint":
            blockid = map.get_block(file=bp.file, line=bp.line)
            item = diag.get_item(blockid)
            if item:
                for it in item.recurse():
                    try:
                        if style:
                            bp_blocks.append(it)
                            prio = it.data.get('priority')
                            if prio is not None and prio > 0 \
                               and prio < diag.current_priority:
                                viewer.diags.set_item_style(
                                    it, it.data.get(
                                        style + '_processed')
                                )
                            else:
                                viewer.diags.set_item_style(
                                    it, it.data.get(style)
                                )
                        else:
                            viewer.diags.set_item_style(it, None)
                    except:
                        # No corresponding style defined
                        pass

    @staticmethod
    def forall_auto_items(diagrams):
        """
        Return the list of all items with an "auto" property (i.e. whose
        value should be displayed in the browser).
        :param diagrams: a sequence of `GPS.Browsers.Diagram`
        :return: a sequence of tuples (diagram, toplevel_item, item)
        """

        if diagrams:
            for d in diagrams:
                for item in d.items:
                    for it in item.recurse():
                        if hasattr(it, "data") and \
                           it.data.get('auto') == "true":
                            yield (d, item, it)

    @staticmethod
    def update_priority_style(viewer, diagrams, exclude_list=[]):
        """
        Browses all items and updates their style when they have a lower
        priority than their containing diagram, meaning they were
        processed.
        :param viewer: a QGEN_Diagram_Viewer
        :param diagrams: a list of QGEN_Diagram
        :param exclude_list: a list of items that do not have to be
        processed
        """
        if diagrams:
            for d in diagrams:
                for item in d.items:
                    for it in item.recurse():
                        if it not in exclude_list:
                            if hasattr(it, "data"):
                                prio = it.data.get('priority')
                                if prio is not None:
                                    if prio > 0\
                                       and prio < d.current_priority:
                                        # If the current block is
                                        # processed, fade its colors by
                                        # updating the style
                                        viewer.diags.set_item_style(
                                            it, it.data.get(
                                                'style_if_processed'))
                                    elif d.reset_priority:
                                        viewer.diags.set_item_style(
                                            it, None)
                d.reset_priority = False
