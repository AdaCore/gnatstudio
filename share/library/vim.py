"""
This plugin provides a Vim emulation layer for GPS.
It does so outside of the GPS keybindings facility, which means
that it won't affect your regular keybindings.

This plugin provides a very small subset of the facilities of vim
for the moment. What works :

- Insertion mode
- Basic commands (word and line movement, deletions, replace)
- Visual mode (both line and character)

Notable features that are not yet implemented:

- Visual block mode
- Repeats
- Macros
- Text objects
- Paragraph and sentences moves
- Much much more
"""

# TODO:
# * Commands : Sentences and paragraphs movements {}()
# * Make w and b work as in vim  OK
# * Goto line
# * Repeats
# * Bind "/" to isearch or implement own search
# * Bind "*" to search whole word

from gi.repository import Gtk
import GPS
from pygps import get_widgets_by_type, send_key_event
from collections import defaultdict
from functools import partial
import re
# UTILS #


def qualifier_decorator(_type):

    def decorator(klass):
        def _action_type(self):
            return _type
        klass.qualifiers = klass.qualifiers.union([_type])
        return klass

    decorator._type = _type

    return decorator


movement_command = qualifier_decorator("movement_command")
composed_movement = qualifier_decorator("composed_movement")
write_command = qualifier_decorator("write_command")
insert_command = qualifier_decorator("insert_command")


def with_col(loc, col):
    return GPS.EditorLocation(loc.buffer(), loc.line(), col)


def forward_vim_word(loc, fwd=True):

    loc = loc if fwd else loc.forward_char(-1)

    def isword(c):
        return c == '_' or c.isalpha()

    def cat(c):
        if isword(c):
            return 1
        elif c.isspace():
            return 2
        else:
            return 3

    offset = 1 if fwd else -1
    fw_ws = partial(forward_until, pred=lambda c: not c.isspace(),
                    backwards=not fwd)

    if loc.get_char().isspace():
        if fwd:
            return fw_ws(loc)
        loc = fw_ws(loc)

    c = cat(loc.get_char())
    while c == cat(loc.get_char()):
        loc = loc.forward_char(offset)

    return fw_ws(loc) if fwd else loc.forward_char(1)


GPS.EditorLocation.forward_vim_word = forward_vim_word


def forward_until(loc, pred,
                  skip_first_char=False,
                  stop_at_eol=False,
                  backwards=False):
    step = -1 if backwards else 1
    cur_loc = loc

    if skip_first_char:
        cur_loc = cur_loc.forward_char(step)

    while not pred(cur_loc.get_char()):
        if cur_loc.get_char() == "\n" and stop_at_eol:
            return loc

        if cur_loc == cur_loc.forward_char(step):
            return loc

        cur_loc = cur_loc.forward_char(step)
    return cur_loc


def get_visual_locs(vim_state):
    return [(vim_state.visual_start_pos, vim_state.view.cursor())]


def get_visual_box_locs(vim_state):
    pass


def get_visual_line_locs(vim_state):
    cur = vim_state.view.cursor()
    if vim_state.visual_start_pos > cur:
        a = cur.beginning_of_line()
        b = vim_state.visual_start_pos.end_of_line()
    else:
        a = vim_state.visual_start_pos.beginning_of_line()
        b = cur.end_of_line()
    return [(a, b)]


GPS.EditorLocation.with_col = with_col
GPS.EditorLocation.forward_until = forward_until


# CONSTANTS #

KEY_ESC = 65307

NormalState = 0
InsertState = 1
ReplaceState = 12
VisualState = 2
VisualStateLine = 3
VisualStateBox = 4

visual_state_locs_funcs = {
    VisualState: get_visual_locs,
    VisualStateLine: get_visual_line_locs,
    VisualStateBox: get_visual_box_locs
}

Mov_Char = 0
Mov_Line = 1
Mov_Word = 2


def create_action(action_tuple, vim_state):

    if isinstance(action_tuple, tuple):
        args = action_tuple[1:]
        args_prepared = [create_action(arg, vim_state)
                         if isinstance(arg, tuple) else arg
                         for arg in args]
        inst = action_tuple[0](*args_prepared)
    else:
        inst = action_tuple()

    inst.vim_state = vim_state

    if inst.is_a(insert_command):
        inst.key_presses = []

    return inst


class KeyMap(object):

    def get_action(keyval, vim_state):
        return NoAction()


class CharActionKeyMap(KeyMap):

    def __init__(self, keymap):
        self.keymap = keymap

    def get_action(self, keyval, vim_state):
        action = self.keymap.get(chr(keyval), (NoAction,))
        return create_action(action, vim_state)


class CharCharKeyMap(KeyMap):

    def get_action(self, keyval, vim_state):
        return create_action((CharAction, chr(keyval)), vim_state)


def is_command_state(state):
    return state in [
        NormalState, VisualState, VisualStateLine, VisualStateBox
    ]


def is_visual_state(state):
    return is_visual_state(state) and state != NormalState


def is_insert_state(state):
    return state in [InsertState, ReplaceState]


override_keys_map = {
    65106: 94
}


class VimState(object):

    def __init__(self, view, gtk_view):
        self.view = view
        self.buffer = view.buffer()
        self.selection_overlay = self.buffer.create_overlay(
            name="vim_selection_overlay"
        )
        self.selection_overlay.set_property(
            "background", "#777"
        )
        self.gtk_view = gtk_view
        self.state = NormalState
        self.column_memory = None
        self.action_stack = []
        self.keymaps_stack = [CharActionKeyMap(basic_actions)]
        self.last_write_command = None
        self.last_movement_command = None
        self.current_insert_command = None

        switch_state(self, NormalState)

    def yank(self, beginning, end, is_line=False):
        yanks[current_yank_buffer] = (self.buffer.get_chars(beginning, end),
                                      is_line)

    def delete(self, start, end, is_line=False):
        self.yank(start, end, is_line)
        self.buffer.delete(start, end)

    def get_selection_locs(self):
        return visual_state_locs_funcs[self.state](self)

    def extend_selection(self):
        locs_pairs = self.get_selection_locs()
        for loc_a, loc_b in locs_pairs:
            self.buffer.remove_overlay(self.selection_overlay)
            self.buffer.apply_overlay(self.selection_overlay, loc_a, loc_b)

    def init_visual(self):
        self.visual_start_pos = self.view.cursor()
        self.extend_selection()

    def is_in_command_state(self):
        return is_command_state(self.state)

    def on_key_pressed(self, view, event):
        _, key = event.get_keyval()

        if key in override_keys_map:
            key = override_keys_map[key]

        # Escape always returns back to normal mode
        if key == KEY_ESC:
            self.action_stack = []
            self.current_insert_command = None
            switch_state(self, NormalState)

        # Bind actions if we are in normal mode
        elif self.is_in_command_state():
            if key in range(256):
                action = self.keymaps_stack[-1].get_action(key, self)
                remember_action = action.apply()
                if remember_action:
                    if remember_action.is_a(composed_movement):
                        self.last_movement_command = remember_action
                    elif remember_action.is_a(write_command)\
                            or remember_action.is_a(insert_command):
                        self.last_write_command = remember_action
                    if remember_action.is_a(insert_command):
                        self.current_insert_command = remember_action

            # Extend the state's selection if we are in visual
            if self.state in [VisualState, VisualStateLine, VisualStateBox]:
                self.extend_selection()

            return True

        elif is_insert_state(self.state):
            if self.current_insert_command:
                self.current_insert_command.key_presses.append(
                    (key, event.hardware_keycode, event.string)
                )
                self.last_ev = event

            return False

    def column(self):
        if self.column_memory:
            return self.column_memory
        else:
            return self.view.cursor().column()

    def set_column_memory(self):
        self.column_memory = self.view.cursor().column()


yanks = {}
current_yank_buffer = "default_yank_buffer"


class BaseAction(object):

    def cursor(self):
        return self.vim_state.view.cursor()

    qualifiers = set()

    def replay(self):
        self.apply_action()
        if self.is_a(insert_command):
            for key, hw_kc, string in self.key_presses:
                send_key_event(
                    key, hardware_keycode=hw_kc, window=self.vim_state.gtk_view
                )

    def apply(self):
        if self.vim_state.action_stack == []:
            self.apply_action()
            return self
        else:
            try:
                action = self.vim_state.action_stack.pop().compose(self)
                action.apply_action()
                return action
            except Exception, e:
                raise e
                self.vim_state.action_stack = []

    def is_a(self, qualifier):
        return qualifier._type in self.qualifiers


class PasteAction(BaseAction):

    def __init__(self, offset=0):
        self.offset = offset

    def apply_action(self):
        text, is_line = yanks[current_yank_buffer]
        pos = self.cursor()
        if is_line and self.offset == 0:
            pos = pos.beginning_of_line()
        elif is_line and self.offset == 1:
            pos = pos.forward_line()
        else:
            pos = pos.forward_char(self.offset)
        self.vim_state.buffer.insert(pos, text)


class YankAction(BaseAction):

    def apply_action(self):
        pass


class CharAction(BaseAction):

    def __init__(self, char):
        self.char = char

    def get_char(self):
        return self.char


class ComposedAction(BaseAction):

    def apply(self):
        self.vim_state.action_stack.append(self)

    def compose(self, expected_action):
        self.expected_action = expected_action
        if self.vim_state.action_stack == []:
            return self
        else:
            return self.vim_state.action_stack.pop().compose(self)


class NoAction(BaseAction):

    def apply_action(self):
        pass


class SwitchState(BaseAction):

    def __init__(self, state):
        self.state = state

    def apply_action(self):
        self.vim_state.state = self.state
        if self.state == NormalState:
            self.vim_state.buffer.remove_overlay(
                self.vim_state.selection_overlay
            )
        if is_command_state(self.state) or self.state == ReplaceState:
            self.vim_state.gtk_view.set_overwrite(True)
        elif self.state == InsertState:
            self.vim_state.gtk_view.set_overwrite(False)
        self.vim_state.set_column_memory()
        self.vim_state.keymaps_stack = [
            CharActionKeyMap(modes_keymaps[self.state])
        ]


@insert_command
class Insert(BaseAction):

    def apply_action(self):
        self.vim_state.state = InsertState
        self.vim_state.gtk_view.set_overwrite(False)
        self.vim_state.set_column_memory()


@movement_command
class Movement(BaseAction):

    def get_start_location(self):
        return self.cursor()

    def get_end_location(self):
        raise Exception("Not implemented")

    def is_line(self):
        return False

    def apply_action(self):
        loc = self.get_end_location()
        self.vim_state.view.goto(loc)
        self.vim_state.set_column_memory()


is_keyword_re = re.compile("[\w_0-9]")


def is_symbol_char(char):
    return not is_keyword_char(char) and not char.isspace()


def is_keyword_char(char):
    return is_keyword_re.match(char) is not None


class SimpleMovement(Movement):

    def __init__(self, movement_type, repeat=1):
        self.type = movement_type
        self.repeat = repeat

    def get_end_location(self, internal=False):
        cur = self.cursor()

        if self.type == Mov_Char:
            return cur.forward_char(self.repeat)

        elif self.type == Mov_Line:
            res = cur.forward_line(self.repeat)
            eol = res.end_of_line()
            return res.with_col(min(eol.column(), self.vim_state.column()))

        elif self.type == Mov_Word:
            loc = cur.forward_vim_word(self.repeat > 0)
            if not internal:
                return loc.forward_char(-1 if self.repeat > 0 else 1)
            return loc

    def apply_action(self):
        loc = self.get_end_location(True)

        self.vim_state.view.goto(loc)
        if self.type != Mov_Line:
            self.vim_state.set_column_memory()


class LineAction(ComposedAction):

    def apply(self):
        if self.vim_state.action_stack != []\
                and isinstance(self.vim_state.action_stack[-1], type(self)):
            BaseAction.apply(self)
        else:
            ComposedAction.apply(self)


@write_command
class Deletion(LineAction):

    def apply_action(self):
        a = self.expected_action
        if isinstance(a, Deletion):
            start_loc = self.cursor().beginning_of_line()
            end_loc = self.cursor().end_of_line()
            is_line = True
        else:
            start_loc = a.get_start_location()
            end_loc = a.get_end_location()
            is_line = a.is_line()

        self.vim_state.delete(start_loc, end_loc, is_line)


@write_command
@insert_command
class Replace(LineAction):

    def apply_action(self):
        cur = self.cursor()
        if not self.expected_action:
            start_loc = cur.beginning_of_line().forward_until(
                lambda c: not c.isspace()
            )
            end_loc = cur.end_of_line().forward_char(-1)
        else:
            start_loc = self.cursor()
            end_loc = self.expected_action.get_end_location()
        self.vim_state.delete(start_loc, end_loc)
        switch_state(self.vim_state, InsertState)


class Undo(BaseAction):

    def apply_action(self):
        self.vim_state.view.buffer().undo()


@write_command
@insert_command
class OpenLine(BaseAction):

    def __init__(self, before=False):
        self.before = before

    def apply_action(self):
        cur = self.cursor()
        if self.before:
            insert_loc = cur.forward_line(-1).end_of_line()
        else:
            insert_loc = cur.end_of_line()
        self.vim_state.view.goto(insert_loc)
        self.vim_state.buffer.insert(insert_loc, "\n")
        GPS.execute_action("autoindent selection")
        switch_state(self.vim_state, InsertState)


@movement_command
@composed_movement
class UntilCharMovement(ComposedAction):

    def __init__(self, backwards=False, stop_before=False):
        self.backwards = backwards
        self.stop_before = stop_before

    def is_line(self):
        return False

    def apply(self):
        self.vim_state.keymaps_stack.append(CharCharKeyMap())
        ComposedAction.apply(self)

    def compose(self, expected_action):
        self.vim_state.keymaps_stack.pop()
        return ComposedAction.compose(self, expected_action)

    def apply_action(self):
        self.vim_state.view.goto(
            self.get_end_location()
        )
        self.vim_state.set_column_memory()

    def get_start_location(self):
        return self.cursor()

    def get_end_location(self):
        loc = self.cursor().forward_until(
            lambda x: x == self.expected_action.get_char(),
            skip_first_char=True,
            stop_at_eol=True,
            backwards=self.backwards
        )

        if self.stop_before:
            loc = loc.forward_char(1 if self.backwards else -1)

        return loc


class ChainedAction(BaseAction):

    def __init__(self, *actions):
        self.actions = actions

    def apply(self):
        for action in self.actions:
            action.apply()


class ReplayAction(BaseAction):

    def apply(self):
        self.vim_state.last_write_command.replay()


class ReplayMove(BaseAction):

    def apply(self):
        self.vim_state.last_movement_command.replay()


class SwitchToVisual(BaseAction):

    def __init__(self, state):
        self.visual_state = state

    def apply_action(self):
        switch_state(self.vim_state, self.visual_state)
        self.vim_state.init_visual()


class VisualDeletion(BaseAction):

    def apply_action(self):
        for loc_a, loc_b in self.vim_state.get_selection_locs():
            self.vim_state.delete(
                loc_a,
                loc_b,
                is_line=self.vim_state.state == VisualStateLine
            )
        switch_state(self.vim_state, NormalState)


class VisualYank(BaseAction):

    def apply_action(self):
        for loc_a, loc_b in self.vim_state.get_selection_locs():
            self.vim_state.yank(
                loc_a, loc_b, is_line=self.vim_state.state == VisualStateLine)
        switch_state(self.vim_state, NormalState)


class EOLMovement(Movement):

    def get_end_location(self):
        return self.cursor().end_of_line().forward_char(-1)


class BOLMovement(Movement):

    def get_end_location(self):
        return self.cursor().beginning_of_line().forward_until(
            lambda c: not c.isspace()
        )


class ConflateLines(BaseAction):

    def apply_action(self):
        self.vim_state.view.goto(self.cursor().end_of_line().forward_char(-1))
        self.vim_state.buffer.delete(
            self.cursor().forward_char(),
            self.cursor().forward_char().forward_until(
                lambda c: not c.isspace()
            ).forward_char(-1)
        )
        self.vim_state.buffer.insert(self.cursor().forward_char(), " ")
        self.vim_state.view.goto(self.cursor().forward_char(-1))


def switch_state(vim_state, state):
    sw = create_action((SwitchState, state), vim_state)
    sw.apply_action()


basic_actions = {
    ".": (ReplayAction, ),
    ";": (ReplayMove, ),
    "i": (Insert, ),
    "a": (ChainedAction, (SimpleMovement, Mov_Char, 1), (Insert, )),
    "x": (ChainedAction, (Deletion,), (SimpleMovement, Mov_Char, 0)),
    "h": (SimpleMovement, Mov_Char, -1),
    "l": (SimpleMovement, Mov_Char, 1),
    "k": (SimpleMovement, Mov_Line, -1),
    "j": (SimpleMovement, Mov_Line, 1),
    "w": (SimpleMovement, Mov_Word, 1),
    "b": (SimpleMovement, Mov_Word, -1),
    "f": (UntilCharMovement, False, False),
    "F": (UntilCharMovement, True, False),
    "t": (UntilCharMovement, False, True),
    "T": (UntilCharMovement, True, True),
    "D": (ChainedAction, (Deletion,),
          (UntilCharMovement, False, True),
          (CharAction, "\n"),
          (SimpleMovement, Mov_Char, -1)),
    "C": (ChainedAction, (Replace,),
          (UntilCharMovement, False, True),
          (CharAction, "\n")),
    "d": (Deletion,),
    "c": (Replace,),
    "u": (Undo,),
    "o": (OpenLine, False),
    "O": (OpenLine, True),
    "v": (SwitchToVisual, VisualState),
    "V": (SwitchToVisual, VisualStateLine),
    "p": (PasteAction, 1),
    "P": (PasteAction,),
    "$": (EOLMovement,),
    "^": (BOLMovement,),
    "J": (ConflateLines,),
}


visual_actions = dict(basic_actions.items() + {
    "d": (VisualDeletion,),
    "D": (VisualDeletion,),
    "y": (VisualYank,),
    "Y": (VisualYank,)
}.items())


modes_keymaps = defaultdict(dict, {
    NormalState: basic_actions,
    VisualState: visual_actions,
    VisualStateLine: visual_actions,
    VisualStateBox: visual_actions
})


def on_file_edited(hn, f):

    def key_pressed_proxy(view, event):
        return view.vim_state.on_key_pressed(view, event)

    buffer = GPS.EditorBuffer.get(f)
    view = buffer.current_view()
    gtk_view = get_widgets_by_type(Gtk.TextView, view.pywidget())[0]
    gtk_view.vim_state = VimState(view, gtk_view)
    buffer.vim_state = gtk_view.vim_state
    gtk_view.connect("key-press-event", key_pressed_proxy)


GPS.Hook("file_edited").add(on_file_edited)
