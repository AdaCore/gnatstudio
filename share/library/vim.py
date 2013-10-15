from gi.repository import Gtk
import GPS
from gps_utils import *
from pygps import get_widgets_by_type, send_key_event

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
write_command = qualifier_decorator("write_command")
insert_command = qualifier_decorator("insert_command")


def with_col(loc, col):
    return GPS.EditorLocation(loc.buffer(), loc.line(), col)


def forward_until(loc, pred, skip_first_char=False, stop_at_eol=False, backwards=False):
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


GPS.EditorLocation.with_col = with_col
GPS.EditorLocation.forward_until = forward_until

# CONSTANTS #

KEY_ESC = 65307

NormalState = 0
InsertState = 1
VisualSelectionState = 2

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
        return create_action(
            self.keymap.get(chr(keyval), (NoAction,)),
            vim_state
        )


class CharCharKeyMap(KeyMap):
    def get_action(self, keyval, vim_state):
        return create_action((CharAction, chr(keyval)), vim_state)


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

    def on_key_pressed(self, view, event):
        _, key = event.get_keyval()

        print "Key : ", key

        # Escape always returns back to normal mode
        if key == KEY_ESC:
            self.action_stack = []
            if self.current_insert_command:
                print self.current_insert_command.key_presses
            self.current_insert_command = None
            switch_state(self, NormalState)

        # Bind actions if we are in normal mode
        elif self.state == NormalState:
            if key in range(256):
                action = self.keymaps_stack[-1].get_action(key, self)
                remember_action = action.apply()
                if remember_action:
                    if remember_action.is_a(movement_command):
                        self.last_movement_command = remember_action
                    elif remember_action.is_a(write_command)\
                            or remember_action.is_a(insert_command):
                        self.last_write_command = remember_action
                        print "LAST WRITE COMMAND : ", self.last_write_command
                    if remember_action.is_a(insert_command):
                        self.current_insert_command = remember_action

            return True

        elif self.state == InsertState:
            if self.current_insert_command:
                self.current_insert_command.key_presses.append((key, event.hardware_keycode, event.string))
                self.last_ev = event

            return False

    def column(self):
        if self.column_memory:
            return self.column_memory
        else:
            return self.view.cursor().column()

    def set_column_memory(self):
        self.column_memory = self.view.cursor().column()

    def update_selection(self):
        if self.state == VisualSelectionState:
            self.selection_end_point = self.view.cursor()
            self.buffer.remove_overlay(self.selection_overlay)
            self.buffer.apply_overlay(
                self.selection_overlay,
                self.selection_start_point,
                self.selection_end_point
            )


class BaseAction(object):

    qualifiers = set()

    def replay(self):
        self.apply_action()
        if self.is_a(insert_command):
            print self.vim_state.state
            for key, hw_kc, string in self.key_presses:
                send_key_event(key, hardware_keycode=hw_kc, window=self.vim_state.gtk_view)

    def apply(self):
        if self.vim_state.action_stack == []:
            self.apply_action()
            return self
        else:
            try:
                action = self.vim_state.action_stack.pop().compose(self)
                print "ACTION : ", action
                action.apply_action()
                return action
            except Exception, e:
                raise e
                self.vim_state.action_stack = []


    def is_a(self, qualifier):
        return qualifier._type in self.qualifiers


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
        if self.state in (NormalState, VisualSelectionState):
            self.vim_state.gtk_view.set_overwrite(True)
        elif self.state == InsertState:
            self.vim_state.gtk_view.set_overwrite(False)
        self.vim_state.set_column_memory()


@insert_command
class Insert(BaseAction):
    def apply_action(self):
        self.vim_state.state = InsertState
        self.vim_state.gtk_view.set_overwrite(False)
        self.vim_state.set_column_memory()


@movement_command
class Movement(BaseAction):

    def get_start_location(self):
        return self.vim_state.view.cursor()

    def get_end_location(self):
        raise Exception("Not implemented")


class SimpleMovement(Movement):

    def __init__(self, movement_type, repeat=1):
        self.type = movement_type
        self.repeat = repeat

    def get_end_location(self):
        cur = self.vim_state.view.cursor()

        if self.type == Mov_Char:
            return cur.forward_char(self.repeat)

        elif self.type == Mov_Line:
            res = cur.forward_line(self.repeat)
            eol = res.end_of_line()
            return res.with_col(min(eol.column(), self.vim_state.column()))

        elif self.type == Mov_Word:
            loc = cur.forward_word(self.repeat)
            if self.repeat > 0:
                loc = loc.forward_char(-1)
            elif self.repeat < 0:
                loc = loc.forward_char(1)
            return loc

    def apply_action(self):
        loc = self.get_end_location()
        if self.type == Mov_Word:
            if self.repeat > 0:
                loc = loc.forward_char(1)
            elif self.repeat < 0:
                loc = loc.forward_char(-1)

        self.vim_state.view.goto(loc)
        if self.type != Mov_Line:
            self.vim_state.set_column_memory()


@write_command
class Deletion(ComposedAction):
    def apply(self):
        if self.vim_state.action_stack != []\
                and isinstance(self.vim_state.action_stack[-1], Deletion):
            BaseAction.apply(self)
        else:
            ComposedAction.apply(self)

    def apply_action (self):
        if isinstance(self.expected_action, Deletion):
            start_loc = self.vim_state.view.cursor().beginning_of_line()
            end_loc = self.vim_state.view.cursor().end_of_line()
        else:
            start_loc = self.expected_action.get_start_location()
            end_loc = self.expected_action.get_end_location()

        self.vim_state.view.buffer().delete(start_loc, end_loc)


@write_command
@insert_command
class Replace(ComposedAction):
    def apply(self):
        if self.vim_state.action_stack != []\
                and isinstance(self.vim_state.action_stack[-1], Replace):
            self.vim_state.action_stack.pop()
            self.expected_action = None
            self.apply_action()
        else:
            ComposedAction.apply(self)

    def apply_action (self):
        cur = self.vim_state.view.cursor()
        if not self.expected_action:
            start_loc = cur.beginning_of_line().forward_until(lambda c: not c.isspace())
            end_loc = cur.end_of_line().forward_char(-1)
        else:
            start_loc = self.vim_state.view.cursor()
            end_loc = self.expected_action.get_end_location()
        self.vim_state.view.buffer().delete(start_loc, end_loc)
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
        if self.before:
            insert_loc = self.vim_state.view.cursor().forward_line(-1).end_of_line()
        else:
            insert_loc = self.vim_state.view.cursor().end_of_line()
        self.vim_state.view.goto(insert_loc)
        self.vim_state.buffer.insert(insert_loc, "\n")
        GPS.execute_action("/Edit/Format Selection")
        switch_state(self.vim_state, InsertState)


@movement_command
class UntilCharMovement(ComposedAction):

    def __init__(self, backwards=False, stop_before=False):
        self.backwards = backwards
        self.stop_before = stop_before

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
        return self.vim_state.view.cursor()

    def get_end_location(self):
        loc = self.vim_state.view.cursor().forward_until(
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
        print "IN REPLAYACTION APPLY"
        self.vim_state.last_write_command.replay()


def switch_state(vim_state, state):
    sw = create_action((SwitchState, state), vim_state)
    sw.apply_action()

basic_actions = {
    ".": (ReplayAction, ),
    "i": (Insert, ),
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
    "$": (ChainedAction, (UntilCharMovement, False, True), (CharAction, "\n")),
    "D": (ChainedAction, (Deletion,),
                         (UntilCharMovement, False, True),
                         (CharAction, "\n")),
    "C": (ChainedAction, (Replace,),
                         (UntilCharMovement, False, True),
                         (CharAction, "\n")),
    "d": (Deletion,),
    "c": (Replace,),
    "u": (Undo,),
    "o": (OpenLine, False),
    "O": (OpenLine, True),
    "v": (SwitchState, VisualSelectionState)
}


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
