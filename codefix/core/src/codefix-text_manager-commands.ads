------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package Codefix.Text_Manager.Commands is

   ---------------------
   -- Remove_Word_Cmd --
   ---------------------

   type Remove_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This              : in out Remove_Word_Cmd;
      Current_Text      : Text_Navigator_Abstr'Class;
      Word              : Word_Cursor'Class;
      Search_Forward    : Boolean := False;
      All_Occurrences   : Boolean := False;
      Remove_Empty_Line : Boolean := False);
   --  Set all the marks that will be necessary later to remove the word.
   --  Remove_Empty_Line is True if we want to remove the line if its contents
   --  is empty after the execution of the command.

   overriding
   procedure Free (This : in out Remove_Word_Cmd);
   --  Free the memory associated to a Remove_Word_Cmd

   overriding
   procedure Execute
     (This         : Remove_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the word removed

   overriding
   function Is_Writable (This : Remove_Word_Cmd) return Boolean;
   --  See inherited documentation

   ---------------------
   -- Insert_Word_Cmd --
   ---------------------

   type Insert_Word_Cmd
     (Complexity : Fix_Complexity) is new Text_Command with private;

   procedure Initialize
     (This            : in out Insert_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      After_Pattern   : String := "";
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified;
      Insert_New_Line : Boolean := False);
   --  Set all the marks that will be necessary later to insert the word

   overriding
   procedure Free (This : in out Insert_Word_Cmd);
   --  Fre the memory associated to an Insert_Word_Cmd

   overriding
   procedure Execute
     (This         : Insert_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the word inserted

   overriding
   function Is_Writable (This : Insert_Word_Cmd) return Boolean;
   --  See inherited documentation

   --------------------
   -- Move_Word_Cmd  --
   --------------------

   type Move_Word_Cmd (Complexity : Fix_Complexity)
     is new Text_Command with private;

   procedure Initialize
     (This            : in out Move_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      Insert_New_Line : Boolean := False);
   --  Set all the marks that will be needed to move the word later

   overriding
   procedure Free (This : in out Move_Word_Cmd);
   --  Free the memory associated to a Move_Word_Cmd

   overriding
   procedure Execute
     (This         : Move_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the word moved

   overriding
   function Is_Writable (This : Move_Word_Cmd) return Boolean;
   --  See inherited documentation

   ----------------------
   -- Replace_Word_Cmd --
   ----------------------

   type Replace_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This           : in out Replace_Word_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Word           : Word_Cursor'Class;
      New_Word       : Unbounded_String;
      Do_Indentation : Boolean := False);
   --  Set all the marks that will be needed to replace the word later

   overriding
   procedure Free (This : in out Replace_Word_Cmd);
   --  Free the memory associated to a Replace_Word_Cmd

   overriding
   procedure Execute
     (This         : Replace_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the word replaced

   overriding
   function Is_Writable (This : Replace_Word_Cmd) return Boolean;
   --  See inherited documentation

   ----------------------
   -- Invert_Words_Cmd --
   ----------------------

   type Invert_Words_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_Loc  : File_Cursor'Class;
      First_Word   : Unbounded_String;
      Second_Word  : Unbounded_String);
   --  Set all the marks that will be needed to invert the two words later

   overriding
   procedure Free (This : in out Invert_Words_Cmd);
   --  Free the memory associated to an Invert_Word_Cmd

   overriding
   procedure Execute
     (This         : Invert_Words_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the invertion of the two word

   overriding
   function Is_Writable (This : Invert_Words_Cmd) return Boolean;
   --  See inherited documentation

   ------------------
   -- Add_Line_Cmd --
   ------------------

   type Add_Line_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Add_Line_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Line         : Unbounded_String;
      Indent       : Boolean);
   --  Set all the marks that will be needed to add the line later

   overriding
   procedure Free (This : in out Add_Line_Cmd);
   --  Free the memory associated to an Add_Line_Cmd

   overriding
   procedure Execute
     (This         : Add_Line_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the invertion add of the line

   overriding
   function Is_Writable (This : Add_Line_Cmd) return Boolean;
   --  See inherited documentation

   -----------------------
   -- Replace_Slice_Cmd --
   -----------------------

   type Replace_Slice_Cmd is new Text_Command with private;

   procedure Initialize
     (This                     : in out Replace_Slice_Cmd;
      Current_Text             : Text_Navigator_Abstr'Class;
      Start_Cursor, End_Cursor : File_Cursor'Class;
      New_Text                 : Unbounded_String);
   --  Set all the marks that will be necessary later to remove the slice

   overriding
   procedure Free (This : in out Replace_Slice_Cmd);
   --  Free the memory associated to a Remove_Sloce_Cmd

   overriding
   procedure Execute
     (This         : Replace_Slice_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the slice removed

   overriding
   function Is_Writable (This : Replace_Slice_Cmd) return Boolean;
   --  See inherited documentation

   ----------------------------
   -- Remove_Blank_Lines_Cmd --
   ----------------------------

   type Remove_Blank_Lines_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Remove_Blank_Lines_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Cursor : File_Cursor'Class);
   --  Set all the marks that will be necessary later to remove the blank lines

   overriding
   procedure Free (This : in out Remove_Blank_Lines_Cmd);
   --  Free the memory associated to a Remove_Sloce_Cmd

   overriding
   procedure Execute
     (This         : Remove_Blank_Lines_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the slice removed

   procedure Remove_Blank_Lines
     (Current_Text : in out Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Remove all consecutive blank lines starting at the location given
   --  in parameter. This helper function may be used directly in commands.

   overriding
   function Is_Writable (This : Remove_Blank_Lines_Cmd) return Boolean;
   --  See inherited documentation

   -----------------------
   -- Tab_Expansion_Cmd --
   -----------------------

   type Tab_Expansion_Cmd is new Text_Command with private;

   procedure Initialize
     (This   : in out Tab_Expansion_Cmd;
      Cursor : File_Cursor);
   --  Store the cursor (needed later to execute the command)

   overriding
   procedure Free (This : in out Tab_Expansion_Cmd);
   --  Free the memory associated to a Tab_Expanion_Cmd

   overriding
   procedure Execute
     (This         : Tab_Expansion_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Replace all the horizontal tabs by spaces

   overriding
   function Is_Writable (This : Tab_Expansion_Cmd) return Boolean;
   --  See inherited documentation

private
   type Remove_Word_Cmd is new Text_Command with record
      Word              : Word_Mark;
      Search_Forward    : Boolean;
      All_Occurrences   : Boolean;
      Remove_Empty_Line : Boolean;
   end record;

   type Insert_Word_Cmd (Complexity : Fix_Complexity)
     is new Text_Command (Complexity) with record
      Word            : Word_Mark;
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified;
      New_Position    : Word_Mark;
      Insert_New_Line : Boolean := False;
      After_Pattern   : Unbounded_String;
   end record;

   type Move_Word_Cmd (Complexity : Fix_Complexity)
     is new Text_Command (Complexity)
   with record
      Step_Remove : Remove_Word_Cmd (Complexity);
      Step_Insert : Insert_Word_Cmd (Complexity);
   end record;

   type Replace_Word_Cmd is new Text_Command with record
      Mark           : Word_Mark;
      Str_Expected   : Unbounded_String;
      Do_Indentation : Boolean := False;
   end record;

   type Invert_Words_Cmd is new Text_Command with record
      Location    : Ptr_Mark;
      First_Word  : Unbounded_String;
      Second_Word : Unbounded_String;
   end record;

   type Add_Line_Cmd is new Text_Command with record
      Line     : Unbounded_String;
      Position : Ptr_Mark;
      Indent   : Boolean;
   end record;

   type Replace_Slice_Cmd is new Text_Command with record
      Start_Mark : Ptr_Mark;
      End_Mark   : Ptr_Mark;
      New_Text   : Unbounded_String;
   end record;

   type Remove_Blank_Lines_Cmd is new Text_Command (Simple) with record
      Start_Mark : Ptr_Mark;
   end record;

   type Tab_Expansion_Cmd is new Text_Command with record
      Cursor : File_Cursor;
   end record;

end Codefix.Text_Manager.Commands;
