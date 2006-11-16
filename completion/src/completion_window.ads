-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements a completion window, which allows the user to
--  browse through a list of possible completions.
--
--  This is a light window, in the same GUI category as a tooltip window, in
--  that it is ephemeral, without any decoration.
--
--  The main completion contains a tree which displays a list of possible
--  completions. Each line in this list can be associated with an additional
--  text, which will be displayed in a secondary window when the line is
--  selected.
with Glib;           use Glib;
with GNAT.Strings;   use GNAT.Strings;

with Gtk.Window;     use Gtk.Window;

with Gtk.Button;     use Gtk.Button;
with Gtk.Label;      use Gtk.Label;

with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.List_Store; use Gtk.List_Store;

with Gtk.Text_View;          use Gtk.Text_View;
with Gtk.Text_Buffer;        use Gtk.Text_Buffer;
with Gtk.Text_Iter;          use Gtk.Text_Iter;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Text_Mark;          use Gtk.Text_Mark;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;

with Completion;    use Completion;
with GPS.Kernel;    use GPS.Kernel;

package Completion_Window is

   type Completion_Window_Record is new Gtk_Window_Record with private;
   type Completion_Window_Access is access all Completion_Window_Record'Class;

   procedure Gtk_New
     (Window : out Completion_Window_Access;
      Kernel : Kernel_Handle);
   --  Create a new Completion_Window.

   procedure Initialize (Window : access Completion_Window_Record'Class);
   --  Internal initialization procedure.

   procedure Add_Contents
     (Window     : Completion_Window_Access;
      Markup     : String;
      Completion : String;
      Notes      : String);
   --  Add new content to be displayed in the completion window.
   --  Markup is the string to display in the main completion tree, in pango
   --   markup format.
   --  Completion is the string to associate to the displayed string, in
   --   UTF-8
   --  Notes is an additional text that will be shown in the secondary window,
   --   in UTF-8.

   procedure Show
     (Window         : Completion_Window_Access;
      View           : Gtk_Text_View;
      Buffer         : Gtk_Text_Buffer;
      Iter           : Gtk_Text_Iter;
      Case_Sensitive : Boolean;
      Complete       : Boolean);
   --  Attach the completion window to a text view, and set the completion
   --  to start on the given mark.
   --  Show the window.

   procedure Set_Completion_Iterator
     (Window : Completion_Window_Access;
      Iter   : Completion_Iterator);
   --  Sets the completion iterator for the window.

   procedure Select_Next (Window : Completion_Window_Access);
   --  Select the next item in the window.

   procedure Delete (Window : access Completion_Window_Record'Class);
   --  Destroy the window and its contents.

   --  ??? Need a function to set case sensitivity

private

   type Completion_Proposal_Access is access Completion_Proposal'Class;

   type Information_Record is record
      Markup  : String_Access;
      Text    : String_Access;
      Notes   : String_Access;
      --  This can be null, in which case it indicates that it must be computed
      --  from Proposal.
      Proposal : Completion_Proposal_Access;
      Visible  : Boolean := True;
   end record;

   type Information_Array is array (Positive range <>) of Information_Record;
   type Information_Array_Access is access Information_Array;

   type Completion_Window_Record is new Gtk_Window_Record with record
      Kernel : Kernel_Handle;

      View  : Gtk_Tree_View;
      Model : Gtk_List_Store;

      Tree_Scroll : Gtk_Scrolled_Window;
      --  The scrolled window that contains the tree view.

      Text   : Gtk_Text_View;
      Buffer : Gtk_Text_Buffer;
      Mark   : Gtk_Text_Mark;
      --  The mark from which the text should be replaced

      Initial_Offset : Gint;
      Initial_Line   : Gint;
      --  Offset of cursor position when the window is first shown.

      Info   : Information_Array_Access;
      Index  : Natural;
      --  Index to the first free position in Info.

      Notes_Window : Gtk_Window;
      Notes_Label  : Gtk_Label;

      Location_Label  : Gtk_Label;
      Location_Title  : Gtk_Label;
      Location_Button : Gtk_Button;
      Location_Location : File_Location;

      Case_Sensitive : Boolean;
      In_Deletion    : Boolean := False;

      More_Iter      : Gtk_Tree_Iter := Null_Iter;
      --  Indicates the iter which says ("more...");

      Iter           : Completion_Iterator;
      --  The iter corresponding to the current completion engine, if any.
   end record;

end Completion_Window;
