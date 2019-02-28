------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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
with Glib.Main;      use Glib.Main;
with GNAT.Strings;   use GNAT.Strings;
with Gtk.Box;        use Gtk.Box;
with Gtk.Bin;        use Gtk.Bin;

with Gtk.Window;     use Gtk.Window;

with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;

with Gtk.Text_View;          use Gtk.Text_View;
with Gtk.Text_Buffer;        use Gtk.Text_Buffer;
with Gtk.Text_Iter;          use Gtk.Text_Iter;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Text_Mark;          use Gtk.Text_Mark;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;

with Pango.Font;             use Pango.Font;

with Completion;    use Completion;
with GPS.Kernel;    use GPS.Kernel;
with Basic_Types;   use Basic_Types;
with Language;      use Language;

with Completion.History; use Completion.History;

with Completion_Utils; use Completion_Utils;
with Engine_Wrappers;  use Engine_Wrappers;
with GPS.Editors;
with Ada.Containers.Indefinite_Holders;

package Completion_Window is

   type Smart_Completion_Type is (Disabled, Manual, Normal, Dynamic);

   type Completion_Window_Record is new Gtk_Window_Record with private;
   type Completion_Window_Access is access all Completion_Window_Record'Class;

   type Completion_Explorer_Record is new Gtk_Hbox_Record with private;
   type Completion_Explorer_Access is access all
     Completion_Explorer_Record'Class;

   type Completion_Notes_Window_Record is new Gtk_Window_Record with private;
   type Completion_Notes_Window is access all
     Completion_Notes_Window_Record'Class;
   --  Type representing the completion notes window that appears when
   --  navigating among the completion proposals.

   procedure Gtk_New
     (Window : out Completion_Window_Access;
      Kernel : Kernel_Handle);
   --  Create a new Completion_Window

   procedure Initialize
     (Window : access Completion_Window_Record'Class;
      Kernel : Kernel_Handle);
   --  Internal initialization procedure

   procedure Show
     (Window   : Completion_Window_Access;
      View     : Gtk_Text_View;
      Buffer   : Gtk_Text_Buffer;
      Iter     : Gtk_Text_Iter;
      Mark     : Gtk_Text_Mark;
      Lang     : Language_Access;
      Complete : Boolean;
      Volatile : Boolean;
      Mode     : Smart_Completion_Type;
      Editor   : GPS.Editors.Editor_Buffer'Class
                   := GPS.Editors.Nil_Editor_Buffer);
   --  Attach the completion window to a text view, and set the completion
   --  to start on the given mark.
   --  Mark is set on the position which the cursor should occupy after a
   --  completion. It should be initialized and freed by the caller.
   --  Show the window.
   --  If Complete is true, select the first entry in the list and complete to
   --  the biggest common prefix.

   procedure Set_Iterator
     (Window : Completion_Window_Access;
      Iter   : Root_Iterator_Access);
   procedure Set_Iterator
     (Explorer : Completion_Explorer_Access;
      Iter     : Root_Iterator_Access);
   --  Sets the iterator for the window.
   --  Caller should not free Iter.

   procedure Select_Next (Window : Completion_Window_Access);
   --  Select the next item in the window.

   procedure Set_History
     (Window : Completion_Window_Access; History : Completion_History_Access);
   --  Sets the history of the completion window. This history will be feed
   --  by completions chosen by the user.

   procedure Complete_And_Exit
     (Window : access Completion_Window_Record'Class);
   --  Complete using the current selection and exit.

   procedure Delete (Window : access Completion_Window_Record'Class);
   --  Destroy the window and its contents.

   --  ??? Need a function to set case sensitivity

private

   Minimal_Items_To_Show : constant := 2;

   procedure Expand_Selection
     (Explorer : access Completion_Explorer_Record'Class);
   --  Expand the current selection until Number entries have been added or
   --  the completion iter has reach the end.

   procedure Clear (Explorer : access Completion_Explorer_Record'Class);
   --  Clear the model and stored information

   procedure Select_Next (Explorer : Completion_Explorer_Access);
   --  Select the next item in the explorer

   type Information_Record is record
      Markup   : String_Access;
      Text     : String_Access;
      Icon_Name : String_Access;
      Offset   : Character_Offset_Type;
      --  The offset at which to place the cursor after completion.
      Proposals : Proposals_List.List;
      Accessible : Boolean;
   end record;

   type Information_Array is array (Positive range <>) of Information_Record;
   type Information_Array_Access is access Information_Array;

   package Completion_Explorer_Idle is new Glib.Main.Generic_Sources
     (Completion_Explorer_Access);

   type Completion_Explorer_Record is new Gtk_Hbox_Record with record

      Kernel : Kernel_Handle;

      View  : Gtk_Tree_View;
      Model : Gtk_List_Store;
      Model_Filter : Gtk_Tree_Model_Filter;

      Tree_Scroll : Gtk_Scrolled_Window;
      --  The scrolled window that contains the tree view.

      Info   : Information_Array_Access;
      Index  : Natural;
      --  Index to the first free position in Info.

      Shown : Natural := 0;
      --  Number of elements displayed in the tree.

      Number_To_Show : Natural := 50;

      Notes_Container : Gtk_Bin;
      --  The container which actually contains the notes.

      Notes_Info      : Notes_Window_Info;
      --  Necessary information to idly complete the notes window

      Notes_Need_Completion : Boolean := False;

      Computing_Iter  : Gtk_Tree_Iter := Null_Iter;
      --  Indicates the iter which says ("more...");

      Iter           : Root_Iterator_Access;
      --  The iter corresponding to the current completion engine, if any.

      Pattern : String_Access;
      --  The currently typed pattern.

      Completion_History : Completion_History_Access;

      Has_Idle_Computation : Boolean := False;
      --  Whether we are querying the database for expansion.

      Idle_Computation : G_Source_Id;
      --  The id of the current idle callback.

      Fixed_Width_Font : Pango_Font_Description;
      --  A fixed-width font in use in the tree and the notes window.

      Case_Sensitive : Boolean;

      Completion_Window : Completion_Window_Access;
      --  access to the parent completion window
   end record;

   package Editors_Holders is
     new Ada.Containers.Indefinite_Holders
       (GPS.Editors.Editor_Buffer'Class, GPS.Editors."=");

   type Completion_Window_Record is new Gtk_Window_Record with record
      Explorer : Completion_Explorer_Access;

      Editor     : Editors_Holders.Holder := Editors_Holders.Empty_Holder;
      Text       : Gtk_Text_View;
      Buffer     : Gtk_Text_Buffer;
      Start_Mark : Gtk_Text_Mark;
      --  The mark from which the text should be replaced

      End_Mark : Gtk_Text_Mark;
      --  The mark set at the cursor place

      Initial_Offset : Gint;
      Initial_Line   : Gint;
      --  Offset of cursor position when the window is first shown.

      In_Deletion    : Boolean := False;
      --  Set to True when we are deleting text.

      In_Destruction : Boolean := False;
      --  Set to True when we are destroying the window.

      Volatile : Boolean := False;
      --  Whether the completion window was created using an automated trigger.

      Mode : Smart_Completion_Type;
      --  The mode of smart completion.

      Lang : Language_Access;
      --  The language on which the window is completing.

      Notes_Window : Completion_Notes_Window;
      --  The popup window containing the documentation
   end record;

   type Completion_Notes_Window_Record is new Gtk_Window_Record with record
      Notes_Scroll : Gtk_Scrolled_Window;
      --  The scrolled window that actually contains the documentation
   end record;

   --  Tree model columns.

   Markup_Column : constant := 0;
   Index_Column  : constant := 1;
   Icon_Name_Column : constant := 2;
   Shown_Column  : constant := 3;
   Completion_Column : constant := 4;

end Completion_Window;
