-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Object;
with Glib;               use Glib;
with Gdk.GC;
with Gdk.Color;          use Gdk.Color;
with Gdk.Pixbuf;         use Gdk.Pixbuf;
with Gtk.Button;
with Gtk.Box;            use Gtk.Box;
with Gtk.Menu_Item;      use Gtk.Menu_Item;
with Gtk.Text_Mark;      use Gtk.Text_Mark;
with Gtk.Text_Buffer;    use Gtk.Text_Buffer;
with Gtk.Text_View;      use Gtk.Text_View;
with Gtkada.MDI;         use Gtkada.MDI;

with Src_Editor_Box;

with Glide_Kernel;       use Glide_Kernel;
with Glide_Kernel.Hooks; use Glide_Kernel.Hooks;
with String_List_Utils;  use String_List_Utils;
with VFS;                use VFS;
with Src_Contexts;

with Ada.Unchecked_Deallocation;
with Generic_List;
with Basic_Types;
with HTables;

package Src_Editor_Module is

   Src_Editor_Module_Id : Glide_Kernel.Module_ID;
   Src_Editor_Module_Name : constant String := "Source_Editor";

   Search_Result_Highlighting : constant String := "Search Results";

   type Undo_Redo_Information is record
      Undo_Button    : Gtk.Button.Gtk_Button;
      Redo_Button    : Gtk.Button.Gtk_Button;

      Undo_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Redo_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   end record;

   Undo_Redo_Id : constant String := "Source_Editor_Undo_Redo_Information";

   package Undo_Redo_Data is new Glib.Object.User_Data (Undo_Redo_Information);

   ---------------------------------------
   -- Module-specific graphical objects --
   ---------------------------------------

   Remove_Blank_Lines_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;
   Hide_Block_Pixbuf         : Gdk_Pixbuf := Null_Pixbuf;
   Unhide_Block_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   function Find_Current_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Return the source editor that has currently the focus in the MDI.
   --  If the focus in the MDI is not set on a source editor, then the top most
   --  editor is returned.

   function Find_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Gtkada.MDI.MDI_Child;
   --  Return the first child that contains an editor that edits file.
   --  null is returned if there are no such editor
   --  File can either be a file name or a buffer identifier.

   function Find_Other_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      View   : Gtk_Text_View;
      Buffer : Gtk_Text_Buffer) return Src_Editor_Box.Source_Editor_Box;
   --  Find an editor other than View that edits Buffer.
   --  Return null if no such editor is found in the MDI.

   function Get_Source_Box_From_MDI
     (Child : Gtkada.MDI.MDI_Child) return Src_Editor_Box.Source_Editor_Box;
   --  Return the source editor contained in a MDI_Child. Constraint_Error if
   --  Child doesn't contain an editor. null is returned Child is null.

   function Find_Child
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Editor : access Src_Editor_Box.Source_Editor_Box_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Return the MDI child containing Editor

private

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Src_Editor_Box.Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   ------------------------
   -- Editors Hash-table --
   ------------------------

   --  This implements a quick way to retrieve an editor which corresponds to
   --  a given file.

   type Header_Num is range 1 .. 1_000;

   type Element is record
      Child : Gtkada.MDI.MDI_Child;
   end record;

   procedure Free (X : in out Element);

   No_Element : constant Element := (Child => null);

   function Hash (F : Virtual_File) return Header_Num;
   function Equal (F1, F2 : Virtual_File) return Boolean;

   package Editors_Hash is new HTables.Simple_HTable
     (Header_Num, Element, Free, No_Element, Virtual_File, Hash, Equal);

   -----------
   -- Marks --
   -----------

   type Mark_Identifier_Record is record
      Id     : Natural;
      Child  : MDI_Child;
      File   : VFS.Virtual_File;
      Line   : Natural := 0;
      Column : Natural := 1;
      Mark   : Gtk_Text_Mark;
      Length : Natural;
   end record;

   procedure Free (X : in out Mark_Identifier_Record);
   --  Free memory associated to X.

   package Mark_Identifier_List is new Generic_List (Mark_Identifier_Record);

   -----------------------------
   -- Highlighting categories --
   -----------------------------

   type Highlighting_Category_Record (L : Natural) is record
      Id    : String (1 .. L);
      GC    : Gdk.GC.Gdk_GC;
      Color : Gdk_Color;
   end record;

   type Highlighting_Category is access Highlighting_Category_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Highlighting_Category_Record, Highlighting_Category);

   type Highlighting_Category_Array is array (Natural range <>) of
     Highlighting_Category;

   type Highlighting_Category_Array_Access is
     access Highlighting_Category_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Highlighting_Category_Array, Highlighting_Category_Array_Access);

   --------------------------
   -- Source_Editor_Module --
   --------------------------

   type Lines_Revealed_Hook_Record is new Glide_Kernel.Hooks.Hook_Args_Record
      with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Data   : Glide_Kernel.Hooks.Hooks_Data'Class);
   --  Hook called when the "source_lines_revealed" hook is run.

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Kernel                : Kernel_Handle;
      Lines_Hook            : Lines_Revealed_Hook;

      Display_Line_Numbers  : Boolean    := False;

      Show_Subprogram_Names : Boolean    := False;

      Stored_Marks          : Mark_Identifier_List.List;
      Next_Mark_Id          : Natural := 0;

      Unopened_Files           : String_List_Utils.String_List.List;
      --  Contains a list of files for which marks have been created but
      --  that are not open.

      Categories            : Highlighting_Category_Array_Access;
      --  Contains a list of registered categories.

      Blank_Lines_GC        : Gdk.GC.Gdk_GC := null;
      Post_It_Note_GC       : Gdk.GC.Gdk_GC := null;

      Editors               : Editors_Hash.HTable;

      --  The following fields are related to the current search.

      Search_Context        : Src_Contexts.Files_Project_Context_Access;
      Search_File           : VFS.Virtual_File;
      Search_Pattern        : Basic_Types.String_Access;
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   procedure Destroy (Id : in out Source_Editor_Module_Record);
   --  Free the memory used by the module.

end Src_Editor_Module;
