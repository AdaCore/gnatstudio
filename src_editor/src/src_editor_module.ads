-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Glib;          use Glib;
with Gdk.GC;
with Gtk.Button;
with Gtk.Box;       use Gtk.Box;
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Text_Mark; use Gtk.Text_Mark;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Entry_Completion;   use Gtkada.Entry_Completion;

with Src_Editor_Box;

with Glide_Kernel;      use Glide_Kernel;
with Basic_Types;       use Basic_Types;
with String_List_Utils; use String_List_Utils;

with Ada.Unchecked_Deallocation;
with Generic_List;

package Src_Editor_Module is

   Src_Editor_Module_Id : Glide_Kernel.Module_ID;
   Src_Editor_Module_Name : constant String := "Source_Editor";

   type Undo_Redo_Information is record
      Undo_Button          : Gtk.Button.Gtk_Button;
      Redo_Button          : Gtk.Button.Gtk_Button;

      Undo_Menu_Item       : Gtk.Menu_Item.Gtk_Menu_Item;
      Redo_Menu_Item       : Gtk.Menu_Item.Gtk_Menu_Item;
   end record;

   Undo_Redo_Id : constant String := "Source_Editor_Undo_Redo_Information";

   package Undo_Redo_Data is new Glib.Object.User_Data (Undo_Redo_Information);

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
      File   : String) return Gtkada.MDI.MDI_Child;
   --  Return the first child that contains an editor that edits file.
   --  null is returned if there are no such editor
   --  File can either be a file name or a buffer identifier.

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

   -----------
   -- Marks --
   -----------

   type Mark_Identifier_Record is record
      Id     : Natural;
      Child  : MDI_Child;
      File   : Basic_Types.String_Access;
      Line   : Natural;
      Column : Natural;
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
      Id : String (1 .. L);
      GC : Gdk.GC.Gdk_GC;
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

   No_Handler : constant Handler_Id := (Null_Signal_Id, null);

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Kernel                   : Kernel_Handle;

      Recent_Menu_Item         : Gtk_Menu_Item;
      Source_Lines_Revealed_Id : Handler_Id := No_Handler;
      File_Edited_Id           : Handler_Id := No_Handler;
      File_Closed_Id           : Handler_Id := No_Handler;
      Display_Line_Numbers     : Boolean    := False;

      Stored_Marks             : Mark_Identifier_List.List;
      Next_Mark_Id             : Natural := 0;

      Open_File_Entry          : Gtkada_Entry;
      Open_File_Dialog         : Gtk_Dialog;

      Unopened_Files           : String_List_Utils.String_List.List;
      --  Contains a list of files for which marks have been created but
      --  that are not open.

      Categories               : Highlighting_Category_Array_Access;
      --  Contains a list of registered categories.
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   procedure Destroy (Id : in out Source_Editor_Module_Record);
   --  Free the memory used by the module.

end Src_Editor_Module;
