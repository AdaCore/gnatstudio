-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with GNAT.Strings;

with Gdk.GC;
with Gdk.Pixbuf;         use Gdk.Pixbuf;

with Glib.Object;
with Glib.Xml_Int;       use Glib.Xml_Int;
with Glib;               use Glib;

with Gtk.Text_Buffer;    use Gtk.Text_Buffer;
with Gtk.Text_View;      use Gtk.Text_View;
with Gtk.Widget;         use Gtk.Widget;

with Gtkada.MDI;         use Gtkada.MDI;

with Commands.Controls;  use Commands.Controls;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Styles;  use GPS.Kernel.Styles;
with GPS.Kernel;         use GPS.Kernel;
with Generic_List;
with HTables;
with Src_Contexts;
with Src_Editor_Box;
with VFS;                use VFS;

package Src_Editor_Module is

   Src_Editor_Module_Id : Module_ID;
   Src_Editor_Module_Name : constant String := "Source_Editor";

   Search_Result_Highlighting : constant String := "Search Results";

   Undo_Redo_Id : constant String := "Source_Editor_Undo_Redo_Information";

   package Undo_Redo_Data is new Glib.Object.User_Data (Undo_Redo);
   --  ??? The undo/redo widgets should probably be handled by the kernel, not
   --  by the source_editor_module. This will allow us to get rid of this
   --  package

   ---------------------------------------
   -- Module-specific graphical objects --
   ---------------------------------------

   Remove_Blank_Lines_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;
   Hide_Block_Pixbuf         : Gdk_Pixbuf := Null_Pixbuf;
   Unhide_Block_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;

   File_Pixbuf               : Gdk_Pixbuf := Null_Pixbuf;
   File_Modified_Pixbuf      : Gdk_Pixbuf := Null_Pixbuf;
   File_Unsaved_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   function Find_Current_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Return the source editor that has currently the focus in the MDI.
   --  If the focus in the MDI is not set on a source editor, then the top most
   --  editor is returned.

   function Find_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Gtkada.MDI.MDI_Child;
   --  Return the first child that contains an editor that edits file. This is
   --  the view that last had the focus, in case multiple views exist for this
   --  file.
   --  null is returned if there are no such editor
   --  File can either be a file name or a buffer identifier.

   function Find_Other_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      View   : Gtk_Text_View;
      Buffer : Gtk_Text_Buffer) return Src_Editor_Box.Source_Editor_Box;
   --  Find an editor other than View that edits Buffer.
   --  Return null if no such editor is found in the MDI.

   function Get_Source_Box_From_MDI
     (Child : Gtkada.MDI.MDI_Child)
      return Src_Editor_Box.Source_Editor_Box;
   --  Return the source editor contained in a MDI_Child. Constraint_Error if
   --  Child doesn't contain an editor. null is returned Child is null.

   function Is_Source_Box (Child : Gtkada.MDI.MDI_Child) return Boolean;
   --  Return True if Child contains an editor

   function Find_Child
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Editor : access Src_Editor_Box.Source_Editor_Box_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Return the MDI child containing Editor

   function New_View
     (Kernel  : access Kernel_Handle_Record'Class;
      Current : Src_Editor_Box.Source_Editor_Box)
      return Src_Editor_Box.Source_Editor_Box;
   --  Create a new view for Current and add it in the MDI.
   --  The current editor is the focus child in the MDI.
   --  If Add is True, the Box is added to the MDI.

   ---------------------
   -- Automatic saves --
   ---------------------

   function Autosaved_File (File : VFS.Virtual_File) return VFS.Virtual_File;
   --  Return the autosaved file corresponding to File.

   function Is_Auto_Save (File : VFS.Virtual_File) return Boolean;
   --  Return True if File is an autosave file.

   --------------------------------
   -- Data common to all editors --
   --------------------------------

   function Line_Number_Character_Width return Gint;
   pragma Inline (Line_Number_Character_Width);
   --  Return the default character width to use when showing line numbers.
   --  Return 0 when we are not showing line numbers.
   --  This function is used as a preference cache for all editors.

   function Post_It_Note_GC return Gdk.GC.Gdk_GC;
   pragma Inline (Post_It_Note_GC);
   --  Return the color to use for post-it notes

private

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

   procedure Do_Nothing (X : in out Location_Marker);
   --  Free memory associated to X.

   package Marker_List is new Generic_List (Location_Marker, Do_Nothing);

   -----------------------------
   -- Highlighting categories --
   -----------------------------

   type Highlighting_Category_Record is record
      Style : Style_Access;
      Mark_In_Speedbar : Boolean := False;
      --  If True, a mark is added in the speedbar even when we only highlight
      --  a small range of a line, as opposed to the whole line.
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

   type Lines_Revealed_Hook_Record is new GPS.Kernel.Hooks.Function_With_Args
      with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Hook called when the "source_lines_revealed" hook is run.

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Lines_Hook            : Lines_Revealed_Hook;

      Display_Line_Numbers  : Boolean    := False;
      Character_Width       : Gint := 0;

      Show_Subprogram_Names : Boolean    := False;

      Stored_Marks          : Marker_List.List;
      Next_Mark_Id          : Natural := 0;

      Categories            : Highlighting_Category_Array_Access;
      --  Contains a list of registered categories.

      Blank_Lines_GC        : Gdk.GC.Gdk_GC := null;
      Post_It_Note_GC       : Gdk.GC.Gdk_GC := null;

      Editors               : Editors_Hash.HTable;

      --  The following fields are related to the current search.

      Search_Context        : Src_Contexts.Files_Project_Context_Access;
      Search_File           : VFS.Virtual_File;
      Search_Pattern        : GNAT.Strings.String_Access;
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   procedure Destroy (Id : in out Source_Editor_Module_Record);
   procedure Default_Context_Factory
     (Module  : access Source_Editor_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget);
   function Save_Function
     (Module       : access Source_Editor_Module_Record;
      Child        : Gtk.Widget.Gtk_Widget;
      Mode         : Save_Function_Mode;
      Single_Child : Boolean) return Boolean;
   procedure Customize
     (Module : access Source_Editor_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   function Bookmark_Handler
     (Module : access Source_Editor_Module_Record;
      Load   : Xml_Int.Node_Ptr := null) return Location_Marker;
   --  See inherited documentation

   ----------
   -- Misc --
   ----------
   --  These utilities are needed by child packages

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File := VFS.No_File;
      Create_New : Boolean := True;
      Focus      : Boolean := True;
      Force      : Boolean := False;
      Group      : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic)
      return Src_Editor_Box.Source_Editor_Box;
   --  Open a file and return the handle associated with it.
   --  If Add_To_MDI is set to True, the box will be added to the MDI window.
   --  If Focus is True, the box will be raised if it is in the MDI.
   --  See Create_File_Exitor.
   --  Position indicates the position to give to the editor in the MDI.
   --  If Force is true, then the file is reloaded without asking confirmation
   --  from the user

end Src_Editor_Module;
