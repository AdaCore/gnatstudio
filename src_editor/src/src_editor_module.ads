------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with System;

with Ada.Containers.Doubly_Linked_Lists;

with Ada.Unchecked_Deallocation;
with GNAT.Expect;
with GNAT.Strings;

with Gdk.Pixbuf;         use Gdk.Pixbuf;

with Glib.Object;
with Glib;               use Glib;

with Gtk.Text_Buffer;    use Gtk.Text_Buffer;
with Gtk.Text_View;      use Gtk.Text_View;
with Gtk.Widget;         use Gtk.Widget;

with Gtkada.MDI;         use Gtkada.MDI;

with Commands.Controls;  use Commands.Controls;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;  use GPS.Kernel.Modules;
with GPS.Styles;         use GPS.Styles;
with GPS.Styles.UI;      use GPS.Styles.UI;
with GPS.Kernel;         use GPS.Kernel;
with Generic_List;
with HTables;
with Src_Contexts;
with Src_Editor_Box;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.Scripts;            use GNATCOLL.Scripts;

with Basic_Types;        use Basic_Types;
with Src_Editor_Buffer;  use Src_Editor_Buffer;
with XML_Utils;          use XML_Utils;

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
      File   : GNATCOLL.VFS.Virtual_File) return Gtkada.MDI.MDI_Child;
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

   ---------------------
   -- Automatic saves --
   ---------------------

   function Autosaved_File
     (File : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File;
   --  Return the autosaved file corresponding to File

   function Is_Auto_Save (File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return True if File is an autosave file

   --------------------------------
   -- Data common to all editors --
   --------------------------------

   function Line_Number_Character_Width return Gint;
   pragma Inline (Line_Number_Character_Width);
   --  Return the default character width to use when showing line numbers.
   --  Return 0 when we are not showing line numbers.
   --  This function is used as a preference cache for all editors.

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Dir        : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Create_New : Boolean := True) return Src_Editor_Box.Source_Editor_Box;
   --  Create a new text editor that edits File.
   --  If File is the empty string, or the file doesn't exist and Create_New is
   --  True, then an empty editor is created.
   --  Dir is the directory in which the file should be saved initially, if
   --  File = VFS.No_File.
   --  No check is done to make sure that File is not already edited
   --  elsewhere. The resulting editor is not put in the MDI window.

   ------------------
   -- Highlighters --
   ------------------

   --  A highlighter is the association of a regular expression with actions,
   --  which allows specifying custom hyperlinks in hyper mode.

   type Highlighter_Record is record
      Pattern_String : GNAT.Strings.String_Access;
      Pattern        : GNAT.Expect.Pattern_Matcher_Access;
      Paren_Count    : Natural := 0;
      Action         : Subprogram_Type;
      Alternate      : Subprogram_Type;
      Index          : Integer;
   end record;

   procedure Free (Self : in out Highlighter_Record);
   --  Free memory allocated for Self

   Null_Highlighter : constant Highlighter_Record :=
                        (null, null, 0, null, null, 0);

   package List_Of_Highlighters is new Ada.Containers.Doubly_Linked_Lists
     (Highlighter_Record);

   ----------------
   -- Hyper_Mode --
   ----------------

   procedure Register_Highlighter (Highlighter : Highlighter_Record);
   --  Register a highlighter in the source editor module

   procedure Unregister_Highlighter (Highlighter : Highlighter_Record);
   --  Unregister a highlighter

   function Get_Highlighters return List_Of_Highlighters.List;
   --  Return the list of registered highlighters

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
     (Header_Num   => Header_Num,
      Element      => Element,
      Free_Element => Free,
      No_Element   => No_Element,
      Key          => Virtual_File,
      Hash         => Hash,
      Equal        => Equal);

   -----------
   -- Marks --
   -----------

   procedure Do_Nothing (X : in out Location_Marker) is null;
   --  Free memory associated to X

   package Marker_List is new Generic_List (Location_Marker, Do_Nothing);

   -----------------------------
   -- Highlighting categories --
   -----------------------------

   type Highlighting_Category_Record is record
      Style : Style_Access;
   end record;

   type Highlighting_Category is access Highlighting_Category_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Highlighting_Category_Record, Highlighting_Category);

   type Highlighting_Category_Array is array (Natural range <>) of
     Highlighting_Category;

   type Highlighting_Category_Array_Access is
     access Highlighting_Category_Array;

   procedure Free (Categories : in out Highlighting_Category_Array);
   --   Free all categories in the array

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Highlighting_Category_Array, Highlighting_Category_Array_Access);

   --------------------------
   -- Source_Editor_Module --
   --------------------------

   type Lines_Revealed_Hook_Record is new GPS.Kernel.Hooks.Function_With_Args
      with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Hook called when the "source_lines_revealed" hook is run

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Lines_Hook            : Lines_Revealed_Hook;

      Display_Line_Numbers  : Boolean    := False;
      Character_Width       : Gint := 0;

      Show_Subprogram_Names : Boolean    := False;

      Stored_Marks          : Marker_List.List;
      Next_Mark_Id          : Natural := 0;

      Categories            : Highlighting_Category_Array_Access;
      --  Contains a list of registered categories

      Editors               : Editors_Hash.Instance;

      Highlighting_Manager : System.Address := System.Null_Address;
      --  The highlighting manager

      --  The following fields are related to the current search

      Search_Context        : Src_Contexts.Files_Project_Context_Access;
      Search_File           : GNATCOLL.VFS.Virtual_File;
      Search_Pattern        : GNAT.Strings.String_Access;

      --  The following fields are related to hyper mode

      Highlighters          : List_Of_Highlighters.List;
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   overriding procedure Destroy (Id : in out Source_Editor_Module_Record);
   overriding procedure Default_Context_Factory
     (Module  : access Source_Editor_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   overriding function Save_Function
     (Module       : access Source_Editor_Module_Record;
      Child        : Glib.Object.GObject;
      Mode         : Save_Function_Mode;
      Single_Child : Boolean;
      Force        : Boolean) return Boolean;
   overriding procedure Customize
     (Module : access Source_Editor_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   overriding function Bookmark_Handler
     (Module : access Source_Editor_Module_Record;
      Load   : XML_Utils.Node_Ptr := null) return Location_Marker;
   --  See inherited documentation

   ----------
   -- Misc --
   ----------
   --  These utilities are needed by child packages

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Create_New : Boolean := True;
      Focus      : Boolean := True;
      Force      : Boolean := False;
      Line       : Editable_Line_Type;
      Column     : Visible_Column_Type;
      Column_End : Visible_Column_Type;
      Group      : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic;
      Initial_Dir      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
      return Src_Editor_Box.Source_Editor_Box;
   --  Open a file and return the handle associated with it.
   --  If Add_To_MDI is set to True, the box will be added to the MDI window.
   --  If Focus is True, the box will be raised if it is in the MDI.
   --  See Create_File_Exitor.
   --  Position indicates the position to give to the editor in the MDI.
   --  If Force is true, then the file is reloaded without asking confirmation
   --  from the user
   --  Initial_Dir is the initial directory to create the file in, in case
   --  we are creating an editor for a new file.

end Src_Editor_Module;
