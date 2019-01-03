------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Basic_Types;                 use Basic_Types;
with Commands.Controls;
with GNAT.Expect;
with GNAT.Strings;
with GNATCOLL.Projects;
with GNATCOLL.Scripts;            use GNATCOLL.Scripts;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GPS.Customizable_Modules;    use GPS.Customizable_Modules;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;    use GPS.Kernel.Style_Manager;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Modules;          use GPS.Kernel.Modules;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Markers;                 use GPS.Markers;
with Glib;                        use Glib;
with Glib.Object;
with Gtk.Text_Buffer;             use Gtk.Text_Buffer;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.MDI;                  use Gtkada.MDI;
with HTables;
with Src_Contexts;
with Src_Editor_Box;
with System;
with XML_Utils;                   use XML_Utils;
with Pango.Font;

package Src_Editor_Module is

   Src_Editor_Module_Id : Module_ID;
   Src_Editor_Module_Name : constant String := "Source_Editor";

   Search_Result_Highlighting : constant String := "Search Results";

   ---------------------------------------
   -- Module-specific graphical objects --
   ---------------------------------------

   Hide_Block_Pixbuf    : constant String := "gps-fold-block-symbolic";
   Unhide_Block_Pixbuf  : constant String := "gps-unfold-block-symbolic";

   File_Pixbuf          : constant String := "gps-emblem-file-unmodified";
   File_Modified_Pixbuf : constant String := "gps-emblem-file-modified";
   File_Unsaved_Pixbuf  : constant String := "gps-emblem-file-unsaved";

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   function Find_Current_Editor
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Only_If_Focused : Boolean := False)
      return Gtkada.MDI.MDI_Child;
   --  Return the source editor that has currently the focus in the MDI.
   --  If the focus in the MDI is not set on a source editor, then the top most
   --  editor is returned if Only_If_Focused is False, or null when it's True.

   procedure For_All_Views
     (Kernel   : not null access Kernel_Handle_Record'Class;
      File     : Virtual_File;
      Callback : not null access procedure
        (Child : not null access GPS_MDI_Child_Record'Class));
   --  For all mdi children corresponding to an editor showing File
   --  (if File is No_File, returns all source editors).

   function Find_Editor
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type) return Gtkada.MDI.MDI_Child;
   --  Return the first child that contains an editor that edits file in
   --  the given project. This is the view that last had the focus, in
   --  case multiple views exist for this file.
   --  When using aggregate projects, we can end up with multiple views of the
   --  same file, each associated with a different project.
   --  null is returned if there are no such editor
   --  File can either be a file name or a buffer identifier.
   --  Project is used to deambiguate the view of the file, when using
   --  aggregate projects. It can be left to No_Project if you want to get any
   --  editor opening the file, whatever the project

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
      Current : Src_Editor_Box.Source_Editor_Box;
      Project : GNATCOLL.Projects.Project_Type)
      return Src_Editor_Box.Source_Editor_Box;
   --  Create a new view for Current and add it in the MDI.
   --  The current editor is the focus child in the MDI.
   --  The new_view will use the same project if Project is set to No_Project,
   --  or a specific project otherwise. This is useful in the context of
   --  aggregate projects.

   procedure Change_Undo_Redo_Queue (Queue : Standard.Commands.Command_Queue);
   --  Change the queue to which undo/redo apply.
   --  Set Queue to Null_Command_Queue to unset

   function Get_Undo_Redo_Queue return Standard.Commands.Command_Queue;
   --  Return the current Undo/Redo queue

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
      Project    : GNATCOLL.Projects.Project_Type;
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

   procedure On_Ed_View_Focus_Lost (Child : MDI_Child; File : Virtual_File);
   --  ???

   function Get_Project
     (Child : not null access MDI_Child_Record'Class)
      return GNATCOLL.Projects.Project_Type;
   --  Return the project associated with the Child, which should be a
   --  source editor view. Return No_Project if no project was found.

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

   subtype Weak_Location_Marker is GPS.Markers.Markers.Weak_Ref;
   package Marker_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Weak_Location_Marker, GPS.Markers.Markers."=");
   --  These do not hold a reference to the marker, which therefore has its
   --  own independent life cycle.

   package File_Marker_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (GNATCOLL.VFS.Virtual_File,
        Marker_List.List,
        GNATCOLL.VFS.Full_Name_Hash,
        GNATCOLL.VFS."=",
        Marker_List."=");

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

   Minimum_Character_Width : constant Gint := 1;

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Font                  : Pango.Font.Pango_Font_Description;
      Display_Line_Numbers  : Boolean    :=
         GPS.Kernel.Preferences.Display_Line_Numbers.Get_Pref /= Never;

      Character_Width       : Gint := Minimum_Character_Width;
      --  Width of the size column to display line numbers and breakpoint
      --  info. This is set to a minimum size so that we can always display
      --  breakpoint information.

      Show_Subprogram_Names : Boolean    := Display_Subprogram_Names.Get_Pref;

      Stored_Marks          : File_Marker_Maps.Map;
      --  Lists of markers for files.

      Recent_File_Actions   : Action_Lists.List;
      --  Actions registered dynamically for the list of recent files

      Categories            : Highlighting_Category_Array_Access;
      --  Contains a list of registered categories

      Editors               : Editors_Hash.Instance;
      Last_Focused_Editor   : Gtkada.MDI.MDI_Child;
      --  Pointer to editor that lost focus last

      Highlighting_Manager : System.Address := System.Null_Address;
      --  The highlighting manager

      --  The following fields are related to the current search

      Search_Context        : Src_Contexts.Files_Project_Context_Access;
      Search_File           : GNATCOLL.VFS.Virtual_File;
      Search_Pattern        : GNAT.Strings.String_Access;

      Undo_Redo             : Standard.Commands.Controls.Undo_Redo;
      --  Undo/redo controls

      --  The following fields are related to hyper mode

      Highlighters          : List_Of_Highlighters.List;
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   overriding procedure Destroy (Id : in out Source_Editor_Module_Record);
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
      Project    : GNATCOLL.Projects.Project_Type;
      Create_New : Boolean := True;
      Focus      : Boolean := True;
      Line       : Editable_Line_Type;
      Column     : Visible_Column_Type;
      Column_End : Visible_Column_Type;
      Group      : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic;
      Initial_Dir      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Areas            : Gtkada.MDI.Allowed_Areas := Gtkada.MDI.Central_Only;
      Title            : String := "";
      Is_Load_Desktop  : Boolean := False)
      return Src_Editor_Box.Source_Editor_Box;
   --  Open a file and return the handle associated with it.
   --  If Add_To_MDI is set to True, the box will be added to the MDI window.
   --  If Focus is True, the box will be raised if it is in the MDI.
   --  See Create_File_Exitor.
   --  Position indicates the position to give to the editor in the MDI.
   --  Initial_Dir is the initial directory to create the file in, in case
   --  we are creating an editor for a new file.
   --  Title can be specified to override the default title of the editor. It
   --  only applies when opening a new editor.

end Src_Editor_Module;
