-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;        use Glib;
with Glib.Values; use Glib.Values;
with Glib.Object; use Glib.Object;

with Gdk.Pixbuf; use Gdk.Pixbuf;

with Gtk;                      use Gtk;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Toolbar;              use Gtk.Toolbar;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Window;               use Gtk.Window;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;

with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;

with Ada.Text_IO; use Ada.Text_IO;

with Log_Editor_Window_Pkg; use Log_Editor_Window_Pkg;

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with System; use System;

with GUI_Utils;    use GUI_Utils;

with VCS;
with VCS.CVS;

with VCS_View_Pixmaps; use VCS_View_Pixmaps;

with Glide_Intl; use Glide_Intl;

package body VCS_View_Pkg is

   ------------------
   --  Local types --
   ------------------

   type Explorer_And_Log is record
      Explorer   : VCS_View_Access;
      Log_Editor : Log_Editor_Window_Access;
   end record;
   --  This type is a convenience type used to pass parameters to some
   --  callbacks.

   package Explorer_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Explorer_And_Log);

   type Iter_Action is access
     procedure (Explorer : access VCS_View_Record'Class;
                Iter     : Gtk_Tree_Iter);
   --  Any action that occurs on one row in the tree view.

   --------------------
   -- Local packages --
   --------------------

   function Boolean_Get is new Model_Data_Get (Boolean);
   function GObject_Get is new Model_Data_Get (GObject);

   package Selection_Callback is
      new Gtk.Handlers.Callback (Gtk_Tree_Selection_Record);

   package Explorer_Selection_Foreach is
      new Selection_Foreach (VCS_View_Access);
   use Explorer_Selection_Foreach;

   use VCS.String_List;

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Name_Column               : constant := 0;
   Local_Rev_Column          : constant := 1;
   Rep_Rev_Column            : constant := 2;
   Status_Description_Column : constant := 3;
   Status_Pixbuf_Column      : constant := 4;
   Log_Column                : constant := 5;
   Log_Editor_Column         : constant := 6;
   Log_Editable_Column       : constant := 7;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Name_Column               => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type,
         Log_Column                => GType_String,
         Log_Editor_Column         => GType_Object,
         Log_Editable_Column       => GType_Boolean);
      --  The Log_Editor_Column should contain a procedure which returns the
      --  log string given a filename.
      --  ??? This is a bit dirty, we are passing a GObject to the underlying C
      --  structure,
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   type Message_Type is (Info, Error, Verbose);
   --  We are dealing with 3 types of messages :
   --   - Info for general information
   --   - Error for signaling errors
   --   - Verbose for detailed information

   procedure Idle;
   --  This procedure will be called whenever the process in the background is
   --  waiting for something (command line output, network connection, etc)

   procedure Create_Model (VCS_View : access VCS_View_Record'Class);
   --  Creates the underlying tree model for VCS_View.

   procedure Set_Column_Types (Explorer : access VCS_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Fill_Info
     (Explorer      : access VCS_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Status_Record : File_Status_Record;
      Selected      : Boolean := False;
      Success       : out Boolean);
   --  Fills the tree info at the given Iter with values from
   --  Status_Record.
   --  Success tells whether the information has been filled or not.

   procedure Launch_Viewer
     (Explorer : access VCS_View_Record'Class;
      Strings  : in out List;
      Title    : String := "");
   --  Display a String_List.
   --  Strings is freed by that procedure.

   procedure Launch_Editor
     (Explorer : access VCS_View_Record'Class;
      Filename : String);
   --  Launch an editor for the given file.

   procedure Push_Message
     (Explorer : access VCS_View_Record'Class;
      M_Type   : Message_Type;
      Message  : String);
   --  Display a message in the Message_Text.

   procedure Handle_VCS_Error
     (Message  : String;
      Explorer : Gtk_Widget);
   --  Handle the error message output by VCS operations.

   procedure
     Display_String_List
     (Explorer : access VCS_View_Record'Class;
      List     : VCS.String_List.List;
      M_Type   : Message_Type);
   --  Convenience procedure to display a String_List.List.

   procedure Refresh_Files (Explorer : access VCS_View_Record'Class;
                            Connect  : Boolean := False);
   --  Display the relevant entries in the local directory.

   procedure Set_Directory (Explorer  : access VCS_View_Record'Class;
                            Directory : String);
   --  Sets the current directory to Directory.
   --  Directory must be an absolute directory name ending
   --  with Directory_Separator. If Directory is invalid, then
   --  the explorer will be set to the current directory.
   --  This procedure will also look for an acceptable VCS system for this
   --  directory.

   function Get_Selected_Files
     (Explorer : access VCS_View_Record'Class)
     return VCS.String_List.List;
   --  Return the list of files that are selected.

   procedure Foreach_Selected_File
     (Explorer : access VCS_View_Record'Class;
      Action   : Iter_Action);
   --  Run the Action for each of the selected file.

   function Get_Iter_From_Name
     (Explorer : access VCS_View_Record'Class;
      Name     : String)
     return Gtk_Tree_Iter;
   --  Return the Iter associated with the given name.
   --  Name is a base file name.
   --  Return Null_Iter if no such iter was found.

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Edit_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Edit_Multiple_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_View_Diff_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Annotate_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_View_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Get_Status_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Update_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Commit_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Revert_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Add_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure On_Remove_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  ???

   procedure Log_Editor_Text_Changed
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Explorer_And_Log);
   --  ???

   procedure Log_Editor_Ok_Clicked
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Explorer_And_Log);
   --  ???

   procedure Edited_Callback
     (Object      : access Gtk_Widget_Record'Class;
      Params      : Glib.Values.GValues);
   --  ???

   -------------------
   -- Launch_Viewer --
   -------------------

   procedure Launch_Viewer
     (Explorer : access VCS_View_Record'Class;
      Strings  : in out List;
      Title    : String := "") is
   begin
      while not Is_Empty (Strings) loop
         Put_Line (Head (Strings));
         Tail (Strings);
      end loop;
   end Launch_Viewer;

   -------------------
   -- Launch_Editor --
   -------------------

   procedure Launch_Editor
     (Explorer : access VCS_View_Record'Class;
      Filename : String) is
   begin
      Put_Line ("glide " & Filename);
   end Launch_Editor;

   -------------------------
   -- Display_String_List --
   -------------------------

   procedure Display_String_List
     (Explorer : access VCS_View_Record'Class;
      List     : VCS.String_List.List;
      M_Type   : Message_Type)
   is
      Temp_List : VCS.String_List.List := List;
   begin
      while not Is_Empty (Temp_List) loop
         Push_Message (Explorer, M_Type, "   "  & Head (List));
         Temp_List := Next (Temp_List);
      end loop;
   end Display_String_List;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer      : access VCS_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Status_Record : File_Status_Record;
      Selected      : Boolean := False;
      Success       : out Boolean) is
   begin
      Success := True;

      if Is_Empty (Status_Record.File_Name)
        or else Is_Directory (Head (Status_Record.File_Name)) then
         Success := False;
         return;
      end if;

      Set (Explorer.Model, Iter, Name_Column,
           Base_Name (Head (Status_Record.File_Name)));

      if not Is_Empty (Status_Record.Working_Revision) then
         Set (Explorer.Model, Iter, Local_Rev_Column,
              Head (Status_Record.Working_Revision));
      else
         Set (Explorer.Model, Iter, Local_Rev_Column, -"n/a");
      end if;

      if not Is_Empty (Status_Record.Repository_Revision) then
         Set (Explorer.Model, Iter, Rep_Rev_Column,
              Head (Status_Record.Repository_Revision));
      else
         Set (Explorer.Model, Iter, Rep_Rev_Column, -"n/a");
      end if;

      case Status_Record.Status is
         when Unknown =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Unknown_Pixbuf));
         when Not_Registered =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Not_Registered_Pixbuf));
         when Up_To_Date =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Up_To_Date_Pixbuf));
         when Removed =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Removed_Pixbuf));
         when Modified =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Modified_Pixbuf));
         when Needs_Merge =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Needs_Merge_Pixbuf));
         when Needs_Update =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Needs_Update_Pixbuf));
      end case;

      Set (Explorer.Model, Iter, Status_Description_Column,
           File_Status'Image (Status_Record.Status));
      Set (Explorer.Model, Iter, Log_Column, "");
      Set (Explorer.Model, Iter, Log_Editor_Column, System.Null_Address);
      Set (Explorer.Model, Iter, Log_Editable_Column, True);
   end Fill_Info;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access VCS_View_Record'Class;
      Name     : String) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Get_Iter_Root (Explorer.Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Explorer.Model, Iter, Name_Column) = Name then
            return Iter;
         end if;

         Next (Explorer.Model, Iter);
      end loop;

      return Null_Iter;
   end Get_Iter_From_Name;

   ---------------------------
   -- Foreach_Selected_File --
   ---------------------------

   procedure Foreach_Selected_File
     (Explorer : access VCS_View_Record'Class;
      Action   : Iter_Action)
   is
      procedure On_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : VCS_View_Access);
      --  Launch the Action for one item.

      procedure On_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : VCS_View_Access)
      is
      begin
         Action (Explorer, Iter);
      end On_Selected_Item;

   begin
      Selected_Foreach (Get_Selection (Explorer.Tree),
                        On_Selected_Item'Unrestricted_Access,
                        VCS_View_Access (Explorer));
   end Foreach_Selected_File;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : access VCS_View_Record'Class) return VCS.String_List.List
   is
      Result  : List;

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : VCS_View_Access);
      --  Add an item to Result.

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : VCS_View_Access)
      is
      begin
         Append (Result, Explorer.Current_Directory.all
                 & Get_String (Explorer.Model, Iter, Name_Column));
      end Add_Selected_Item;

   begin
      Selected_Foreach (Get_Selection (Explorer.Tree),
                        Add_Selected_Item'Unrestricted_Access,
                        VCS_View_Access (Explorer));
      return Result;
   end Get_Selected_Files;

   -------------------
   -- Refresh_Files --
   -------------------

   procedure Refresh_Files
     (Explorer : access VCS_View_Record'Class;
      Connect  : Boolean := False)
   is
      Iter           : Gtk_Tree_Iter;
      L              : File_Status_List.List;
      Directory_List : List;
      Success        : Boolean := True;

      use File_Status_List;

   begin
      if Explorer.Current_Directory = null then
         return;
      end if;

      Clear (Explorer.Model);

      Append (Directory_List, Explorer.Current_Directory.all);

      if Connect then
         L := Get_Status (Explorer.VCS_Ref, Directory_List);
      else
         L := Local_Get_Status (Explorer.VCS_Ref, Directory_List);
      end if;

      while not Is_Empty (L) loop
         Append (Explorer.Model, Iter, Null_Iter);
         Fill_Info (Explorer, Iter, Head (L), False, Success);

         if not Success then
            Remove (Explorer.Model, Iter);
         end if;

         Tail (L);
      end loop;

      Columns_Autosize (Explorer.Tree);
   end Refresh_Files;

   ------------------
   -- Push_Message --
   ------------------

   procedure Push_Message
     (Explorer : access VCS_View_Record'Class;
      M_Type   : Message_Type;
      Message  : String) is
   begin
      --  ??? Right now we display any message.
      --  Later, we may want to be able to
      --    - use a different color for error messages
      --    - let the user decide what kind of message he wants
      --    - pass the message to the glide console instead
      --  and so on...

      if M_Type = Error then
         Insert (Explorer.Message_Text,
                 Chars => -"    Error : ");
      end if;

      Insert (Explorer.Message_Text,
              Chars => Message & ASCII.LF);
   end Push_Message;

   ---------------------------
   -- Log_Editor_Text_Changed --
   ---------------------------

   procedure Log_Editor_Text_Changed
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Explorer_And_Log)
   is
      Temp_Path : List := Parameter.Log_Editor.Files;
      Iter      : Gtk_Tree_Iter;
   begin
      while not Is_Empty (Temp_Path) loop
         Iter := Get_Iter_From_Name
           (Parameter.Explorer,
            Head (Temp_Path));

         if Iter /= Null_Iter then
            Set (Parameter.Explorer.Model, Iter, Log_Column,
                 Get_Text (Parameter.Log_Editor));
         end if;

         Temp_Path := Next (Temp_Path);
      end loop;
   end Log_Editor_Text_Changed;

   ---------------------------
   -- Log_Editor_Ok_Clicked --
   ---------------------------

   procedure Log_Editor_Ok_Clicked
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Explorer_And_Log)
   is
      Value      : GValue;
      Iter       : Gtk_Tree_Iter;
      Temp_Paths : List := Parameter.Log_Editor.Files;

   begin
      Init (Value, GType_String);

      while not Is_Empty (Temp_Paths) loop
         Iter := Get_Iter_From_Name
           (Parameter.Explorer,
            Head (Temp_Paths));

         if Iter /= Null_Iter then
            Set
              (Parameter.Explorer.Model,
               Iter,
               Log_Editor_Column,
               System.Null_Address);
         end if;

         Temp_Paths := Next (Temp_Paths);
      end loop;

      Close (Parameter.Log_Editor);
   end Log_Editor_Ok_Clicked;

   --------------------------------
   -- On_Edit_Log_Button_Clicked --
   --------------------------------

   procedure On_Edit_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer          : constant VCS_View_Access := VCS_View_Access (Object);
      No_Files_Selected : Boolean := True;

      procedure Create_And_Launch_Log_Editor
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter);
      --  Creates a log editor for the given row,
      --  displays it, and connects the necessary callbacks.

      procedure Create_And_Launch_Log_Editor
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter)
      is
         Stored_Object    : GObject;
         Parameter_Object : Explorer_And_Log;
         Log_Editor       : Log_Editor_Window_Access;

      begin
         No_Files_Selected := False;

         Stored_Object :=
           GObject_Get (Explorer.Model, Iter, Log_Editor_Column);

         if Stored_Object = null then
            Gtk_New (Log_Editor);
            --  Parameter_Object := new Explorer_And_Log_Record;

            Parameter_Object.Explorer := VCS_View_Access (Explorer);
            Parameter_Object.Log_Editor := Log_Editor;

            Set_Title (Log_Editor,
                       -"Log editor for "
                       & Get_String (Explorer.Model, Iter, Name_Column));

            Add_File_Name (Log_Editor,
                           Get_String (Explorer.Model, Iter, Name_Column));

            Set (Explorer.Model,
                 Iter,
                 Log_Editor_Column,
                 Get_Object (Log_Editor));

            Set_Text (Log_Editor,
                      Get_String (Explorer.Model, Iter, Log_Column));

            Explorer_Callback.Connect
              (Log_Editor.Ok_Button,
               "clicked",
               Explorer_Callback.To_Marshaller (Log_Editor_Ok_Clicked'Access),
               Parameter_Object);

            Explorer_Callback.Connect
              (Log_Editor.Log_Text,
               "insert_text",
               Explorer_Callback.To_Marshaller
                (Log_Editor_Text_Changed'Access),
               Parameter_Object,
               After => True);

            Explorer_Callback.Connect
              (Log_Editor.Log_Text,
               "destroy",
               Explorer_Callback.To_Marshaller (Log_Editor_Ok_Clicked'Access),
               Parameter_Object);

            if Explorer.Kernel = null then
               Show_All (Log_Editor);
            else
               declare
                  Child : MDI_Child;
               begin
                  Child := Put (Get_MDI (Explorer.Kernel), Log_Editor);
               end;
            end if;
         end if;
      end Create_And_Launch_Log_Editor;

   begin
      if Explorer.Current_Directory = null then
         return;
      end if;

      Foreach_Selected_File
        (Explorer, Create_And_Launch_Log_Editor'Unrestricted_Access);

      if No_Files_Selected then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;
   end On_Edit_Log_Button_Clicked;

   -----------------------------------------
   -- On_Edit_Multiple_Log_Button_Clicked --
   -----------------------------------------

   procedure On_Edit_Multiple_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer          : constant VCS_View_Access := VCS_View_Access (Object);
      Stub              : Log_Editor_Window_Record;
      Log_Editor        : Log_Editor_Window_Access;
      Parameter_Object  : Explorer_And_Log;
      No_Files_Selected : Boolean := True;

      procedure Clear_Launch_Log_Editor
        (View     : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter);
      --  Removes any log_editor attached to the given row.

      procedure Fill_Launch_Log_Editor
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter);
      --  Adds the necessary information to the current
      --  log_editor.

      procedure Clear_Launch_Log_Editor
        (View     : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter)
      is
         Stored_Object : GObject;
      begin
         No_Files_Selected := False;

         Stored_Object :=
           GObject_Get (Explorer.Model, Iter, Log_Editor_Column);

         if Stored_Object /= null then
            Log_Editor := Log_Editor_Window_Access
              (Get_User_Data (Stored_Object.all'Address, Stub));

            Log_Editor_Ok_Clicked (Object, (Explorer, Log_Editor));
         end if;
      end Clear_Launch_Log_Editor;

      procedure Fill_Launch_Log_Editor
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter) is
      begin
         Add_File_Name
           (Log_Editor, Get_String (Explorer.Model, Iter, Name_Column));

         Set
           (Explorer.Model,
            Iter,
            Log_Editor_Column,
            Get_Object (Log_Editor));
      end Fill_Launch_Log_Editor;

   begin
      if Explorer.Current_Directory = null then
         return;
      end if;

      Foreach_Selected_File
        (Explorer, Clear_Launch_Log_Editor'Unrestricted_Access);

      if No_Files_Selected then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      --  Create the log editor.
      Gtk_New (Log_Editor);

      Set_Title (Log_Editor, -"Multiple log editor");
      Set_Text (Log_Editor, "");

      Parameter_Object.Explorer := Explorer;
      Parameter_Object.Log_Editor := Log_Editor;

      --  Associate the log editor to all files.
      Foreach_Selected_File
        (Explorer, Fill_Launch_Log_Editor'Unrestricted_Access);

      Explorer_Callback.Connect
        (Log_Editor.Ok_Button,
         "clicked",
         Explorer_Callback.To_Marshaller (Log_Editor_Ok_Clicked'Access),
         Parameter_Object);

      Explorer_Callback.Connect
        (Log_Editor.Log_Text,
         "insert_text",
         Explorer_Callback.To_Marshaller (Log_Editor_Text_Changed'Access),
         Parameter_Object,
         After => True);

      if Explorer.Kernel = null then
         Show_All (Log_Editor);
      else
         declare
            Child : MDI_Child;
         begin
            Child := Put (Get_MDI (Explorer.Kernel), Log_Editor);
         end;
      end if;
   end On_Edit_Multiple_Log_Button_Clicked;

   --------------------------------
   -- On_View_Diff_Button_Clicked --
   --------------------------------

   procedure On_View_Diff_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer         : constant VCS_View_Access := VCS_View_Access (Object);
      L                : List := Get_Selected_Files (Explorer);
      L_Temp           : List := L;
      Temp_String_List : List;

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer,
                    Verbose,
                    -"Viewing diffs for files:");

      Display_String_List (Explorer, L, Verbose);

      while not Is_Empty (L_Temp) loop
         Temp_String_List := Diff (Explorer.VCS_Ref, Head (L_Temp));
         Launch_Viewer (Explorer,
                        Temp_String_List,
                        -"Diff for current revision of " & Head (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
   end On_View_Diff_Button_Clicked;

   --------------------------------
   -- On_Annotate_Button_Clicked --
   --------------------------------

   procedure On_Annotate_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer         : constant VCS_View_Access := VCS_View_Access (Object);
      L                : List := Get_Selected_Files (Explorer);
      L_Temp           : List := L;
      Temp_String_List : List;

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer,
                    Verbose,
                    -"Annotating files:");

      Display_String_List (Explorer, L, Verbose);

      while not Is_Empty (L_Temp) loop
         Temp_String_List := Annotate (Explorer.VCS_Ref, Head (L_Temp));
         Launch_Viewer (Explorer,
                        Temp_String_List,
                        -"Annotating of " & Head (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
   end On_Annotate_Button_Clicked;

   --------------------------------
   -- On_View_Log_Button_Clicked --
   --------------------------------

   procedure On_View_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer         : constant VCS_View_Access := VCS_View_Access (Object);
      L                : List := Get_Selected_Files (Explorer);
      L_Temp           : List := L;
      Temp_String_List : List;

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer,
                    Verbose,
                    -"Viewing logs of files:");

      Display_String_List (Explorer, L, Verbose);

      while not Is_Empty (L_Temp) loop
         Temp_String_List := Log (Explorer.VCS_Ref, Head (L_Temp));
         Launch_Viewer (Explorer, Temp_String_List);
         L_Temp := Next (L_Temp);
      end loop;

      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
   end On_View_Log_Button_Clicked;

   ----------------------------------
   -- On_Get_Status_Button_Clicked --
   ----------------------------------

   procedure On_Get_Status_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (Object);
      L        : List := Get_Selected_Files (Explorer);

   begin
      if Is_Empty (L) then
         if Explorer.Current_Directory = null then
            Explorer.Current_Directory := new String' (Get_Current_Dir);
         end if;

         Push_Message (Explorer,
                       Verbose,
                       -"Querying status for files in directory "
                       & Explorer.Current_Directory.all
                       & " ... ");
         Set_Busy_Cursor (Get_Window (Explorer), True, True);
         Refresh_Files (Explorer, True);
         Set_Busy_Cursor (Get_Window (Explorer), False);
         Push_Message (Explorer, Info, -"... done." & ASCII.LF);
      else
         Push_Message (Explorer,
                       Verbose,
                       -"Querying status for files:");

         Display_String_List (Explorer, L, Verbose);

         declare
            Iter   : Gtk_Tree_Iter;
            Result : File_Status_List.List := Get_Status (Explorer.VCS_Ref, L);
            Dummy  : Boolean;

         begin
            while not File_Status_List.Is_Empty (Result) loop
               Iter := Get_Iter_From_Name
                 (Explorer,
                  Base_Name
                    (Head (File_Status_List.Head (Result).File_Name)));

               if Iter /= Null_Iter then
                  Fill_Info (Explorer,
                             Iter,
                             File_Status_List.Head (Result),
                             True,
                             Dummy);
               end if;

               File_Status_List.Tail (Result);
            end loop;

            Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
         end;
      end if;

      Free (L);
   end On_Get_Status_Button_Clicked;

   ------------------------------
   -- On_Update_Button_Clicked --
   ------------------------------

   procedure On_Update_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (Object);
      L        : List := Get_Selected_Files (Explorer);

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer, Verbose, -"Updating files:");
      Display_String_List (Explorer, L, Verbose);
      Update (Explorer.VCS_Ref, L);
      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
      Free (L);
      On_Get_Status_Button_Clicked (Object, Params);
   end On_Update_Button_Clicked;

   ----------------------------
   -- On_Open_Button_Clicked --
   ----------------------------

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (Object);
      L        : List := Get_Selected_Files (Explorer);

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer, Verbose, -"Opening files:");
      Display_String_List (Explorer, L, Verbose);

      Open (Explorer.VCS_Ref, L);

      declare
         L_Temp : List := L;
      begin
         while not Is_Empty (L_Temp) loop
            Launch_Editor (Explorer, Head (L_Temp));
            L_Temp := Next (L_Temp);
         end loop;
      end;

      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
      Free (L);
      On_Get_Status_Button_Clicked (Object, Params);
   end On_Open_Button_Clicked;

   ------------------------------
   -- On_Commit_Button_Clicked --
   ------------------------------

   procedure On_Commit_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer   : constant VCS_View_Access := VCS_View_Access (Object);
      L          : List := Get_Selected_Files (Explorer);
      Files_List : List;
      Logs_List  : List;

      procedure Check_File
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter);
      --  Check that a given file has a log associated to it,
      --  and display an error message if not.

      procedure Check_File
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter)
      is
         Log  : String := Get_String (Explorer.Model, Iter, Log_Column);
         Name : String := Get_String (Explorer.Model, Iter, Name_Column);

      begin
         if Log = "" then
            Push_Message
              (Explorer, Error,
               -"You must provide a log before committing file " & Name);

         else
            Append (Files_List, Explorer.Current_Directory.all & Name);
            Append (Logs_List, Log);
         end if;
      end Check_File;

   begin
      if Explorer.Current_Directory = null then
         return;
      end if;

      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer, Verbose, -"Committing files:");

      Display_String_List (Explorer, L, Verbose);

      Foreach_Selected_File
        (Explorer, Check_File'Unrestricted_Access);

      if not Is_Empty (Files_List) then
         Commit (Explorer.VCS_Ref, Files_List, Logs_List);
      end if;

      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
      Free (L);

      if not Is_Empty (Files_List) then
         On_Get_Status_Button_Clicked (Object, Params);
      end if;

      Free (Files_List);
      Free (Logs_List);
   end On_Commit_Button_Clicked;

   ------------------------------
   -- On_Revert_Button_Clicked --
   ------------------------------

   procedure On_Revert_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) is
   begin
      null;
   end On_Revert_Button_Clicked;

   ---------------------------
   -- On_Add_Button_Clicked --
   ---------------------------

   procedure On_Add_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (Object);
      L        : List := Get_Selected_Files (Explorer);

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer, Verbose, -"Adding files:");
      Display_String_List (Explorer, L, Verbose);
      Add (Explorer.VCS_Ref, L);
      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
      Free (L);
      On_Get_Status_Button_Clicked (Object, Params);
   end On_Add_Button_Clicked;

   ------------------------------
   -- On_Remove_Button_Clicked --
   ------------------------------

   procedure On_Remove_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (Object);
      L        : List := Get_Selected_Files (Explorer);

   begin
      if Is_Empty (L) then
         Push_Message (Explorer, Error, -"No files are selected.");
         return;
      end if;

      Push_Message (Explorer, Verbose, -"Removing files:");
      Display_String_List (Explorer, L, Verbose);

      Remove (Explorer.VCS_Ref, L);

      Push_Message (Explorer, Verbose, -"... done." & ASCII.LF);
      Free (L);
      On_Get_Status_Button_Clicked (Object, Params);
   end On_Remove_Button_Clicked;

   ----------
   -- Idle --
   ----------

   procedure Idle is
      No_Main_Loop : Boolean;
   begin
      while Gtk.Main.Events_Pending loop
         No_Main_Loop := Gtk.Main.Main_Iteration;
      end loop;
   end Idle;

   ----------------------
   -- Handle_VCS_Error --
   ----------------------

   procedure Handle_VCS_Error
     (Message  : String;
      Explorer : Gtk_Widget) is
   begin
      Push_Message (VCS_View_Access (Explorer), Error, Message);
   end Handle_VCS_Error;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory
     (Explorer  : access VCS_View_Record'Class;
      Directory : String) is
   begin
      if Explorer.Current_Directory /= null then
         Free (Explorer.Current_Directory);
      end if;

      if Is_Absolute_Path (Directory)
        and then Is_Directory (Directory)
        and then Directory (Directory'Last) = Directory_Separator
      then
         Explorer.Current_Directory := new String'(Directory);
      else
         Explorer.Current_Directory := new String'(Get_Current_Dir);
      end if;

      --  Find an acceptable VCS for this directory.
      --  ??? right now, we assume only CVS is implemented.
      --  ??? we need functions in VCS.XXX to validate that a
      --  given entry is acceptable for a given directory.

      Explorer.VCS_Ref := new VCS.CVS.CVS_Record;

      Register_Idle_Function (Explorer.VCS_Ref, Idle'Access, 200);

      Register_Error_Function
        (Explorer.VCS_Ref, Handle_VCS_Error'Access, Gtk_Widget (Explorer));
   end Set_Directory;

   ----------------
   -- Show_Files --
   ----------------

   procedure Show_Files
     (Explorer  : VCS_View_Access;
      Directory : String) is
   begin
      Set_Directory (Explorer, Directory);
      Refresh_Files (Explorer, False);
   end Show_Files;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Object      : access Gtk_Widget_Record'Class;
      Params      : Glib.Values.GValues)
   is
      Explorer      : constant VCS_View_Access := VCS_View_Access (Object);
      Iter          : Gtk_Tree_Iter;
      Path_String   : String := Get_String (Nth (Params, 1));
      Text_String   : String := Get_String (Nth (Params, 2));
      Text_Value    : GValue := Nth (Params, 2);
      Stub          : Log_Editor_Window_Record;
      Log_Editor    : Log_Editor_Window_Access := null;
      Stored_Object : GObject;

   begin
      Iter := Get_Iter_From_String (Explorer.Model, Path_String);

      if Iter = Null_Iter then
         return;
      end if;

      Set_Value (Explorer.Model, Iter, Log_Column, Text_Value);

      Stored_Object := GObject_Get (Explorer.Model, Iter, Log_Editor_Column);

      if Stored_Object /= null then
         Log_Editor := Log_Editor_Window_Access
           (Get_User_Data (Stored_Object.all'Address, Stub));
         Set_Text (Log_Editor, Text_String);
      end if;
   end Edited_Callback;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_View_Record'Class) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Editable_Rend : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Toggle_Rend   : Gtk_Cell_Renderer_Toggle;
      Dummy         : Gint;

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Editable_Rend);
      Gtk_New (Toggle_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Explorer.Tree, True);

      Gtk_New (Col);
      Set_Title (Col, -"Local file name");
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Status_Pixbuf_Column);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Name_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Local revision");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Local_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Repository revision");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Rep_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Log");
      Pack_Start (Col, Editable_Rend, True);
      Add_Attribute (Col, Editable_Rend, "text", Log_Column);
      Add_Attribute (Col, Editable_Rend, "editable", Log_Editable_Column);

      Dummy := Append_Column (Explorer.Tree, Col);

      Widget_Callback.Object_Connect
        (Editable_Rend,
         "edited",
         Edited_Callback'Access,
         Gtk_Widget (Explorer));
   end Set_Column_Types;

   -------------------
   --  Create_Model --
   -------------------

   procedure Create_Model (VCS_View : access VCS_View_Record'Class) is
   begin
      Gtk_New (VCS_View.Model, Columns_Types'Length, Columns_Types);
   end Create_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (VCS_View : out VCS_View_Access;
                      Kernel   : Kernel_Handle := null)
   is
   begin
      Init_Graphics;

      VCS_View := new VCS_View_Record;
      VCS_View_Pkg.Initialize (VCS_View);
      VCS_View.Kernel := Kernel;

      Show_Files (VCS_View, "");
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (VCS_View : access VCS_View_Record'Class) is
      Vbox1           : Gtk_Vbox;
      Hbox1           : Gtk_Hbox;
      Hbox2           : Gtk_Hbox;
      Toolbar2        : Gtk_Toolbar;
      Toolbar1        : Gtk_Toolbar;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Selection       : Gtk_Tree_Selection;

   begin
      Gtk.Window.Initialize (VCS_View, Window_Toplevel);
      Set_Default_Size (VCS_View, 600, 600);

      Set_Policy (VCS_View, False, True, False);

      Gtk_New_Vbox (Vbox1, False, 0);
      Add (VCS_View, Vbox1);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Vbox1, Hbox1, True, True, 3);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1, Policy_Automatic, Policy_Automatic);
      Pack_Start (Hbox1, Scrolledwindow1, True, True, 3);

      Create_Model (VCS_View);

      Gtk_New (VCS_View.Tree, VCS_View.Model);

      Selection := Get_Selection (VCS_View.Tree);

      Set_Mode (Selection, Selection_Multiple);

      Add (Scrolledwindow1, VCS_View.Tree);

      --  Set columns types for the Tree.

      Set_Column_Types (VCS_View);

      Gtk_New (Toolbar2, Orientation_Vertical, Toolbar_Both);
      Set_Tooltips (Toolbar2, True);
      VCS_View.Edit_Log_Button := Append_Element
        (Toolbar  => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text     => -"Edit Log");
      Widget_Callback.Object_Connect
        (VCS_View.Edit_Log_Button, "clicked",
         On_Edit_Log_Button_Clicked'Access,
         VCS_View);

      VCS_View.Edit_Multiple_Log_Button := Append_Element
        (Toolbar  => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text     => -"Edit multiple Log");
      Widget_Callback.Object_Connect
        (VCS_View.Edit_Multiple_Log_Button, "clicked",
         On_Edit_Multiple_Log_Button_Clicked'Access,
         VCS_View);

      VCS_View.View_Diff_Button := Append_Element
        (Toolbar  => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text     => -"View Diff");
      Widget_Callback.Object_Connect
        (VCS_View.View_Diff_Button, "clicked",
         On_View_Diff_Button_Clicked'Access,
         VCS_View);

      VCS_View.Annotate_Button := Append_Element
        (Toolbar => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text => -"Annotate");
      Widget_Callback.Object_Connect
        (VCS_View.Annotate_Button, "clicked",
         On_Annotate_Button_Clicked'Access,
         VCS_View);

      VCS_View.View_Log_Button := Append_Element
        (Toolbar  => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text     => -"View Log");
      Widget_Callback.Object_Connect
        (VCS_View.View_Log_Button, "clicked",
         On_View_Log_Button_Clicked'Access,
         VCS_View);
      Pack_Start (Hbox1, Toolbar2, False, False, 3);

      Gtk_New (Toolbar1, Orientation_Vertical, Toolbar_Both);
      Set_Tooltips (Toolbar1, True);
      VCS_View.Get_Status_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Get status");
      Widget_Callback.Object_Connect
        (VCS_View.Get_Status_Button, "clicked",
         On_Get_Status_Button_Clicked'Access,
         VCS_View);
      VCS_View.Update_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Update");
      Widget_Callback.Object_Connect
        (VCS_View.Update_Button, "clicked",
         On_Update_Button_Clicked'Access,
         VCS_View);
      VCS_View.Open_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Open");
      Widget_Callback.Object_Connect
        (VCS_View.Open_Button, "clicked", On_Open_Button_Clicked'Access,
         VCS_View);
      VCS_View.Commit_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Commit");
      Widget_Callback.Object_Connect
        (VCS_View.Commit_Button, "clicked", On_Commit_Button_Clicked'Access,
         VCS_View);
      VCS_View.Revert_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Revert");
      Widget_Callback.Object_Connect
        (VCS_View.Revert_Button, "add", On_Revert_Button_Clicked'Access,
         VCS_View);
      VCS_View.Add_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Add");
      Widget_Callback.Object_Connect
        (VCS_View.Add_Button, "clicked", On_Add_Button_Clicked'Access,
         VCS_View);
      VCS_View.Remove_Button := Append_Element
        (Toolbar  => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text     => -"Remove");
      Widget_Callback.Object_Connect
        (VCS_View.Remove_Button, "clicked", On_Remove_Button_Clicked'Access,
         VCS_View);
      Pack_Start (Hbox1, Toolbar1, False, False, 3);

      Gtk_New_Hbox (Hbox2, False, 0);
      Pack_Start (Vbox1, Hbox2, True, True, 3);

      Gtk_New (Scrolledwindow2);
      Set_Policy (Scrolledwindow2, Policy_Never, Policy_Always);
      Pack_Start (Hbox2, Scrolledwindow2, True, True, 3);

      Gtk_New (VCS_View.Message_Text);
      Set_Editable (VCS_View.Message_Text, False);
      Add (Scrolledwindow2, VCS_View.Message_Text);
   end Initialize;

end VCS_View_Pkg;

--  ??? known problems, missing features, etc
--  when there is nothing in the tree, clicking raises storage_error
