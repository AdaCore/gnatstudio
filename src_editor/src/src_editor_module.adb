-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gint_Xml;                  use Gint_Xml;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Glide_Main_Window;         use Glide_Main_Window;
with GVD.Status_Bar;            use GVD.Status_Bar;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Src_Editor_Box;            use Src_Editor_Box;
with GNAT.Expect;               use GNAT.Expect;
with Traces;                    use Traces;

package body Src_Editor_Module is

   Me : Debug_Handle := Create ("Src_Editor_Module");

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   function Generate_Body_Timeout (Data : Process_Data) return Boolean;
   --  Callback for Process_Timeout, to handle gnatstub execution

   procedure Gtk_New
     (Box : out Source_Box; Editor : Source_Editor_Box);
   --  Create a new source box.

   procedure Initialize
     (Box : access Source_Box_Record'Class; Editor : Source_Editor_Box);
   --  Internal initialization function.

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure Save_To_File
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   function Get_Editor_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the filename of the last editor window that had the focus,
   --  "" if none.

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String := "";
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Open a file and return the handle associated with it.
   --  See Create_File_Exitor.

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Create a new text editor that edits File.
   --  If File is the empty string, or the file doesn't exist and Create_New is
   --  True, then an empty editor is created.
   --  No check is done to make sure that File is not already edited
   --  elsewhere. The resulting editor is not put in the MDI window.

   type Location_Idle_Data is record
      Edit : Source_Editor_Box;
      Line, Column : Natural;
   end record;

   package Location_Idle is new Gtk.Main.Idle (Location_Idle_Data);

   function Location_Callback (D : Location_Idle_Data) return Boolean;
   --  Idle callback used to scroll the source editors.

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open menu

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New View menu

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New menu

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save menu

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save As menu

   procedure On_Undo
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Undo menu

   procedure On_Redo
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Redo menu

   procedure On_Cut
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Cut menu

   procedure On_Copy
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Copy menu

   procedure On_Paste
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Paste menu

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Select All menu

   procedure On_Goto_Declaration_Or_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Declaration<->Body

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Generate Body menu

   procedure On_Pretty_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Pretty Print menu

   procedure On_Edit_File
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Edit a file (from a contextual menu)

   procedure Source_Editor_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Generate the contextual menu entries for contextual menus in other
   --  modules than the source editor.

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create the current context for Glide_Kernel.Get_Current_Context

   procedure New_View
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   function Find_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Return the top-most MDI child that is an internal editor

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget
   is
      Box : Source_Editor_Box;
      Src : Source_Box := null;
      File : Glib.String_Ptr;
   begin
      if Node.Tag.all = "Source_Editor" then
         File := Get_Field (Node, "File");
         if File /= null then
            Box := Create_File_Editor (User, File.all, False);
            Gtk_New (Src, Box);
            Attach (Box, Src);
         end if;

         return Gtk_Widget (Src);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N, Child : Node_Ptr;
   begin
      if Widget.all in Source_Box_Record'Class then
         N := new Node;
         N.Tag := new String' ("Source_Editor");

         Child := new Node;
         Child.Tag := new String' ("File");
         Child.Value := new String'
           (Get_Filename (Source_Box (Widget).Editor));
         Add_Child (N, Child);

         return N;
      end if;

      return null;
   end Save_Desktop;

   -------------------------
   -- Find_Current_Editor --
   -------------------------

   function Find_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return Source_Editor_Box
   is
      Child : MDI_Child := Find_Current_Editor (Kernel);
   begin
      if Child = null then
         return null;
      else
         return Source_Box (Get_Widget (Child)).Editor;
      end if;
   end Find_Current_Editor;

   -------------------------
   -- Find_Current_Editor --
   -------------------------

   function Find_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child is
   begin
      return Find_MDI_Child_By_Tag (Get_MDI (Kernel), Source_Box_Record'Tag);
   end Find_Current_Editor;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box    : out Source_Box;
      Editor : Source_Editor_Box) is
   begin
      Box := new Source_Box_Record;
      Initialize (Box, Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Box_Record'Class;
      Editor : Source_Editor_Box) is
   begin
      Gtk.Box.Initialize_Hbox (Box);
      Box.Editor := Editor;
   end Initialize;

   --------------
   -- New_View --
   --------------

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class)
   is
      MDI     : constant MDI_Window := Get_MDI (Kernel);
      Current : constant Source_Editor_Box := Find_Current_Editor (Kernel);
      Title   : constant String := Get_Filename (Current);
      Editor  : Source_Editor_Box;
      Box     : Source_Box;
      Child   : MDI_Child;

   begin
      if Current /= null then
         Create_New_View (Editor, Kernel, Current);
         Gtk_New (Box, Editor);
         Set_Size_Request
           (Box,
            Get_Pref (Kernel, Default_Widget_Width),
            Get_Pref (Kernel, Default_Widget_Height));
         Attach (Editor, Box);
         Child := Put (MDI, Box);
         --  ??? Should compute the right number.
         Set_Title (Child, Title & " <2>", Base_Name (Title) & " <2>");
      end if;
   end New_View;

   ------------------------
   -- Create_File_Editor --
   ------------------------

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box
   is
      Success     : Boolean;
      Editor      : Source_Editor_Box;
      File_Exists : Boolean := True;
   begin
      if File /= "" then
         File_Exists := Is_Regular_File (File);
      end if;

      --  Create a new editor only if the file exists or we are asked to
      --  create a new empty one anyway.
      if File_Exists or else Create_New then
         Gtk_New (Editor, Kernel_Handle (Kernel));
      end if;

      if File /= "" and then File_Exists then
         Load_File (Editor, File, Get_Language_Handler (Kernel),
                    Success => Success);
         if not Success then
            Destroy (Editor);
            Editor := null;
         end if;
      end if;

      return Editor;
   end Create_File_Editor;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String := "";
      Create_New : Boolean := True) return Source_Editor_Box
   is
      MDI         : constant MDI_Window := Get_MDI (Kernel);
      Short_File  : String := Base_Name (File);
      Editor      : Source_Editor_Box;
      Box         : Source_Box;
      Child       : MDI_Child;
      Iter        : Child_Iterator := First_Child (MDI);

   begin
      if File /= "" then
         --  ??? Should do a search on the full filename instead
         loop
            Child := Get (Iter);
            exit when Child = null
              or else Get_Title (Child) = File;
            Next (Iter);
         end loop;

         if Child /= null then
            Raise_Child (Child);
            Set_Focus_Child (Child);
            return Source_Box (Get_Widget (Child)).Editor;
         end if;
      end if;

      Editor := Create_File_Editor (Kernel, File, Create_New);

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Gtk_New (Box, Editor);
         Set_Size_Request
           (Box,
            Get_Pref (Kernel, Default_Widget_Width),
            Get_Pref (Kernel, Default_Widget_Height));
         Attach (Editor, Box);
         Child := Put (MDI, Box);

         if File /= "" then
            Set_Title (Child, File, Short_File);
         else
            Set_Title (Child, -"No Name");
         end if;

         Raise_Child (Child);

      else
         Console.Insert
           (Kernel, "Can not open file '" & File & "'",
            Highlight_Sloc => False);
      end if;

      return Editor;
   end Open_File;

   -----------------------
   -- Location_Callback --
   -----------------------

   function Location_Callback (D : Location_Idle_Data) return Boolean is
   begin
      Set_Cursor_Location (D.Edit, D.Line, D.Column);
      return False;
   end Location_Callback;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean)
   is
      Child : MDI_Child := Find_Current_Editor (Kernel);
      Source : Source_Editor_Box;
   begin
      if Child = null then
         return;
      end if;

      Source := Source_Box (Get_Widget (Child)).Editor;
      Save_To_File (Source, Name, Success);

      --  Update the title, in case "save as..." was used.
      Set_Title (Child, Base_Name (Get_Filename (Source)));
   end Save_To_File;

   -------------------------
   -- Get_Editor_Filename --
   -------------------------

   function Get_Editor_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String
   is
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source = null then
         return "";
      else
         return Get_Filename (Source);
      end if;
   end Get_Editor_Filename;

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant String := Select_File (Title => -"Open File");
      begin
         if Filename = "" then
            return;
         end if;

         Open_File_Editor (Kernel, Filename);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_File;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : Source_Editor_Box;
   begin
      Editor := Open_File (Kernel, File => "");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_File;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Undo;

   -------------
   -- On_Redo --
   -------------

   procedure On_Redo
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Redo;

   -------------
   -- On_Save --
   -------------

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Success : Boolean;
   begin
      Save_To_File (Kernel, Success => Success);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save;

   ----------------
   -- On_Save_As --
   ----------------

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Success : Boolean;
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source /= null then
         declare
            Name : constant String := Select_File
              (-"Save File As",
               Base_Directory => Dir_Name (Get_Filename (Source)));
         begin
            Save_To_File (Kernel, Name, Success);
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_As;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source /= null then
         Cut_Clipboard (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Cut;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source /= null then
         Copy_Clipboard (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Copy;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source /= null then
         Paste_Clipboard (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Paste;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source /= null then
         Select_All (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Select_All;

   -----------------
   -- On_New_View --
   -----------------

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      New_View (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_View;

   ---------------------------------
   -- On_Goto_Declaration_Or_Body --
   ---------------------------------

   procedure On_Goto_Declaration_Or_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      if Get_Editor_Filename (Kernel) /= "" then
         Push_State (Kernel, Busy);
         Goto_Declaration_Or_Body (Kernel);
         Pop_State (Kernel);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Declaration_Or_Body;

   ---------------------------
   -- Generate_Body_Timeout --
   ---------------------------

   function Generate_Body_Timeout (Data : Process_Data) return Boolean is
      function Body_Name (Name : String) return String;
      --  Return the name of the body corresponding to a spec file.

      function Body_Name (Name : String) return String is
      begin
         --  ??? Should ask the project module instead
         return Name (Name'First .. Name'Last - 1) & 'b';
      end Body_Name;

      Fd     : Process_Descriptor_Access := Data.Descriptor;
      Result : Expect_Match;
      Title  : String_Access := Data.Name;

   begin
      Expect (Fd.all, Result, "\n", Timeout => 1);

      if Result /= Expect_Timeout then
         Console.Insert (Data.Kernel, Expect_Out (Fd.all), Add_LF => False);
      end if;

      return True;

   exception
      when Process_Died =>
         Console.Insert
           (Data.Kernel, Expect_Out (Fd.all), Add_LF => False);
         Close (Fd.all);
         Free (Fd);
         Open_File_Editor (Data.Kernel, Body_Name (Title.all));
         Free (Title);
         Pop_State (Data.Kernel);
         return False;

      when E : others =>
         Close (Fd.all);
         Free (Fd);
         Free (Title);
         Pop_State (Data.Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Generate_Body_Timeout;

   ----------------------
   -- On_Generate_Body --
   ----------------------

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Timeout : constant Guint32 := 50;

      Success : Boolean;
      Title   : constant String := Get_Editor_Filename (Kernel);
      Id      : Timeout_Handler_Id;
      Fd      : Process_Descriptor_Access;

      procedure Gnatstub (Name : String; Success : out Boolean);
      --  Launch gnatstub process and wait for it.

      --------------
      -- Gnatstub --
      --------------

      procedure Gnatstub (Name : String; Success : out Boolean) is
         Args   : Argument_List (1 .. 1);
         Exec   : String_Access := Locate_Exec_On_Path ("gnatstub");

      begin
         if Exec = null then
            Success := False;
            return;
         end if;

         Fd := new Process_Descriptor;
         Args (1) := new String' (Name);
         Non_Blocking_Spawn (Fd.all, Exec.all, Args, Err_To_Out => True);
         Success := True;

         Free (Exec);
         Free (Args (1));

      exception
         when Invalid_Process =>
            Success := False;
      end Gnatstub;

   begin
      if Title /= "" then
         Push_State (Kernel, Processing);

         --  ??? Should check language first
         Gnatstub (Title, Success);

         if Success then
            Print_Message
              (Glide_Window (Get_Main_Window (Kernel)).Statusbar,
               Help, -"Generating body...");
            Id := Process_Timeout.Add
              (Timeout, Generate_Body_Timeout'Access,
               (Kernel, Fd, new String' (Title)));
         end if;
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Generate_Body;

   ---------------------
   -- On_Pretty_Print --
   ---------------------

   procedure On_Pretty_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Title : constant String := Get_Editor_Filename (Kernel);
   begin
      Push_State (Kernel, Processing);
      Trace (Me, "pretty printing " & Title);
      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Pretty_Print;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      pragma Unreferenced (Mode);
      Edit : Source_Editor_Box;
      Id : Idle_Handler_Id;
   begin
      if Mime_Type = Mime_Source_File then
         declare
            File      : constant String  := Get_String (Data (Data'First));
            Line      : constant Gint    := Get_Int (Data (Data'First + 1));
            Column    : constant Gint    := Get_Int (Data (Data'First + 2));
            Highlight : constant Boolean :=
              Get_Boolean (Data (Data'First + 3));
         begin
            Edit := Open_File (Kernel, File, Create_New => False);

            if Edit /= null
              and then (Line /= 0 or else Column /= 0)
            then
               --  For some reason, we can not directly call
               --  Set_Cursor_Location, since the source editor won't be
               --  scrolled the first time the editor is displayed. Doing
               --  this in an idle callback ensures that all the proper
               --  events and initializations have taken place before we try
               --  to scroll the editor.

               Id := Location_Idle.Add
                 (Location_Callback'Access,
                  (Edit, Natural (Line), Natural (Column)));

               if Highlight then
                  Highlight_Line (Edit, Natural (Line));
               end if;
            end if;

            return Edit /= null;
         end;
      end if;

      return False;
   end Mime_Action;

   ------------------
   -- On_Edit_File --
   ------------------

   procedure On_Edit_File
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      File : File_Name_Selection_Context_Access :=
        File_Name_Selection_Context_Access (Context);
   begin
      Trace (Me, "On_Edit_File: " & File_Information (File));
      Open_File_Editor
        (Get_Kernel (Context),
         Directory_Information (File) & File_Information (File));
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Edit_File;

   ------------------------------
   -- Source_Editor_Contextual --
   ------------------------------

   procedure Source_Editor_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      File  : File_Name_Selection_Context_Access;
      Mitem : Gtk_Menu_Item;
   begin
      if Context.all in File_Name_Selection_Context'Class then
         File := File_Name_Selection_Context_Access (Context);

         if Has_Directory_Information (File)
           and then Has_File_Information (File)
         then
            Gtk_New (Mitem, -"Edit " & Base_Name (File_Information (File)));
            Append (Menu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (On_Edit_File'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Source_Editor_Contextual;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      C : Source_Box := Source_Box (Child);
   begin
      return Get_Contextual_Menu (Kernel, Child, C.Editor, null, null);
   end Default_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File    : constant String := '/' & (-"File") & '/';
      Edit    : constant String := '/' & (-"Edit") & '/';
      Gotom   : constant String := '/' & (-"Navigate") & '/';
      Mitem   : Gtk_Menu_Item;
      Button  : Gtk_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);

      Undo_Redo : Undo_Redo_Information;
   begin
      Src_Editor_Module_Id := Register_Module
        (Kernel                  => Kernel,
         Module_Name             => Src_Editor_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Source_Editor_Contextual'Access,
         Mime_Handler            => Mime_Action'Access,
         MDI_Child_Tag           => Source_Box_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      --  Menus

      Register_Menu (Kernel, File, -"Open...",  Stock_Open,
                     On_Open_File'Access, GDK_F3, Ref_Item => -"Close");
      Register_Menu (Kernel, File & (-"Reopen"), Ref_Item => -"Open...",
                     Add_Before => False);
      Gtk_New (Mitem);
      Register_Menu
        (Kernel, File, Mitem, Ref_Item => -"Reopen", Add_Before => False);

      Register_Menu (Kernel, File, -"New",  Stock_New, On_New_File'Access,
                     Ref_Item => -"Open...");
      Register_Menu (Kernel, File, -"New View",  "", On_New_View'Access,
                     Ref_Item => -"Open...");

      Register_Menu (Kernel, File, -"Save",  Stock_Save, On_Save'Access,
                     GDK_S, Control_Mask, Ref_Item => -"Close");
      Register_Menu (Kernel, File, -"Save As...",  Stock_Save_As,
                     On_Save_As'Access, Ref_Item => -"Close");

      Undo_Redo.Undo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"Undo",  Stock_Undo,
                       On_Undo'Access, Ref_Item => -"Preferences");

      Undo_Redo.Redo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"Redo",  Stock_Redo,
                       On_Redo'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu
        (Kernel, Edit, Mitem, Ref_Item => "Redo", Add_Before => False);

      Register_Menu (Kernel, Edit, -"Cut",  Stock_Cut,
                     On_Cut'Access, GDK_Delete, Shift_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Copy",  Stock_Copy,
                     On_Copy'Access, GDK_Insert, Control_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Paste",  Stock_Paste,
                     On_Paste'Access, GDK_Insert, Shift_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Select All",  "",
                     On_Select_All'Access, GDK_A, Control_Mask,
                     Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"Generate Body", "",
                     On_Generate_Body'Access, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Pretty Print", "",
                     On_Pretty_Print'Access, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Gotom, -"Goto Declaration<->Body", Stock_Home,
                     On_Goto_Declaration_Or_Body'Access,
                     Ref_Item => -"Goto Line...");

      --  ??? Not implemented yet
      Register_Menu (Kernel, Gotom, -"Goto Body", "", null,
                     Ref_Item => -"Goto Declaration<->Body");

      --  Toolbar buttons

      Button := Insert_Stock
        (Toolbar, Stock_New, -"Create a New File", Position => 0);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_New_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Open, -"Open a File", Position => 1);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Open_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Save, -"Save Current File", Position => 2);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Save'Access),
         Kernel_Handle (Kernel));

      Insert_Space (Toolbar, Position => 3);
      Undo_Redo.Undo_Button := Insert_Stock
        (Toolbar, Stock_Undo, -"Undo Previous Action", Position => 4);
      Set_Sensitive (Undo_Redo.Undo_Button, False);
      Undo_Redo.Redo_Button := Insert_Stock
        (Toolbar, Stock_Redo, -"Redo Previous Action", Position => 5);
      Set_Sensitive (Undo_Redo.Redo_Button, False);

      Insert_Space (Toolbar, Position => 6);
      Button := Insert_Stock
        (Toolbar, Stock_Cut, -"Cut to Clipboard", Position => 7);
      Button := Insert_Stock
        (Toolbar, Stock_Copy, -"Copy to Clipboard", Position => 8);
      Button := Insert_Stock
        (Toolbar, Stock_Paste, -"Paste from Clipboard", Position => 9);

      Undo_Redo_Data.Set (Kernel, Undo_Redo, Undo_Redo_Id);
   end Register_Module;

end Src_Editor_Module;
