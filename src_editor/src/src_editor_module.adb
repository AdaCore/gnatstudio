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
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with OS_Utils;                  use OS_Utils;
with Src_Editor_Box;            use Src_Editor_Box;
with Traces;                    use Traces;

package body Src_Editor_Module is

   Src_Editor_Module_Id : Module_ID;
   Src_Editor_Module_Name : constant String := "Source_Editor";

   --  ??? Preferences
   Default_Editor_Width  : constant := 400;
   Default_Editor_Height : constant := 400;

   Me : Debug_Handle := Create ("Src_Editor_Module");

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   procedure Gtk_New
     (Box : out Source_Box; Editor : Source_Editor_Box);
   --  Create a new source box.

   procedure Initialize
     (Box : access Source_Box_Record'Class; Editor : Source_Editor_Box);
   --  Internal initialization function.

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize the module and all its menus

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel

   function Go_To
     (Kernel    : access Kernel_Handle_Record'Class;
      File      : String;
      Line      : Natural := 0;
      Column    : Natural := 0;
      Highlight : Boolean := True) return Source_Editor_Box;
   --  Go to the specified file at Line:Column
   --  Depending on the preferences, this may or may not open a new editor.
   --  If Highlight is True, Line will be highlighted.
   --  null is returned if no editor could be open

   procedure Save_To_File
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   procedure Cut_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Copy the currently-selected text to the clipboard and then delete it.

   procedure Copy_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Copy the currently-selected text to the clipboard.

   procedure Paste_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Paste the contents of the clipboard.

   procedure Select_All (Kernel : access Kernel_Handle_Record'Class);
   --  Set the selection bounds from the begining to the end of the buffer.

   function Get_Editor_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the filename of the last editor window that had the focus,
   --  "" if none.
   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Open a file and return the handle associated with it. If the given
   --  file does not exist, create an empty editor only if Create_New is True.
   --  ??? Need more comments.

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Create a new text editor that edits File.
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
   --  Tools->Generate Body menu

   ----------------------
   -- Creating editors --
   ----------------------

   procedure New_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new empty editor and add it in the MDI.

   procedure New_View
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   procedure Open_File
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : String;
      Success : out Boolean);
   --  Open a given file. Print an error message if we failed to open or read
   --  the file.
   --  Depending on the preferences, a new editor will be opened each time
   --  this function is called, or the same editor will be used, or a
   --  new editor will be opened only for different files.

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
      Iter : Child_Iterator := First_Child (Get_MDI (Kernel));
   begin
      while Get (Iter) /= null loop
         if Get_Widget (Get (Iter)).all in Source_Box_Record'Class then
            return Source_Box (Get_Widget (Get (Iter))).Editor;
         end if;

         Next (Iter);
      end loop;
      return null;
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

   ----------------
   -- New_Editor --
   ----------------

   procedure New_Editor
     (Kernel : access Kernel_Handle_Record'Class)
   is
      MDI    : constant MDI_Window := Get_MDI (Kernel);
      Editor : Source_Editor_Box;
      Box    : Source_Box;
      Child  : MDI_Child;

   begin
      Gtk_New (Editor, Kernel_Handle (Kernel));
      Gtk_New (Box, Editor);
      Set_Size_Request (Box, Default_Editor_Width, Default_Editor_Height);
      Attach (Editor, Box);
      Child := Put (MDI, Box);
      Show_All (Box);
      Set_Title (Child, "No Name");
   end New_Editor;

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
         Create_New_View (Editor, Current);
         Gtk_New (Box, Editor);
         Set_Size_Request (Box, Default_Editor_Width, Default_Editor_Height);
         Attach (Editor, Box);
         Child := Put (MDI, Box);
         --  ??? Should compute the right number.
         Set_Title (Child, Base_Name (Title) & " <2>");
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
      File_Exists : Boolean;
   begin
      if File = "" then
         return null;
      end if;

      --  At this point, we know that this file has not been opened yet,
      --  so we need to open it.

      File_Exists := Is_Regular_File (File);

      --  Create a new editor only if the file exists or we are asked to
      --  create a new empty one anyway.
      if File_Exists or else Create_New then
         Gtk_New (Editor, Kernel_Handle (Kernel));
      end if;

      if File_Exists then
         Load_File (Editor, File, Success => Success);
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
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box
   is
      MDI         : constant MDI_Window := Get_MDI (Kernel);
      Short_File  : String := Base_Name (File);
      Editor      : Source_Editor_Box;
      Box         : Source_Box;
      Child       : MDI_Child;
      Iter        : Child_Iterator := First_Child (MDI);

   begin
      if File = "" then
         return null;
      end if;

      --  ??? Should do a search on the full filename instead
      loop
         Child := Get (Iter);
         exit when Child = null
           or else Get_Title (Child) = Short_File;
         Next (Iter);
      end loop;

      if Child /= null then
         Raise_Child (Child);
         Set_Focus_Child (Child);
         return Source_Box (Get_Widget (Child)).Editor;
      end if;

      Editor := Create_File_Editor (Kernel, File, Create_New);

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Gtk_New (Box, Editor);
         Set_Size_Request (Box, Default_Editor_Width, Default_Editor_Height);
         Attach (Editor, Box);
         Child := Put (MDI, Box);
         Set_Title (Child, Short_File);
      else
         Console.Insert
           (Kernel, "Can not open file '" & File & "'",
            Highlight_Sloc => False);
      end if;

      return Editor;
   end Open_File;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : String;
      Success : out Boolean)
   is
      Editor  : Source_Editor_Box;
   begin
      Editor := Open_File (Kernel, File, Create_New => False);
      Success := Editor /= null;
   end Open_File;

   -----------------------
   -- Location_Callback --
   -----------------------

   function Location_Callback (D : Location_Idle_Data) return Boolean is
   begin
      Set_Cursor_Location (D.Edit, D.Line, D.Column);
      return False;
   end Location_Callback;

   -----------
   -- Go_To --
   -----------

   function Go_To
     (Kernel    : access Kernel_Handle_Record'Class;
      File      : String;
      Line      : Natural := 0;
      Column    : Natural := 0;
      Highlight : Boolean := True) return Source_Editor_Box
   is
      Edit : Source_Editor_Box;
      Id   : Idle_Handler_Id;
   begin
      Edit := Open_File (Kernel, File, Create_New => False);
      if Edit /= null then
         --  For some reason, we can not directly call Set_Cursor_Location,
         --  since the source editor won't be scrolled the first time the
         --  editor is displayed. Doing this in an idle callback ensures that
         --  all the proper events and initializations have taken place before
         --  we try to scroll the editor.
         Id := Location_Idle.Add
           (Location_Callback'Access, (Edit, Line, Column));

         if Highlight then
            Highlight_Line (Edit, Line);
         end if;
      end if;
      return Edit;
   end Go_To;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean)
   is
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source = null then
         return;
      end if;

      Save_To_File (Source, Name, Success);
   end Save_To_File;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source = null then
         return;
      end if;

      Cut_Clipboard (Source);
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source = null then
         return;
      end if;

      Copy_Clipboard (Source);
   end Copy_Clipboard;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source = null then
         return;
      end if;

      Paste_Clipboard (Source);
   end Paste_Clipboard;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Kernel : access Kernel_Handle_Record'Class) is
      Source : constant Source_Editor_Box := Find_Current_Editor (Kernel);
   begin
      if Source = null then
         return;
      end if;

      Select_All (Source);
   end Select_All;

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
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
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
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      New_Editor (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_File;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
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
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
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
      Success : Boolean;
   begin
      declare
         Name : constant String := Select_File (-"Save File As");
      begin
         Save_To_File (Kernel, Name, Success);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_As;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      Cut_Clipboard (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Cut;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      Copy_Clipboard (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Copy;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      Paste_Clipboard (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Paste;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      Select_All (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Select_All;

   -----------------
   -- On_New_View --
   -----------------

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
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
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      if Get_Editor_Filename (Kernel) = "" then
         --  Nothing to do, since no saved editor has the focus
         return;
      end if;

      Goto_Declaration_Or_Body (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Declaration_Or_Body;

   ----------------------
   -- On_Generate_Body --
   ----------------------

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Success : Boolean;
      Title   : constant String := Get_Editor_Filename (Kernel);

      function Body_Name (Name : String) return String;
      --  Return the name of the body corresponding to a spec file.

      procedure Gnatstub (Name : String; Success : out Boolean);
      --  Launch gnatstub process and wait for it.

      function Body_Name (Name : String) return String is
      begin
         return Name (Name'First .. Name'Last - 1) & 'b';
      end Body_Name;

      procedure Refresh;
      --  Handle pending graphical events.

      -------------
      -- Refresh --
      -------------

      procedure Refresh is
         Dead : Boolean;
      begin
         while Gtk.Main.Events_Pending loop
            Dead := Main_Iteration;
         end loop;
      end Refresh;

      --------------
      -- Gnatstub --
      --------------

      procedure Gnatstub (Name : String; Success : out Boolean) is
         Args : Argument_List (1 .. 1);
         Exec : String_Access := Locate_Exec_On_Path ("gnatstub");

      begin
         if Exec = null then
            Success := False;
            return;
         end if;

         Args (1) := new String' (Name);
         Spawn (Exec.all, Args, Refresh'Unrestricted_Access, Success);
         Free (Exec);
         Free (Args (1));
      end Gnatstub;

   begin
      if Title /= "" then
         Gnatstub (Title, Success);

         if Success then
            Open_File (Kernel, Body_Name (Title), Success);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Generate_Body;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      Success : Boolean := True;
      Edit : Source_Editor_Box;
   begin
      if Mime_Type = Mime_Source_File then
         declare
            File   : constant String := Get_String (Data (Data'First));
            Line   : constant Gint := Get_Int (Data (Data'First + 1));
            Column : constant Gint := Get_Int (Data (Data'First + 2));
            Highlight : constant Boolean :=
              Get_Boolean (Data (Data'First + 3));
         begin
            if File = "" then
               New_Editor (Kernel);
            elsif Line /= 0 or else Column /= 0 then
               Edit := Go_To
                 (Kernel, File, Natural (Line), Natural (Column), Highlight);
               Success := Edit /= null;
            else
               Open_File (Kernel, File, Success);
            end if;
            return Success;
         end;
      end if;
      return False;
   end Mime_Action;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File    : constant String := '/' & (-"File") & '/';
      Edit    : constant String := '/' & (-"Edit") & '/';
      Gotom   : constant String := '/' & (-"Navigate") & '/';
      Tools   : constant String := '/' & (-"Tools") & '/';
      Mitem   : Gtk_Menu_Item;
      Button  : Gtk_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);

   begin
      --  Menus

      Register_Menu (Kernel, File, -"Open...",  Stock_Open,
                     On_Open_File'Access, GDK_F3, Ref_Item => -"Close");
      Register_Menu (Kernel, File & (-"Reopen"), Ref_Item => -"Open...",
                     Add_Before => False);
      Gtk_New (Mitem, "");
      Register_Menu
        (Kernel, File, Mitem, Ref_Item => -"Reopen", Add_Before => False);

      Register_Menu (Kernel, File, -"New",  Stock_New, On_New_File'Access,
                     Ref_Item => -"Open...");
      Register_Menu (Kernel, File, -"New View",  "", On_New_View'Access,
                     Ref_Item => -"Open...");

      Register_Menu (Kernel, File, -"Save",  Stock_Save, On_Save'Access,
                     Ref_Item => -"Close");
      Register_Menu (Kernel, File, -"Save As...",  Stock_Save_As,
                     On_Save_As'Access, Ref_Item => -"Close");

      Register_Menu (Kernel, Edit, -"Undo",  Stock_Undo,
                     On_Undo'Access, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Redo",  Stock_Undo,
                     On_Redo'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem, "");
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

      Gtk_New (Mitem, "");
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Gotom, -"Goto Declaration<->Body", Stock_Home,
                     On_Goto_Declaration_Or_Body'Access,
                     Ref_Item => -"Goto Line...");
      Register_Menu (Kernel, Tools, -"Generate Body", "",
                     On_Generate_Body'Access);

      --  Toolbars

      Button := Insert_Stock (Toolbar, Stock_New, -"Create a New File");
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_New_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock (Toolbar, Stock_Open, -"Open a File");
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Open_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock (Toolbar, Stock_Save, -"Save Current File");
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Save'Access),
         Kernel_Handle (Kernel));

      Append_Space (Toolbar);
      Button := Insert_Stock (Toolbar, Stock_Undo, -"Undo Previous Action");
      Button := Insert_Stock (Toolbar, Stock_Redo, -"Redo Previous Action");
      Append_Space (Toolbar);
      Button := Insert_Stock (Toolbar, Stock_Cut, -"Cut to Clipboard");
      Button := Insert_Stock (Toolbar, Stock_Copy, -"Copy to Clipboard");
      Button := Insert_Stock (Toolbar, Stock_Paste, -"Paste from Clipboard");
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Src_Editor_Module_Id := Register_Module
        (Module_Name             => Src_Editor_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => null,
         Mime_Handler            => Mime_Action'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

end Src_Editor_Module;
