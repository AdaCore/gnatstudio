-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Case_Util;            use GNAT.Case_Util;
with Glib.Xml_Int;              use Glib.Xml_Int;
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
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Glide_Main_Window;         use Glide_Main_Window;
with Interactive_Consoles;      use Interactive_Consoles;
with Basic_Types;               use Basic_Types;
with GVD.Status_Bar;            use GVD.Status_Bar;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Entry_Completion;   use Gtkada.Entry_Completion;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Src_Editor_Box;            use Src_Editor_Box;
with String_List_Utils;         use String_List_Utils;
with String_Utils;              use String_Utils;
with File_Utils;                use File_Utils;
with Traces;                    use Traces;
with Prj_API;                   use Prj_API;
with Src_Contexts;              use Src_Contexts;
with Find_Utils;                use Find_Utils;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with OS_Utils;                  use OS_Utils;

with Generic_List;
with GVD.Preferences; use GVD.Preferences;

package body Src_Editor_Module is

   Me : constant Debug_Handle := Create ("Src_Editor_Module");

   No_Handler : constant Handler_Id := (Null_Signal_Id, null);

   Hist_Key : constant History_Key := "reopen_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.

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

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Recent_Menu_Item         : Gtk_Menu_Item;
      Source_Lines_Revealed_Id : Handler_Id := No_Handler;
      File_Edited_Id           : Handler_Id := No_Handler;
      File_Closed_Id           : Handler_Id := No_Handler;
      Location_Open_Id         : Idle_Handler_Id := 0;
      Display_Line_Numbers     : Boolean    := False;

      Stored_Marks             : Mark_Identifier_List.List;
      Next_Mark_Id             : Natural := 0;

      Open_File_Entry          : Gtkada_Entry;
      Open_File_Dialog         : Gtk_Dialog;

      Unopened_Files           : String_List_Utils.String_List.List;
      --  Contains a list of files for which marks have been created but
      --  that are not open.
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   procedure Destroy (Id : in out Source_Editor_Module_Record);
   --  Free the memory used by the module.

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   procedure Generate_Body_Cb (Data : Process_Data; Status : Integer);
   --  Callback called when gnatstub has completed.

   procedure Pretty_Print_Cb (Data : Process_Data; Status : Integer);
   --  Callback called when gnatpp has completed.

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

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String := "";
      Create_New : Boolean := True;
      Add_To_MDI : Boolean := True;
      Focus      : Boolean := True) return Source_Box;
   --  Open a file and return the handle associated with it.
   --  If Add_To_MDI is set to True, the box will be added to the MDI window.
   --  If Focus is True, the box will be raised if it is in the MDI.
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

   procedure Refresh_Recent_Menu (Kernel : access Kernel_Handle_Record'Class);
   --  Fill the Recent menu.

   function Save_Function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Force  : Boolean := False) return Save_Return_Value;
   --  Save the text editor.
   --  If Force is False, then offer a choice to the user before doing so.

   type Location_Idle_Data is record
      Edit  : Source_Editor_Box;
      Line, Column, Column_End : Natural;
      Focus : Boolean;
      Child : MDI_Child;
   end record;

   package Location_Idle is new Gtk.Main.Idle (Location_Idle_Data);

   function Location_Callback (D : Location_Idle_Data) return Boolean;
   --  Idle callback used to scroll the source editors.

   procedure Location_Destroy (D : in out Location_Idle_Data);
   --  Called when the Idle callback to scroll the source editors is
   --  destroyed.

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean;
   --  Emit the File_Edited signal.

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open menu

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open From Path menu

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New View menu

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New menu

   procedure On_Recent
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  File->Recent menu

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save menu

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save As... menu

   procedure On_Save_All_Editors
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save All Editors menu

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save All menu

   procedure On_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Print menu

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

   procedure On_Goto_Line
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Line... menu

   procedure On_Goto_Declaration
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Declaration menu
   --  Goto the declaration of the entity under the cursor in the current
   --  editor.

   procedure On_Goto_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Body menu
   --  Goto the next body of the entity under the cursor in the current
   --  editor.

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Generate Body menu

   procedure On_Pretty_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Pretty Print menu

   procedure On_Comment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Comment Lines menu

   procedure On_Uncomment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Uncomment Lines menu

   procedure Comment_Uncomment
     (Kernel : Kernel_Handle; Comment : Boolean);
   --  Comment or uncomment the current selection, if any.
   --  Auxiliary procedure for On_Comment_Lines and On_Uncomment_Lines.

   procedure On_Edit_File
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Edit a file (from a contextual menu)

   procedure On_Lines_Revealed
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Display the line numbers.

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

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access;
   --  Same as above.

   procedure New_View
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" signal.

   procedure File_Edited_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_edited" signal.

   procedure File_Closed_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_closed" signal.

   procedure File_Saved_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_saved" signal.

   procedure Preferences_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

   function Edit_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String;
   --  Interactive command handler for the source editor module.

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String);
   --  Add an entry for File to the Recent menu, if needed.

   function Console_Has_Focus
     (Kernel : access Kernel_Handle_Record'Class) return Boolean;
   --  Return True if the focus MDI child is an interactive console.

   function Find_Mark (Identifier : String) return Mark_Identifier_Record;
   --  Find the mark corresponding to Identifier, or return an empty
   --  record.

   procedure Fill_Marks (Kernel : Kernel_Handle; File : String);
   --  Create the marks on the buffer corresponding to File, if File has just
   --  been open.

   -----------------------
   -- Console_Has_Focus --
   -----------------------

   function Console_Has_Focus
     (Kernel : access Kernel_Handle_Record'Class) return Boolean
   is
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      if Child = null then
         return False;
      else
         return Get_Widget (Child).all in Interactive_Console_Record'Class;
      end if;
   end Console_Has_Focus;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Mark_Identifier_Record) is
   begin
      Free (X.File);
   end Free;

   ---------------
   -- Find_Mark --
   ---------------

   function Find_Mark (Identifier : String) return Mark_Identifier_Record is
      use type Mark_Identifier_List.List_Node;

      Id          : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Mark_Node   : Mark_Identifier_List.List_Node;
      Mark_Record : Mark_Identifier_Record;
   begin
      Mark_Node := Mark_Identifier_List.First (Id.Stored_Marks);

      while Mark_Node /= Mark_Identifier_List.Null_Node loop
         Mark_Record := Mark_Identifier_List.Data (Mark_Node);

         if Image (Mark_Record.Id) = Identifier then
            return Mark_Record;
         end if;

         Mark_Node := Mark_Identifier_List.Next (Mark_Node);
      end loop;

      return Mark_Identifier_Record'
        (Id     => 0,
         Child  => null,
         File   => null,
         Mark   => null,
         Line   => 0,
         Column => 0,
         Length => 0);
   end Find_Mark;

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   function Edit_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;
      Id       : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Node          : List_Node;
      Filename      : Basic_Types.String_Access;
      Error_Message : Basic_Types.String_Access;
      Line     : Natural := 1;
      Length   : Natural := 0;
      Column   : Natural := 1;
      Force    : Boolean;
      All_Save : Boolean;

      function Parse_Argument (Arg : String) return Natural;
      --  Parse a numerical argument, produce an error message corresponding
      --  to Arg if parsing fails.

      function Parse_Argument (Arg : String) return Natural is
      begin
         Node := Next (Node);

         if Node = Null_Node then
            Free (Filename);
            Error_Message := new String'
              (Command & ": " & (-"option " & Arg & " requires a value"));
            return 0;
         end if;

         declare
         begin
            return Natural'Value (Data (Node));
         exception
            when others =>
               Free (Filename);

               Error_Message := new String'
                 (Command
                  & ": " & (-"option " & Arg & " requires a numerical value"));
               return 0;
         end;
      end Parse_Argument;

   begin
      if Command = "edit" or else Command = "create_mark" then
         Node := First (Args);

         while Node /= Null_Node loop
            if Data (Node) = "-c" then
               Column := Parse_Argument ("-c");
            elsif Data (Node) = "-l" then
               Line := Parse_Argument ("-l");
            elsif Data (Node) = "-L" then
               Length := Parse_Argument ("-L");
            elsif Filename = null then
               Filename := new String'(Data (Node));
            else
               Free (Filename);
               return Command & ": " & (-"too many parameters");
            end if;

            if Error_Message /= null then
               declare
                  Message : constant String := Error_Message.all;
               begin
                  Free (Filename);
                  Free (Error_Message);
                  return Message;
               end;
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            if Command = "edit" then
               Open_File_Editor
                 (Kernel,
                  Filename.all,
                  Line,
                  Column);

            elsif Command = "create_mark" then
               declare
                  Box         : Source_Box;
                  Child       : MDI_Child;
                  Mark_Record : Mark_Identifier_Record;
                  File        : constant String :=
                    Find_Source_File (Kernel, Filename.all, True);

               begin
                  if File /= "" then
                     Free (Filename);
                     Filename := new String'(File);
                  end if;

                  Child := Find_Editor (Kernel, Filename.all);

                  --  Create a new mark record and insert it in the list.

                  Mark_Record.File := new String'(Filename.all);
                  Mark_Record.Id := Id.Next_Mark_Id;

                  Id.Next_Mark_Id := Id.Next_Mark_Id + 1;

                  Mark_Record.Length := Length;

                  if Child /= null then
                     Mark_Record.Child := Child;
                     Box := Source_Box (Get_Widget (Child));
                     Mark_Record.Mark :=
                       Create_Mark (Box.Editor, Line, Column);
                  else
                     Mark_Record.Line := Line;
                     Mark_Record.Column := Column;
                     Add_Unique_Sorted (Id.Unopened_Files, Filename.all);
                  end if;

                  Mark_Identifier_List.Append (Id.Stored_Marks, Mark_Record);

                  Free (Filename);
                  return Image (Mark_Record.Id);
               end;
            end if;

            Free (Filename);

            return "";
         else
            return Command & ": " & (-"missing parameter file_name");
         end if;

      elsif Command = "close"
        or else Command = "edit_undo"
        or else Command = "edit_redo"
      then
         Node := First (Args);

         while Node /= Null_Node loop
            if Filename = null then
               Filename := new String'(Data (Node));
            else
               Free (Filename);
               return Command & ": " & (-"too many parameters");
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            if Command = "close" then
               Close_File_Editors (Kernel, Filename.all);
            else
               declare
                  Child : MDI_Child;
                  Box   : Source_Box;
               begin
                  Child := Find_Editor (Kernel, Filename.all);

                  if Child = null then
                     Free (Filename);
                     return "file not open";
                  end if;

                  Box := Source_Box (Get_Widget (Child));

                  if Command = "edit_redo" then
                     Redo (Box.Editor);
                  elsif Command = "edit_undo" then
                     Undo (Box.Editor);
                  end if;
               end;
            end if;

            Free (Filename);

            return "";
         else
            return Command & ": " & (-"missing parameter file_name");
         end if;

      elsif Command = "goto_mark" then
         Node := First (Args);

         while Node /= Null_Node loop
            if Filename = null then
               Filename := new String'(Data (Node));
            else
               Free (Filename);
               return Command & ": " & (-"too many parameters");
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            declare
               Mark_Record : constant Mark_Identifier_Record
                 := Find_Mark (Filename.all);
            begin
               if Mark_Record.Child /= null then
                  Raise_Child (Mark_Record.Child);
                  Set_Focus_Child (Mark_Record.Child);
                  Grab_Focus
                    (Source_Box
                       (Get_Widget (Mark_Record.Child)).Editor);

                  --  If the Length is null, we set the length to 1, otherwise
                  --  the cursor is not visible.

                  if Mark_Record.Length = 0 then
                     Scroll_To_Mark
                       (Source_Box (Get_Widget (Mark_Record.Child)).Editor,
                        Mark_Record.Mark, 1);
                  else
                     Scroll_To_Mark
                       (Source_Box (Get_Widget (Mark_Record.Child)).Editor,
                        Mark_Record.Mark,
                        Mark_Record.Length);
                  end if;

               else
                  if Mark_Record.File /= null
                    and then Is_In_List
                      (Id.Unopened_Files, Mark_Record.File.all)
                  then
                     Open_File_Editor (Kernel,
                                       Mark_Record.File.all,
                                       Mark_Record.Line,
                                       Mark_Record.Column,
                                       Mark_Record.Column
                                         + Mark_Record.Length);
                     Fill_Marks (Kernel_Handle (Kernel), Mark_Record.File.all);
                  end if;
               end if;

               Free (Filename);

               return "";
            end;
         else
            return -"goto_mark: missing parameter file_name";
         end if;

      elsif Command = "get_chars" or else Command = "replace_text" then
         declare
            Before : Integer := -1;
            After  : Integer := -1;
            Text   : Basic_Types.String_Access;
         begin
            Node := First (Args);

            while Node /= Null_Node loop
               if Data (Node) = "-c" then
                  Column := Parse_Argument ("-c");

               elsif Data (Node) = "-l" then
                  Line := Parse_Argument ("-l");

               elsif Data (Node) = "-b" then
                  Before := Parse_Argument ("-b");

               elsif Data (Node) = "-a" then
                  After := Parse_Argument ("-a");

               elsif Filename = null then
                  Filename := new String'(Data (Node));

               elsif Text = null then
                  declare
                     T : constant String := Data (Node);
                  begin
                     Text := new String'(T (T'First + 1 .. T'Last - 1));
                  end;

               else
                  Free (Text);
                  Free (Filename);
                  return Command & ": " & (-"too many parameters");
               end if;

               if Error_Message /= null then
                  declare
                     Message : constant String := Error_Message.all;
                  begin
                     Free (Filename);
                     Free (Error_Message);
                     Free (Text);
                     return Message;
                  end;
               end if;

               Node := Next (Node);
            end loop;

            if Filename /= null then
               declare
                  Mark_Record : constant Mark_Identifier_Record
                    := Find_Mark (Filename.all);
                  Child       : MDI_Child;
               begin
                  if Mark_Record.Mark /= null
                    and then Mark_Record.Child /= null
                  then
                     Free (Filename);

                     if Command = "get_chars" then
                        Free (Text);
                        return
                          Get_Chars
                            (Source_Box
                                 (Get_Widget (Mark_Record.Child)).Editor,
                             Mark_Record.Mark,
                             Before, After);
                     else
                        if Text = null then
                           Text := new String'("");
                        end if;

                        Replace_Slice
                          (Source_Box (Get_Widget (Mark_Record.Child)).Editor,
                           Mark_Record.Mark,
                           Before, After,
                           Text.all);

                        Free (Text);
                        return "";
                     end if;
                  else
                     Child := Find_Editor (Kernel, Filename.all);
                     Free (Filename);

                     if Child /= null then
                        if Command = "get_chars" then
                           Free (Text);
                           return Get_Chars
                             (Source_Box (Get_Widget (Child)).Editor,
                              Line, Column,
                              Before, After);
                        else
                           if Text = null then
                              Text := new String'("");
                           end if;

                           Replace_Slice_At_Position
                             (Source_Box (Get_Widget (Child)).Editor,
                              Line, Column,
                              Before, After,
                              Text.all);

                           Free (Text);
                           return "";
                        end if;
                     end if;

                     Free (Text);
                     return "mark not found";
                  end if;
               end;

            else
               return "invalid position";
            end if;
         end;

      elsif Command = "get_line"
        or else Command = "get_column"
        or else Command = "get_file"
      then
         Node := First (Args);

         while Node /= Null_Node loop
            if Filename = null then
               Filename := new String'(Data (Node));
            else
               Free (Filename);
               return -"close: too many parameters";
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            declare
               Mark_Record : constant Mark_Identifier_Record
                 := Find_Mark (Filename.all);
            begin
               Free (Filename);

               if Mark_Record.File = null then
                  return -"mark not found";
               else
                  if Command = "get_line" then
                     if Mark_Record.Child /= null then
                        return Image
                          (Get_Line
                             (Source_Box
                                (Get_Widget (Mark_Record.Child)).Editor,
                              Mark_Record.Mark));
                     else
                        return Image (Mark_Record.Line);
                     end if;
                  elsif Command = "get_column" then
                     if Mark_Record.Child /= null then
                        return Image
                          (Get_Column
                             (Source_Box
                                (Get_Widget (Mark_Record.Child)).Editor,
                              Mark_Record.Mark));
                     else
                        return Image (Mark_Record.Column);
                     end if;
                  else
                     if Mark_Record.File = null then
                        return "";
                     else
                        return Mark_Record.File.all;
                     end if;
                  end if;
               end if;
            end;
         else
            return -"missing parameter mark";
         end if;

      elsif Command = "get_last_line" then
         Node := First (Args);

         while Node /= Null_Node loop
            if Filename = null then
               Filename := new String'(Data (Node));
            else
               Free (Filename);
               return -"close: too many parameters";
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            declare
               Child : constant MDI_Child := Find_Editor
                 (Kernel, Filename.all);
            begin
               if Child = null then
                  declare
                     A : GNAT.OS_Lib.String_Access
                       := Read_File (Filename.all);
                     N : Natural := 0;
                  begin
                     Free (Filename);

                     if A /= null then
                        for J in A'Range loop
                           if A (J) = ASCII.LF then
                              N := N + 1;
                           end if;
                        end loop;

                        Free (A);

                        if N = 0 then
                           N := 1;
                        end if;

                        return Image (N);
                     else
                        return -"file not found or not opened";
                     end if;
                  end;
               else
                  Free (Filename);

                  return Image
                    (Get_Last_Line
                       (Source_Box (Get_Widget (Child)).Editor));
               end if;
            end;
         else
            return -"missing parameter file";
         end if;

      elsif Command = "get_buffer" then
         Node := First (Args);

         while Node /= Null_Node loop
            if Filename = null then
               Filename := new String'(Data (Node));
            else
               Free (Filename);
               return -"close: too many parameters";
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            declare
               Child : constant MDI_Child
                := Find_Editor (Kernel, Filename.all);
            begin
               if Child /= null then
                  Free (Filename);
                  return Get_Buffer (Source_Box (Get_Widget (Child)).Editor);

               else
                  --  The buffer is not currently open,
                  --  read directly from disk.

                  declare
                     A : GNAT.OS_Lib.String_Access
                       := Read_File (Filename.all);
                  begin
                     Free (Filename);

                     if A /= null then
                        declare
                           S : constant String := A.all;
                        begin
                           Free (A);
                           return S;
                        end;

                     else
                        return -"file not found";
                     end if;
                  end;
               end if;
            end;
         else
            return -"missing parameter file";
         end if;

      elsif Command = "save" then
         Force := True;
         All_Save := False;

         Node := First (Args);

         while Node /= Null_Node loop
            if Data (Node) = "-i" then
               Force := False;
            elsif Data (Node) = "all" then
               All_Save := True;
            else
               return -"save: invalid parameter";
            end if;

            Node := Next (Node);
         end loop;

         if All_Save then
            if Save_All_MDI_Children (Kernel, Force) then
               return "";
            else
               return -"cancelled";
            end if;
         else
            declare
               Child : constant MDI_Child := Find_Current_Editor (Kernel);
            begin
               if Child = null then
                  return -"no file selected";
               else
                  return To_Lower (Save_Return_Value'Image
                    (Save_Child (Kernel, Child, False)));
               end if;
            end;
         end if;

      else
         return -"command not recognized: " & Command;
      end if;
   end Edit_Command_Handler;

   ----------------
   -- Fill_Marks --
   ----------------

   procedure Fill_Marks
     (Kernel : Kernel_Handle;
      File   : String)
   is
      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

      use Mark_Identifier_List;

      Box         : Source_Box;
      Child       : MDI_Child;
      Node        : List_Node;
      Mark_Record : Mark_Identifier_Record;
   begin
      if Is_In_List (Id.Unopened_Files, File) then
         Child := Find_Editor (Kernel, File);

         if Child = null then
            return;
         end if;

         Box := Source_Box (Get_Widget (Child));
         Remove_From_List (Id.Unopened_Files, File);

         Node := First (Id.Stored_Marks);

         while Node /= Null_Node loop
            Mark_Record := Data (Node);

            if Mark_Record.File /= null
              and then Mark_Record.File.all = File
            then
               Set_Data (Node,
                         Mark_Identifier_Record'
                           (Id => Mark_Record.Id,
                            Child => Child,
                            File => new String'(File),
                            Line => Mark_Record.Line,
                            Mark =>
                              Create_Mark
                                (Box.Editor,
                                 Mark_Record.Line, Mark_Record.Column),
                            Column => Mark_Record.Column,
                            Length => Mark_Record.Length));
            end if;

            Node := Next (Node);
         end loop;
      end if;
   end Fill_Marks;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Infos : Line_Information_Data;
      File  : constant String := Get_String (Nth (Args, 1));
   begin
      if Id.Display_Line_Numbers then
         Create_Line_Information_Column
           (Kernel,
            File,
            Src_Editor_Module_Name,
            Stick_To_Data => False,
            Every_Line    => True);

         Infos := new Line_Information_Array (1 .. 1);
         Infos (1).Text := new String'("   1");

         Add_Line_Information
           (Kernel,
            File,
            Src_Editor_Module_Name,
            Infos);

         Unchecked_Free (Infos);
      end if;

      Fill_Marks (Kernel, File);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);

      use Mark_Identifier_List;

      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      File  : constant String := Get_String (Nth (Args, 1));

      Node        : List_Node;
      Mark_Record : Mark_Identifier_Record;
      Added       : Boolean := False;

      Box         : Source_Box;

   begin
      --  If the file has marks, store their location.

      Node := First (Id.Stored_Marks);

      while Node /= Null_Node loop
         if Data (Node).File /= null
           and then Data (Node).File.all = File
         then
            Mark_Record := Data (Node);

            if Mark_Record.Child /= null
              and then Mark_Record.Mark /= null
            then
               Box := Source_Box (Get_Widget (Mark_Record.Child));

               Mark_Record.Line := Get_Line (Box.Editor, Mark_Record.Mark);
               Mark_Record.Column :=
                 Get_Column (Box.Editor, Mark_Record.Mark);
            end if;

            Set_Data (Node,
                      Mark_Identifier_Record'
                        (Id => Mark_Record.Id,
                         Child => null,
                         File => new String'(File),
                         Line => Mark_Record.Line,
                         Mark => null,
                         Column => Mark_Record.Column,
                         Length => Mark_Record.Length));

            if not Added then
               Add_Unique_Sorted (Id.Unopened_Files, File);
               Added := True;
            end if;
         end if;

         Node := Next (Node);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Closed_Cb;

   -------------------
   -- File_Saved_Cb --
   -------------------

   procedure File_Saved_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      File  : constant String := Get_String (Nth (Args, 1));
      Base  : constant String := Base_Name (File);
   begin
      --  Insert the saved file in the Recent menu.

      if File /= ""
        and then not (Base'Length > 2
                      and then Base (Base'First .. Base'First + 1) = ".#")
      then
         Add_To_Recent_Menu (Kernel, File);
      end if;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Saved_Cb;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);
   begin
      return Save_Function
        (Get_Kernel (Source_Box (Widget).Editor), Gtk_Widget (Widget), False)
        = Cancel;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Delete_Callback;

   ------------------------
   -- File_Edit_Callback --
   ------------------------

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean is
   begin
      File_Edited (Get_Kernel (D.Edit), Get_Filename (D.Edit));
      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end File_Edit_Callback;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget
   is
      Src  : Source_Box := null;
      File : Glib.String_Ptr;
      Data : Location_Idle_Data;
      Id   : Idle_Handler_Id;
      pragma Unreferenced (Id);

   begin
      if Node.Tag.all = "Source_Editor" then
         File := Get_Field (Node, "File");

         if File /= null then
            Src := Open_File (User, File.all, False, False);

            if Src /= null then
               Data.Edit := Src.Editor;
               Id := Location_Idle.Add
                 (File_Edit_Callback'Access,
                  (Src.Editor, 1, 1, 0, True, null));

               return Gtk_Widget (Src);
            end if;
         end if;
      end if;

      return null;
   end Load_Desktop;

   -----------------------
   -- On_Lines_Revealed --
   -----------------------

   procedure On_Lines_Revealed
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context      : constant Selection_Context_Access :=
        To_Selection_Context_Access (Get_Address (Nth (Args, 1)));
      Area_Context : File_Area_Context_Access;
      Infos        : Line_Information_Data;
      Line1, Line2 : Integer;

   begin
      if Context.all in File_Area_Context'Class then
         Area_Context := File_Area_Context_Access (Context);

         Get_Area (Area_Context, Line1, Line2);

         Infos := new Line_Information_Array (Line1 .. Line2);

         for J in Infos'Range loop
            Infos (J).Text := new String'(Image (J));
         end loop;

         if Has_File_Information (Area_Context) then
            Add_Line_Information
              (Kernel,
               Directory_Information (Area_Context) &
               File_Information (Area_Context),
               Src_Editor_Module_Name,
               Infos);
         else
            Add_Line_Information
              (Kernel,
               "",
               Src_Editor_Module_Name,
               Infos);
         end if;

         Unchecked_Free (Infos);
      end if;
   end On_Lines_Revealed;

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
         N.Tag := new String'("Source_Editor");

         Child := new Node;
         Child.Tag := new String'("File");
         Child.Value := new String'
           (Get_Filename (Source_Box (Widget).Editor));
         Add_Child (N, Child);

         return N;
      end if;

      return null;
   end Save_Desktop;

   -----------------------------
   -- Get_Source_Box_From_MDI --
   -----------------------------

   function Get_Source_Box_From_MDI
     (Child : Gtkada.MDI.MDI_Child) return Source_Editor_Box is
   begin
      if Child = null then
         return null;
      else
         return Source_Box (Get_Widget (Child)).Editor;
      end if;
   end Get_Source_Box_From_MDI;

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
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
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

         Gtkada.Handlers.Return_Callback.Object_Connect
           (Box,
            "delete_event",
            Delete_Callback'Access,
            Gtk_Widget (Box),
            After => False);

         --  ??? Should compute the right number.
         Set_Title (Child, Title & " <2>", Base_Name (Title) & " <2>");
      end if;
   end New_View;

   -------------------
   -- Save_Function --
   -------------------

   function Save_Function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Force  : Boolean := False) return Save_Return_Value
   is
      Success        : Boolean;
      Containing_Box : constant Source_Box := Source_Box (Child);
      Box            : constant Source_Editor_Box := Containing_Box.Editor;
      Button         : Message_Dialog_Buttons;
   begin
      if Force then
         if Modified (Box) then
            Save_To_File (Box, Success => Success);
         end if;

      elsif Modified (Box) then
         Button := Message_Dialog
           (Msg            =>
              (-"Do you want to save file ") & Get_Filename (Box) & " ?",
            Dialog_Type    => Confirmation,
            Buttons        =>
              Button_Yes or Button_All or Button_No or Button_Cancel,
            Default_Button => Button_Cancel,
            Parent         => Get_Main_Window (Kernel));

         case Button is
            when Button_Yes =>
               Save_To_File (Box, Success => Success);
               return Saved;

            when Button_No =>
               return Not_Saved;

            when Button_All =>
               Save_To_File (Box, Success => Success);
               return Save_All;

            when others =>
               return Cancel;

         end case;
      end if;
      return Saved;
   end Save_Function;

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
      File_Exists : Boolean := False;

   begin
      if File /= "" then
         File_Exists := Is_Regular_File (File);
      end if;

      --  Create a new editor only if the file exists or we are asked to
      --  create a new empty one anyway.

      if File_Exists or else Create_New then
         Gtk_New (Editor, Kernel_Handle (Kernel));
      else
         return null;
      end if;

      if File_Exists then
         Load_File (Editor, File, Success => Success);

         if not Success then
            Destroy (Editor);
            Editor := null;
         end if;

      else
         Load_Empty_File (Editor, File, Get_Language_Handler (Kernel));
      end if;

      return Editor;
   end Create_File_Editor;

   ------------------------
   -- Add_To_Recent_Menu --
   ------------------------

   procedure Add_To_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class; File : String) is
   begin
      Add_To_History (Kernel, Hist_Key, File);
      Refresh_Recent_Menu (Kernel);
   end Add_To_Recent_Menu;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String := "";
      Create_New : Boolean := True;
      Add_To_MDI : Boolean := True;
      Focus      : Boolean := True) return Source_Box
   is
      MDI        : constant MDI_Window := Get_MDI (Kernel);
      Editor     : Source_Editor_Box;
      Box        : Source_Box;
      Child      : MDI_Child;

   begin
      if File /= "" then
         Child := Find_Editor (Kernel, File);

         if Child /= null then
            if Focus then
               Raise_Child (Child);
               Set_Focus_Child (Child);
            end if;

            Trace (Me, "Editor for " & File & " already exists");
            return Source_Box (Get_Widget (Child));
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

         if Add_To_MDI then
            Child := Put (MDI, Box);

            if File /= "" then
               Set_Title (Child, File, Base_Name (File));
            else
               --  Determine the number of "Untitled" files open.

               declare
                  Iterator    : Child_Iterator := First_Child (MDI);
                  The_Child   : MDI_Child;
                  Nb_Untitled : Natural := 0;
                  No_Name     : constant String := -"Untitled";
               begin
                  The_Child := Get (Iterator);

                  while The_Child /= null loop
                     if Get_Widget (Child).all in Source_Box_Record'Class then
                        declare
                           Title : constant String := Get_Title (The_Child);
                        begin
                           if Title'Length >= No_Name'Length
                             and then Title
                               (Title'First
                                .. Title'First + No_Name'Length - 1) = No_Name
                           then
                              Nb_Untitled := Nb_Untitled + 1;
                           end if;
                        end;
                     end if;

                     Next (Iterator);
                     The_Child := Get (Iterator);
                  end loop;

                  if Nb_Untitled = 0 then
                     Set_Title (Child, -"Untitled");
                  else
                     Set_Title
                       (Child,
                          -"Untitled" & " (" & Image (Nb_Untitled + 1) & ")");
                  end if;

                  Set_Filename (Editor, Get_Title (Child));
               end;
            end if;

            --  We have created a new file editor: emit the
            --  corresponding signal.
            --  ??? what do we do when opening an editor with no name ?

            File_Edited (Kernel, Get_Title (Child));
         end if;

         Gtkada.Handlers.Return_Callback.Object_Connect
           (Box,
            "delete_event",
            Delete_Callback'Access,
            Gtk_Widget (Box),
            After => False);

         if File /= "" then
            Add_To_Recent_Menu (Kernel, File);
         end if;

      else
         Console.Insert
           (Kernel, (-"Cannot open file ") & "'" & File & "'",
            Add_LF => True,
            Mode   => Error);
      end if;

      return Box;
   end Open_File;

   -----------------------
   -- Location_Callback --
   -----------------------

   function Location_Callback (D : Location_Idle_Data) return Boolean is
   begin
      if D.Focus then
         Grab_Focus (D.Edit);
      elsif D.Child /= null then
         Set_Focus_Child (D.Child);
      end if;

      if Is_Valid_Location (D.Edit, D.Line) then
         Set_Screen_Location (D.Edit, D.Line, D.Column, D.Focus);

         if D.Column_End /= 0
           and then Is_Valid_Location (D.Edit, D.Line, D.Column_End)
         then
            Select_Region (D.Edit, D.Line, D.Column, D.Line, D.Column_End);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Location_Callback;

   ----------------------
   -- Location_Destroy --
   ----------------------

   procedure Location_Destroy (D : in out Location_Idle_Data) is
      pragma Unreferenced (D);
   begin
      Source_Editor_Module (Src_Editor_Module_Id).Location_Open_Id := 0;
   end Location_Destroy;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Source : Source_Editor_Box;

   begin
      if Child = null then
         return;
      end if;

      Source := Source_Box (Get_Widget (Child)).Editor;

      declare
         Old_Name : constant String := Get_Filename (Source);
      begin
         Save_To_File (Source, Name, Success);

         declare
            New_Name : constant String := Get_Filename (Source);
         begin
            --  Update the title, in case "save as..." was used.

            if Old_Name /= New_Name then
               Set_Title (Child, New_Name, Base_Name (New_Name));
               Change_Project_Dir (Kernel, Dir_Name (New_Name));
            end if;
         end;
      end;
   end Save_To_File;

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant String :=
           Select_File
             (Title             => -"Open File",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              History           => Get_History (Kernel));

      begin
         if Filename = "" then
            return;
         end if;

         Open_File_Editor (Kernel, Filename);
         Change_Project_Dir (Kernel, Dir_Name (Filename));
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_File;

   -----------------------
   -- On_Open_From_Path --
   -----------------------

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Label  : Gtk_Label;
      Button : Gtk_Widget;
      pragma Unreferenced (Widget, Button);

      Id     : Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if Id.Open_File_Dialog = null then
         Gtk_New (Id.Open_File_Dialog,
                  Title  => -"Open file from project",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);
         Set_Default_Size (Id.Open_File_Dialog, 300, 200);
         Set_Position (Id.Open_File_Dialog, Win_Pos_Mouse);

         Gtk_New (Label, -"Enter file name (use <tab> for completion):");
         Pack_Start (Get_Vbox (Id.Open_File_Dialog), Label, Expand => False);

         Gtk_New (Id.Open_File_Entry);
         Pack_Start (Get_Vbox (Id.Open_File_Dialog), Id.Open_File_Entry,
                     Fill => True, Expand => True);

         Button := Add_Button (Id.Open_File_Dialog, Stock_Ok, Gtk_Response_OK);
         Button := Add_Button
           (Id.Open_File_Dialog, Stock_Cancel, Gtk_Response_Cancel);
         Set_Default_Response (Id.Open_File_Dialog, Gtk_Response_OK);
      else
         Set_Text (Get_Entry (Get_Combo (Id.Open_File_Entry)), "");
      end if;

      Grab_Focus (Get_Entry (Get_Combo (Id.Open_File_Entry)));
      Show_All (Id.Open_File_Dialog);

      declare
         List1 : String_Array_Access := Get_Source_Files
           (Project_View => Get_Project_View (Kernel),
            Recursive    => True,
            Full_Path    => False);
         List2 : String_Array_Access := Get_Predefined_Source_Files (Kernel);
      begin
         Set_Completions
           (Id.Open_File_Entry, new String_Array'(List1.all & List2.all));
         Unchecked_Free (List1);
         Unchecked_Free (List2);
      end;

      if Run (Id.Open_File_Dialog) = Gtk_Response_OK then
         declare
            Text : constant String :=
              Get_Text (Get_Entry (Get_Combo (Id.Open_File_Entry)));
         begin
            Add_Unique_Combo_Entry (Get_Combo (Id.Open_File_Entry), Text);
            Open_File_Editor (Kernel, Text, From_Path => True);
         end;
      end if;

      Hide_All (Id.Open_File_Dialog);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_From_Path;

   ---------------
   -- On_Recent --
   ---------------

   procedure On_Recent
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Item  : constant Gtk_Menu_Item := Gtk_Menu_Item (Widget);
      Label : constant Gtk_Label := Gtk_Label (Get_Child (Item));
   begin
      Open_File_Editor (Kernel, Get_Text (Label));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Recent;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor : Source_Box;
      pragma Unreferenced (Widget, Editor);
   begin
      Editor := Open_File (Kernel, File => "");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_File;

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
      Source  : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      if Source /= null then
         declare
            New_Name : constant String :=
              Select_File
                (Title             => -"Save File As",
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 History           => Get_History (Kernel));

         begin
            if New_Name = "" then
               return;
            else
               Save_To_File (Kernel, New_Name, Success);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_As;

   -----------------
   -- On_Save_All --
   -----------------

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Ignore : Boolean;
      pragma Unreferenced (Widget, Ignore);

   begin
      Ignore := Save_All_MDI_Children (Kernel, Force => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_All;

   --------------
   -- On_Print --
   --------------

   procedure On_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Success : Boolean;
      Child   : constant MDI_Child := Find_Current_Editor (Kernel);
      Source  : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      if Source /= null then
         if Save_Child (Kernel, Child, False) /= Cancel then
            declare
               Cmd : Argument_List_Access := Argument_String_To_List
                 (Get_Pref (Kernel, Print_Command) & " " &
                  Get_Filename (Source));
            begin
               Launch_Process
                 (Kernel, Cmd (Cmd'First).all, Cmd (Cmd'First + 1 .. Cmd'Last),
                  Name => "", Success => Success);
               Free (Cmd);
            end;
         end if;
      end if;
   end On_Print;

   -------------------------
   -- On_Save_All_Editors --
   -------------------------

   procedure On_Save_All_Editors
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Ignore : Boolean;
      pragma Unreferenced (Widget, Ignore);

   begin
      Ignore := Save_All_Editors (Kernel, Force => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_All_Editors;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
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
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
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
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
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
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
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

   ------------------
   -- On_Goto_Line --
   ------------------

   procedure On_Goto_Line
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      On_Goto_Line
        (Editor => Get_Source_Box_From_MDI (Find_Current_Editor (Kernel)),
         Kernel => Kernel);
   end On_Goto_Line;

   -------------------------
   -- On_Goto_Declaration --
   -------------------------

   procedure On_Goto_Declaration
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return;
      end if;

      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Editor,
         Context => Entity_Selection_Context_Access
           (Default_Factory (Kernel, Editor)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Declaration;

   ------------------
   -- On_Goto_Body --
   ------------------

   procedure On_Goto_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return;
      end if;

      Goto_Declaration_Or_Body
        (Kernel, To_Body => True,
         Editor => Editor,
         Context => Entity_Selection_Context_Access
           (Default_Factory (Kernel, Editor)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Body;

   ----------------------
   -- Generate_Body_Cb --
   ----------------------

   procedure Generate_Body_Cb (Data : Process_Data; Status : Integer) is
      Body_Name : constant String := Other_File_Name
        (Data.Kernel, Data.Name.all, Full_Name => True);
   begin
      if Status = 0
        and then Is_Regular_File (Body_Name)
      then
         Open_File_Editor (Data.Kernel, Body_Name);
      end if;
   end Generate_Body_Cb;

   ----------------------
   -- On_Generate_Body --
   ----------------------

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context = null
        or else not (Context.all in File_Selection_Context'Class)
      then
         Console.Insert
           (Kernel, -"No file selected, cannot generate body", Mode => Error);
         return;
      end if;

      declare
         File_Context : constant File_Selection_Context_Access :=
           File_Selection_Context_Access (Context);
         Filename     : constant String := File_Information (File_Context);
         File         : constant String :=
           Directory_Information (File_Context) & Filename;
         Success      : Boolean;
         Args         : Argument_List (1 .. 4);
         Lang         : String := Get_Language_From_File
           (Get_Language_Handler (Kernel), File);

      begin
         if File = "" then
            Console.Insert
              (Kernel, -"No file name, cannot generate body", Mode => Error);
            return;
         end if;

         To_Lower (Lang);

         if Lang /= "ada" then
            Console.Insert
              (Kernel, -"Body generation of non Ada file not yet supported",
               Mode => Error);
            return;
         end if;

         if Save_All_MDI_Children (Kernel, Force => False) = False then
            return;
         end if;

         Args (1) := new String'("stub");
         Args (2) := new String'("-P" & Get_Subproject_Name (Kernel, File));
         Args (3) := new String'(File);
         Args (4) := new String'(Dir_Name (File));

         declare
            Scenar : Argument_List_Access := Argument_String_To_List
              (Scenario_Variables_Cmd_Line (Kernel, GNAT_Syntax));
         begin
            Launch_Process
              (Kernel, "gnat", Args (1 .. 2) & Scenar.all & Args (3 .. 4),
               "", null,
               Generate_Body_Cb'Access, File, Success);
            Free (Args);
            Free (Scenar);
         end;

         if Success then
            Print_Message
              (Glide_Window (Get_Main_Window (Kernel)).Statusbar,
               Help, -"Generating body...");
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Generate_Body;

   ---------------------
   -- Pretty_Print_Cb --
   ---------------------

   procedure Pretty_Print_Cb (Data : Process_Data; Status : Integer) is
      function Pretty_Name (Name : String) return String;
      --  Return the name of the pretty printed file.

      function Pretty_Name (Name : String) return String is
      begin
         return Name & ".pp";
      end Pretty_Name;

   begin
      if Status = 0 then
         Open_File_Editor (Data.Kernel, Pretty_Name (Data.Name.all));
      end if;
   end Pretty_Print_Cb;

   -----------------------
   -- Comment_Uncomment --
   -----------------------

   procedure Comment_Uncomment
     (Kernel : Kernel_Handle; Comment : Boolean)
   is
      Context    : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

      Area         : File_Area_Context_Access;
      File_Context : File_Selection_Context_Access;
      Start_Line   : Integer;
      End_Line     : Integer;


      use String_List_Utils.String_List;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_File_Information
          (File_Selection_Context_Access (Context))
        and then Has_Directory_Information
          (File_Selection_Context_Access (Context))
      then
         File_Context := File_Selection_Context_Access (Context);

         declare
            Lang       : Language_Access;
            File       : constant String :=
              Directory_Information (File_Context)
              & File_Information (File_Context);

            Lines      : List;
            Length     : Integer := 0;

         begin
            if Context.all in File_Area_Context'Class then
               Area := File_Area_Context_Access (Context);
               Get_Area (Area, Start_Line, End_Line);

            elsif Context.all in Entity_Selection_Context'Class
              and then Has_Line_Information
                (Entity_Selection_Context_Access (Context))
            then
               Start_Line := Line_Information
                 (Entity_Selection_Context_Access (Context));

               End_Line := Start_Line;
            else
               return;
            end if;

            Lang := Get_Language_From_File
              (Get_Language_Handler (Kernel), File);

            --  Create a list of lines, in order to perform the replace
            --  as a block.

            for J in Start_Line .. End_Line loop
               declare
                  Line : constant String :=
                    Interpret_Command
                      (Kernel, "get_chars " & File & " -l " & Image (J));

               begin
                  Length := Length + Line'Length;

                  if Line = "" then
                     Append (Lines, "");
                  else
                     if Comment then
                        Append (Lines,
                                Comment_Line
                                  (Lang, Line (Line'First .. Line'Last - 1)));
                     else
                        Append (Lines,
                                Uncomment_Line
                                  (Lang, Line (Line'First .. Line'Last - 1)));
                     end if;
                  end if;
               end;
            end loop;

            --  Create a String containing the modified lines.

            declare
               L : Integer := 0;
               N : List_Node := First (Lines);
            begin
               while N /= Null_Node loop
                  L := L + Data (N)'Length + 1;
                  N := Next (N);
               end loop;

               declare
                  S    : String (1 .. L);
                  Args : List;
               begin
                  N := First (Lines);
                  L := 1;

                  while N /= Null_Node loop
                     S (L .. L + Data (N)'Length) := Data (N) & ASCII.LF;
                     L := L + Data (N)'Length + 1;
                     N := Next (N);
                  end loop;

                  Append (Args, File);
                  Append (Args, "-l");
                  Append (Args, Image (Start_Line));
                  Append (Args, "-a");
                  Append (Args, Image (Length));
                  Append (Args, """" & S & """");

                  Interpret_Command (Kernel, "replace_text", Args);

                  Free (Args);
               end;
            end;
         end;
      end if;
   end Comment_Uncomment;

   ----------------------
   -- On_Comment_Lines --
   ----------------------

   procedure On_Comment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Comment_Uncomment (Kernel, Comment => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Comment_Lines;

   ------------------------
   -- On_Uncomment_Lines --
   ------------------------

   procedure On_Uncomment_Lines
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      Comment_Uncomment (Kernel, Comment => False);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Uncomment_Lines;

   ---------------------
   -- On_Pretty_Print --
   ---------------------

   procedure On_Pretty_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context = null
        or else not (Context.all in File_Selection_Context'Class)
      then
         Console.Insert
           (Kernel, -"No file selected, cannot pretty print",
            Mode => Error);
         return;
      end if;

      declare
         File_Context : constant File_Selection_Context_Access :=
           File_Selection_Context_Access (Context);
         Filename     : constant String := File_Information (File_Context);
         File         : constant String :=
           Directory_Information (File_Context) & Filename;
         Project      : constant String :=
           Get_Subproject_Name (Kernel, Filename);
         Success      : Boolean;
         Args, Vars   : Argument_List_Access;
         Lang         : String := Get_Language_From_File
           (Get_Language_Handler (Kernel), File);

      begin
         if File = "" then
            Console.Insert
              (Kernel, -"No file name, cannot pretty print",
               Mode => Error);
            return;
         end if;

         To_Lower (Lang);

         if Lang /= "ada" then
            Console.Insert
              (Kernel, -"Pretty printing of non Ada file not yet supported",
               Mode => Error);
            return;
         end if;

         if Save_All_MDI_Children (Kernel, Force => False) = False then
            return;
         end if;

         if Project = "" then
            Args := new Argument_List'
              (new String'("pretty"), new String'(File));

         else
            Vars := Argument_String_To_List
              (Scenario_Variables_Cmd_Line (Kernel, GNAT_Syntax));
            Args := new Argument_List'
              ((1 => new String'("pretty"),
                2 => new String'("-P" & Project),
                3 => new String'(File)) & Vars.all);
            Unchecked_Free (Vars);
         end if;

         Launch_Process
           (Kernel, "gnat", Args.all, "", null,
            Pretty_Print_Cb'Access, File, Success);
         Free (Args);

         if Success then
            Print_Message
              (Glide_Window (Get_Main_Window (Kernel)).Statusbar,
               Help, -"Pretty printing...");
         end if;
      end;

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

      Source    : Source_Box;
      Edit      : Source_Editor_Box;
      MDI       : constant MDI_Window := Get_MDI (Kernel);

   begin
      if Mime_Type = Mime_Source_File then
         declare
            File        : constant String  := Get_String (Data (Data'First));
            Line        : constant Gint    := Get_Int (Data (Data'First + 1));
            Column      : constant Gint    := Get_Int (Data (Data'First + 2));
            Column_End  : constant Gint    := Get_Int (Data (Data'First + 3));
            Highlight   : constant Boolean :=
              Get_Boolean (Data (Data'First + 4));
            New_File    : constant Boolean :=
              Get_Boolean (Data (Data'First + 6));
            The_Data    : Source_Editor_Module :=
              Source_Editor_Module (Src_Editor_Module_Id);
            Iter        : Child_Iterator := First_Child (MDI);
            Child       : MDI_Child;
            No_Location : Boolean := False;
            Get_Focus   : Boolean;

         begin
            if Line = -1 then
               loop
                  Child := Get (Iter);

                  exit when Child = null;

                  if Get_Widget (Child).all in Source_Box_Record'Class
                    and then File_Equal (Get_Title (Child), File)
                  then
                     Destroy (Source_Box (Get_Widget (Child)));
                  end if;

                  Next (Iter);
               end loop;

               return True;

            else
               if The_Data.Location_Open_Id /= 0 then
                  Idle_Remove (The_Data.Location_Open_Id);
               end if;

               if Line = 0 then
                  No_Location := True;
               end if;

               Get_Focus := not Console_Has_Focus (Kernel);
               Child := Get_Focus_Child (Get_MDI (Kernel));
               Source := Open_File
                 (Kernel, File,
                  Create_New => New_File,
                  Focus      => Get_Focus);

               --  Only grab again the focus on Child (in Location_Callback)
               --  if the focus was changed by Open_File, and an interactive
               --  console had the focus previousely.

               if Get_Focus_Child (Get_MDI (Kernel)) = Child then
                  Child := null;
               end if;

               if Source /= null then
                  Edit := Source.Editor;
               end if;

               if Edit /= null
                 and then not No_Location
               then
                  Trace (Me, "Setup editor to go to line,col="
                         & Line'Img & Column'Img);
                  --  For some reason, we can not directly call
                  --  Set_Cursor_Location, since the source editor won't be
                  --  scrolled the first time the editor is displayed. Doing
                  --  this in an idle callback ensures that all the proper
                  --  events and initializations have taken place before we
                  --  try to scroll the editor.

                  The_Data.Location_Open_Id := Location_Idle.Add
                    (Location_Callback'Access,
                     (Edit,
                      Natural (Line),
                      Natural (Column),
                      Natural (Column_End),
                      Get_Focus,
                      Child),
                     Destroy => Location_Destroy'Access);

                  if Highlight then
                     Highlight_Line (Edit, Natural (Line));
                  end if;
               end if;

               if Edit /= null
                 and then not Highlight
               then
                  Cancel_Highlight_Line (Edit);
               end if;

               return Edit /= null;
            end if;
         end;

      elsif Mime_Type = Mime_File_Line_Info then
         declare
            File  : constant String  := Get_String (Data (Data'First));
            Id    : constant String  := Get_String (Data (Data'First + 1));
            Info  : constant Line_Information_Data :=
              To_Line_Information (Get_Address (Data (Data'First + 2)));
            Stick_To_Data : constant Boolean :=
              Get_Boolean (Data (Data'First + 3));
            Every_Line : constant Boolean :=
              Get_Boolean (Data (Data'First + 4));
            Child : MDI_Child;
            Iter  : Child_Iterator := First_Child (MDI);

            procedure Apply_Mime_On_Child (Child : MDI_Child);
            --  Apply the mime information on Child.

            procedure Apply_Mime_On_Child (Child : MDI_Child) is
            begin
               if Info'First = 0 then
                  Create_Line_Information_Column
                    (Source_Box (Get_Widget (Child)).Editor,
                     Id,
                     Stick_To_Data,
                     Every_Line);

               elsif Info'Length = 0 then
                  Remove_Line_Information_Column
                    (Source_Box (Get_Widget (Child)).Editor, Id);

               else
                  Add_File_Information
                    (Source_Box (Get_Widget (Child)).Editor, Id, Info);
               end if;
            end Apply_Mime_On_Child;

         begin
            --  Look for the corresponding file editor.

            if File = "" then
               loop
                  Child := Get (Iter);

                  exit when Child = null;

                  if Get_Widget (Child).all in Source_Box_Record'Class then
                     Apply_Mime_On_Child (Child);
                  end if;

                  Next (Iter);
               end loop;

               return True;

            else
               Child := Find_Editor (Kernel, File);

               if Child /= null then
                  --  The editor was found.
                  Apply_Mime_On_Child (Child);

                  return True;
               end if;
            end if;
         end;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Mime_Action;

   ------------------
   -- On_Edit_File --
   ------------------

   procedure On_Edit_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
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
      File  : File_Selection_Context_Access;
      Mitem : Gtk_Menu_Item;

   begin
      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

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
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access is
   begin
      return Get_Contextual_Menu (Kernel, Editor, null, null);
   end Default_Factory;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      C : constant Source_Box := Source_Box (Child);
   begin
      return Default_Factory (Kernel, C.Editor);
   end Default_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File             : constant String := '/' & (-"File") & '/';
      Save             : constant String := File & (-"Save...") & '/';
      Edit             : constant String := '/' & (-"Edit") & '/';
      Navigate         : constant String := '/' & (-"Navigate") & '/';
      Mitem            : Gtk_Menu_Item;
      Button           : Gtk_Button;
      Toolbar          : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Undo_Redo        : Undo_Redo_Information;
      Selector         : Scope_Selector;
      Extra            : Files_Extra_Scope;

   begin
      Src_Editor_Module_Id := new Source_Editor_Module_Record;
      Register_Module
        (Module                  => Src_Editor_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => Src_Editor_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Source_Editor_Contextual'Access,
         Mime_Handler            => Mime_Action'Access,
         MDI_Child_Tag           => Source_Box_Record'Tag,
         Default_Context_Factory => Default_Factory'Access,
         Save_Function           => Save_Function'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      --  Menus

      Register_Menu (Kernel, File, -"_Open...",  Stock_Open,
                     On_Open_File'Access, null,
                     GDK_F3, Ref_Item => -"Save...");
      Register_Menu (Kernel, File, -"Open _From Project...",  Stock_Open,
                     On_Open_From_Path'Access, null,
                     GDK_F3, Shift_Mask, Ref_Item => -"Save...");

      Source_Editor_Module (Src_Editor_Module_Id).Recent_Menu_Item :=
        Register_Menu (Kernel, File, -"_Recent", "", null,
                       Ref_Item   => -"Open From Project...",
                       Add_Before => False);

      Refresh_Recent_Menu (Kernel);

      Register_Menu (Kernel, File, -"_New", Stock_New, On_New_File'Access,
                     Ref_Item => -"Open...");
      Register_Menu (Kernel, File, -"New _View", "", On_New_View'Access,
                     Ref_Item => -"Open...");

      Register_Menu (Kernel, File, -"_Save", Stock_Save,
                     On_Save'Access, null,
                     GDK_S, Control_Mask, Ref_Item => -"Save...");
      Register_Menu (Kernel, Save, -"_File As...", Stock_Save_As,
                     On_Save_As'Access, Ref_Item => -"Desktop");
      Register_Menu (Kernel, Save, -"All _Editors", "",
                     On_Save_All_Editors'Access, Sensitive => False,
                     Ref_Item => -"Desktop");
      Register_Menu (Kernel, Save, -"_All", "",
                     On_Save_All'Access, Ref_Item => -"Desktop");

      Register_Menu (Kernel, File, -"_Print", Stock_Print, On_Print'Access,
                     Ref_Item => -"Exit");
      Gtk_New (Mitem);
      Register_Menu (Kernel, File, Mitem, Ref_Item => -"Exit");

      --  Note: callbacks for the Undo/Redo menu items will be added later
      --  by each source editor.

      Undo_Redo.Undo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"_Undo", Stock_Undo,
                       null, null,
                       GDK_Z, Control_Mask, Ref_Item => -"Preferences",
                       Sensitive => False);
      Undo_Redo.Redo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"_Redo", Stock_Redo,
                       null, null,
                       GDK_R, Control_Mask, Ref_Item => -"Preferences",
                       Sensitive => False);

      Gtk_New (Mitem);
      Register_Menu
        (Kernel, Edit, Mitem, Ref_Item => "Redo", Add_Before => False);

      Register_Menu (Kernel, Edit, -"_Cut",  Stock_Cut,
                     On_Cut'Access, null,
                     GDK_Delete, Shift_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"C_opy",  Stock_Copy,
                     On_Copy'Access, null,
                     GDK_Insert, Control_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"P_aste",  Stock_Paste,
                     On_Paste'Access, null,
                     GDK_Insert, Shift_Mask,
                     Ref_Item => -"Preferences");

      --  ??? This should be bound to Ctrl-A, except this would interfer with
      --  Emacs keybindings for people who want to use them.
      Register_Menu (Kernel, Edit, -"_Select All",  "",
                     On_Select_All'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"Comment Lines", "",
                     On_Comment_Lines'Access, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Uncomment Lines", "",
                     On_Uncomment_Lines'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"_Generate Body", "",
                     On_Generate_Body'Access, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"P_retty Print", "",
                     On_Pretty_Print'Access, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Navigate, -"Goto _Line...", Stock_Jump_To,
                     On_Goto_Line'Access, null,
                     GDK_G, Control_Mask,
                     Ref_Item => -"Goto File Spec<->Body");
      Register_Menu (Kernel, Navigate, -"Goto _Declaration", Stock_Home,
                     On_Goto_Declaration'Access, Ref_Item => -"Goto Line...");
      Register_Menu (Kernel, Navigate, -"Goto _Body", "",
                     On_Goto_Body'Access, Ref_Item => -"Goto Line...");

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
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Cut'Access),
         Kernel_Handle (Kernel));
      Button := Insert_Stock
        (Toolbar, Stock_Copy, -"Copy to Clipboard", Position => 8);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Copy'Access),
         Kernel_Handle (Kernel));
      Button := Insert_Stock
        (Toolbar, Stock_Paste, -"Paste from Clipboard", Position => 9);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Paste'Access),
         Kernel_Handle (Kernel));
      Kernel_Callback.Connect
        (Kernel, File_Saved_Signal,
         File_Saved_Cb'Access,
         Kernel_Handle (Kernel));

      Undo_Redo_Data.Set (Kernel, Undo_Redo, Undo_Redo_Id);

      Preferences_Changed (Kernel, Kernel_Handle (Kernel));

      Kernel_Callback.Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         User_Data   => Kernel_Handle (Kernel));

      Source_Editor_Module (Src_Editor_Module_Id).File_Closed_Id :=
        Kernel_Callback.Connect
          (Kernel,
           File_Closed_Signal,
           File_Closed_Cb'Access,
           Kernel_Handle (Kernel));

      Source_Editor_Module (Src_Editor_Module_Id).File_Edited_Id :=
        Kernel_Callback.Connect
          (Kernel,
           File_Edited_Signal,
           File_Edited_Cb'Access,
           Kernel_Handle (Kernel));

      Register_Command
        (Kernel,
         Command => "edit",
         Help    => -"Usage:" & ASCII.LF
         & "  edit [-l line] [-c column] file_name" & ASCII.LF
         & (-"Open a file editor for file_name."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "create_mark",
         Help    => -"Usage:" & ASCII.LF
         & "  create_mark [-l line] [-c column] [-L length] file_name"
           & ASCII.LF
           & (-("Create a mark for file_name," &
                " at position given by line and column.")) & ASCII.LF
           & (-("Length corresponds to the text length to highlight"
                & " after the mark.")) & ASCII.LF
           & (-("The identifier of the mark is returned.")) & ASCII.LF
           & (-("Use the command goto_mark to jump to this mark.")),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "goto_mark",
         Help    => -"Usage:" & ASCII.LF
         & "  goto_mark identifier" & ASCII.LF
         & (-"Jump to the location of the mark corresponding to identifier."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "get_chars",
         Help    => -"Usage:" & ASCII.LF
         & "  get_chars {mark_identifier | -l line -c col} "
         & (-"[-b before] [-a after]") & ASCII.LF
         & (-"Get the characters around a certain mark or position.")
         & ASCII.LF
         & (-"Returns string between <before> characters before the mark")
         & ASCII.LF
         & (-"and <after> characters after the position.") & ASCII.LF
         & (-"If <before> or <after> is omitted, the bounds will be")
         & ASCII.LF
         & (-"at the beginning and/or the end of the line."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "get_line",
         Help => -"Usage:" & ASCII.LF
         & "   mark" & ASCII.LF
         & "Returns the current line of mark.",
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "get_column",
         Help => -"Usage:" & ASCII.LF
         & "  get_column mark" & ASCII.LF
         & "Returns the current column of mark.",
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "get_file",
         Help => -"Usage:" & ASCII.LF
         & "  get_file mark" & ASCII.LF
         & "Returns the current file of mark.",
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "get_last_line",
         Help => -"Usage:" & ASCII.LF
         & "  get_last_line file" & ASCII.LF
         & "Returns the number of the last line in file.",
         Handler => Edit_Command_Handler'Access);


      Register_Command
        (Kernel,
         Command => "get_buffer",
         Help => -"Usage:" & ASCII.LF
         & "  get_buffer file" & ASCII.LF
         & "Returns the text contained in the current buffer for file.",
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "replace_text",
         Help    => -"Usage:" & ASCII.LF
         & "  replace_text {mark_identifier | -l line -c col} "
         & (-"[-b before] [-a after] ""text""") & ASCII.LF
         & (-"Replace the characters around a certain mark or position.")
         & ASCII.LF
         & (-"Replace string between <before> characters before the mark")
         & ASCII.LF
         & (-"and <after> characters after the position.") & ASCII.LF
         & (-"If <before> or <after> is omitted, the bounds will be")
         & ASCII.LF
         & (-"at the beginning and/or the end of the line."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "edit_undo",
         Help    => -"Usage:" & ASCII.LF
         & " edit_undo file"
         & (-"Undo the last edition command for file."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "edit_redo",
         Help    => -"Usage:" & ASCII.LF
         & " edit_redo file"
         & (-"Redo the last edition command for file."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "close",
         Help    => -"Usage:" & ASCII.LF
         & "  close file_name" & ASCII.LF
         & (-"Close all file editors for file_name."),
         Handler => Edit_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "save",
         Help    => -"Usage:" & ASCII.LF
         & "  save [-i] [all]" & ASCII.LF
         & (-"Save current or all files.") & ASCII.LF
         & (-"  -i: prompt before each save"),
         Handler => Edit_Command_Handler'Access);

      --  Register the search functions

      Gtk_New (Selector, Kernel);
      Gtk_New (Extra, Kernel);

      declare
         Name  : constant String := -"Current File";
         Name2 : constant String := -"Files From Project";
         Name3 : constant String := -"Files...";
      begin
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name'Length,
               Label             => Name,
               Factory           => Current_File_Factory'Access,
               Extra_Information => Gtk_Widget (Selector),
               Id                => Src_Editor_Module_Id,
               Mask              => All_Options));
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name2'Length,
               Label             => Name2,
               Factory           => Files_From_Project_Factory'Access,
               Extra_Information => Gtk_Widget (Selector),
               Id                => null,
               Mask              => All_Options
                 and not Search_Backward));
         Register_Search_Function
           (Kernel => Kernel,
            Data   =>
              (Length            => Name3'Length,
               Label             => Name3,
               Factory           => Files_Factory'Access,
               Extra_Information => Gtk_Widget (Extra),
               Id                => null,
               Mask              => All_Options
                 and not Search_Backward));
      end;
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);
      Id                        : Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Pref_Display_Line_Numbers : constant Boolean :=
        Get_Pref (Kernel, Display_Line_Numbers);

   begin
      if Pref_Display_Line_Numbers = Id.Display_Line_Numbers then
         return;
      end if;

      Id.Display_Line_Numbers := Pref_Display_Line_Numbers;

      --  Connect necessary signal to display line numbers.
      if Pref_Display_Line_Numbers then
         if Id.Source_Lines_Revealed_Id = No_Handler then
            Id.Source_Lines_Revealed_Id :=
              Kernel_Callback.Connect
                (Kernel,
                 Source_Lines_Revealed_Signal,
                 On_Lines_Revealed'Access,
                 Kernel);
            Create_Line_Information_Column
              (Kernel,
               "",
               Src_Editor_Module_Name,
               Stick_To_Data => False,
               Every_Line    => True);
         end if;

      elsif Id.Source_Lines_Revealed_Id /= No_Handler then
         Gtk.Handlers.Disconnect
           (Kernel, Id.Source_Lines_Revealed_Id);
         Id.Source_Lines_Revealed_Id := No_Handler;

         Remove_Line_Information_Column (Kernel, "", Src_Editor_Module_Name);
      end if;
   end Preferences_Changed;

   -------------------------
   -- Refresh_Recent_Menu --
   -------------------------

   procedure Refresh_Recent_Menu
     (Kernel : access Kernel_Handle_Record'Class)
   is
      The_Data    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Value       : constant String_List_Access := Get_History
        (Get_History (Kernel).all, Hist_Key);
      Recent_Menu : Gtk_Menu;
      Mitem       : Gtk_Menu_Item;
   begin
      if Get_Submenu (The_Data.Recent_Menu_Item) /= null then
         Remove_Submenu (The_Data.Recent_Menu_Item);
      end if;

      Gtk_New (Recent_Menu);
      Set_Submenu (The_Data.Recent_Menu_Item, Gtk_Widget (Recent_Menu));

      if Value /= null then
         for V in Value'Range loop
            Gtk_New (Mitem, Value (V).all);
            Append (Recent_Menu, Mitem);

            Kernel_Callback.Connect
              (Mitem,
               "activate",
               Kernel_Callback.To_Marshaller (On_Recent'Access),
               Kernel_Handle (Kernel));
         end loop;

         Show_All (Recent_Menu);
      end if;
   end Refresh_Recent_Menu;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Source_Editor_Module_Record) is
   begin
      if Id.Open_File_Dialog /= null then
         Destroy (Id.Open_File_Dialog);
      end if;

      String_List_Utils.String_List.Free (Id.Unopened_Files);
      Mark_Identifier_List.Free (Id.Stored_Marks);
   end Destroy;

   -----------------
   -- Find_Editor --
   -----------------

   function Find_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String) return Gtkada.MDI.MDI_Child
   is
      Iter  : Child_Iterator := First_Child (Get_MDI (Kernel));
      Child : MDI_Child;
   begin
      if File /= Base_Name (File) then
         loop
            Child := Get (Iter);

            exit when Child = null
              or else (Get_Widget (Child).all in Source_Box_Record'Class
                       and then File_Equal (Get_Title (Child), File));
            Next (Iter);
         end loop;

         return Child;

      else
         loop
            Child := Get (Iter);

            exit when Child = null
              or else (Get_Widget (Child).all in Source_Box_Record'Class
                       and then File_Equal (Base_Name (Get_Title (Child)),
                                            File));
            Next (Iter);
         end loop;

         return Child;
      end if;
   end Find_Editor;

end Src_Editor_Module;
