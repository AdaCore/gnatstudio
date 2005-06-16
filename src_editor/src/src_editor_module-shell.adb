-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
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

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;
with Src_Editor_View;           use Src_Editor_View;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer.Text_Handling; use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Contexts;              use Src_Contexts;
with Find_Utils;                use Find_Utils;
with Basic_Types;               use Basic_Types;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with GPS.Intl;                  use GPS.Intl;
with String_Utils;              use String_Utils;
with Casing_Exceptions;         use Casing_Exceptions;
with VFS;                       use VFS;
with Traces;                    use Traces;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Exceptions;            use Ada.Exceptions;

with Glib.Convert;              use Glib.Convert;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Handlers;

package body Src_Editor_Module.Shell is
   Filename_Cst          : aliased constant String := "filename";
   Line_Cst              : aliased constant String := "line";
   Col_Cst               : aliased constant String := "column";
   Length_Cst            : aliased constant String := "length";
   Pattern_Cst           : aliased constant String := "pattern";
   Case_Cst              : aliased constant String := "case_sensitive";
   Regexp_Cst            : aliased constant String := "regexp";
   Scope_Cst             : aliased constant String := "scope";
   Force_Cst             : aliased constant String := "force";
   All_Cst               : aliased constant String := "all";
   Interactive_Cst       : aliased constant String := "interactive";
   Current_Line_Only_Cst : aliased constant String := "current_line_only";
   Before_Cst            : aliased constant String := "before";
   After_Cst             : aliased constant String := "after";
   Name_Cst              : aliased constant String := "name";
   First_Line_Cst        : aliased constant String := "first_line";
   Start_Column_Cst      : aliased constant String := "start_column";
   Last_Line_Cst         : aliased constant String := "last_line";
   End_Column_Cst        : aliased constant String := "end_column";
   Writable_Cst          : aliased constant String := "writable";
   Position_Cst          : aliased constant String := "position";

   Edit_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access,
      4 => Length_Cst'Access,
      5 => Force_Cst'Access,
      6 => Position_Cst'Access);
   Create_Mark_Parameters : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access,
      4 => Length_Cst'Access);
   File_Search_Parameters : constant Cst_Argument_List :=
     (1 => Pattern_Cst'Access,
      2 => Case_Cst'Access,
      3 => Regexp_Cst'Access,
      4 => Scope_Cst'Access);
   Save_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Interactive_Cst'Access,
      2 => All_Cst'Access);
   Indent_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Current_Line_Only_Cst'Access);
   Get_Chars_Args : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access,
      4 => Before_Cst'Access,
      5 => After_Cst'Access);
   Case_Exception_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Set_Writable_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access,
      2 => Writable_Cst'Access);
   Select_Text_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => First_Line_Cst'Access,
      2 => Last_Line_Cst'Access,
      3 => Start_Column_Cst'Access,
      4 => End_Column_Cst'Access);

   type Child_Triplet is array (1 .. 3) of Gtkada.MDI.MDI_Child;
   type Child_Triplet_Access is access Child_Triplet;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Child_Triplet, Child_Triplet_Access);

   package Child_Triplet_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Child_Triplet_Access);

   procedure Edit_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the source editor module.

   procedure Current_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure File_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Project_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Common_Search_Command_Handler
     (Data  : in out Callback_Data'Class;
      Files : VFS.File_Array_Access);
   --  Interactive command handler for the source editor module (Search part)

   procedure On_Raise_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access);
   --  Called when synchronized editor Child in Triplet is raised.

   procedure On_Delete_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access);
   --  Called when synchronized editor Child in Triplet is deleted.

   ---------------------
   -- On_Delete_Child --
   ---------------------

   procedure On_Delete_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access)
   is
      All_Null : Boolean := True;
   begin
      for C in Triplet'Range loop
         if Triplet (C) = MDI_Child (Child) then
            Triplet (C) := null;
         end if;

         if Triplet (C) /= null then
            All_Null := False;
         end if;
      end loop;

      if All_Null then
         --  All editors in Triplet are closed: free memory associated to it
         declare
            X : Child_Triplet_Access := Triplet;
         begin
            Unchecked_Free (X);
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Delete_Child;

   --------------------
   -- On_Raise_Child --
   --------------------

   procedure On_Raise_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access) is
   begin
      for C in Triplet'Range loop
         if Triplet (C) /= null
           and then Triplet (C) /= MDI_Child (Child)
           and then not Is_Floating (Triplet (C))
           and then not Is_Raised (Triplet (C))
           and then Get_Parent (Triplet (C)) /= Get_Parent (Child)
         then
            Raise_Child (Triplet (C), False);
         end if;
      end loop;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Raise_Child;

   -----------------------------------
   -- Common_Search_Command_Handler --
   -----------------------------------

   procedure Common_Search_Command_Handler
     (Data    : in out Callback_Data'Class;
      Files   : File_Array_Access)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Context : Files_Project_Context_Access;
      Pattern : constant String  := Nth_Arg (Data, 2);
      Casing  : constant Boolean := Nth_Arg (Data, 3, False);
      Regexp  : constant Boolean := Nth_Arg (Data, 4, False);
      Scope   : constant String  := Nth_Arg (Data, 5, "whole");
      S       : Search_Scope;

      function Callback (Match : Match_Result) return Boolean;
      --  Store the result of the match in Data

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Set_Return_Value
           (Data,
            Create_File_Location
              (Get_Script (Data),
               Create_File (Get_Script (Data), Current_File (Context)),
               Match.Line,
               Match.Column));
         return True;
      end Callback;

   begin
      if Scope = "whole" then
         S := Whole;
      elsif Scope = "comments" then
         S := Comments_Only;
      elsif Scope = "strings" then
         S := Strings_Only;
      elsif Scope = "code" then
         S := All_But_Comments;
      else
         S := Whole;
      end if;

      Context := Files_From_Project_Factory
        (Scope           => S,
         All_Occurrences => True);
      Set_File_List (Context, Files);
      Set_Context
        (Context,
         Look_For => Pattern,
         Options  => (Case_Sensitive => Casing,
                      Whole_Word     => False,
                      Regexp         => Regexp));

      Set_Return_Value_As_List (Data);

      while Search
        (Context  => Context,
         Handler  => Get_Language_Handler (Kernel),
         Kernel   => Kernel,
         Callback => Callback'Unrestricted_Access)
      loop
         --  No need to delay, since the search is done in same process.
         null;
      end loop;
   end Common_Search_Command_Handler;

   ------------------------------------
   -- Current_Search_Command_Handler --
   ------------------------------------

   procedure Current_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);

      Id      : constant Source_Editor_Module :=
         Source_Editor_Module (Src_Editor_Module_Id);

      Inst    : constant Class_Instance :=
                 Nth_Arg (Data, 1, Get_File_Class (Kernel));
      File    : constant Virtual_File := Get_File (Get_Data (Inst));
      Pattern : constant String := Nth_Arg (Data, 2);
      Casing  : constant Boolean := Nth_Arg (Data, 3, False);
      Regexp  : constant Boolean := Nth_Arg (Data, 4, False);

      Dummy   : Boolean;
      pragma Unreferenced (Dummy);

      function Callback (Match : Match_Result) return Boolean;
      --  Store the result of the match in Data

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Set_Return_Value
           (Data,
            Create_File_Location
              (Get_Script (Data),
               Create_File (Get_Script (Data), File),
               Match.Line,
               Match.Column));
         return True;
      end Callback;

   begin
      if Id.Search_Context = null
        or else Id.Search_Pattern = null
        or else Id.Search_Pattern.all /= Pattern
        or else Id.Search_File /= File
      then
         Free (Id.Search_Pattern);
         Id.Search_Pattern := new String'(Pattern);
         Id.Search_File    := File;
         Id.Search_Context := Files_From_Project_Factory (Whole, False);
         Set_File_List (Id.Search_Context, new File_Array'(1 => File));

         Set_Context
           (Id.Search_Context,
            Look_For => Pattern,
            Options  => (Case_Sensitive => Casing,
                         Whole_Word     => False,
                         Regexp         => Regexp));
      end if;

      Dummy := Search
        (Context  => Id.Search_Context,
         Handler  => Get_Language_Handler (Kernel),
         Kernel   => Kernel,
         Callback => Callback'Unrestricted_Access);
   end Current_Search_Command_Handler;

   ---------------------------------
   -- File_Search_Command_Handler --
   ---------------------------------

   procedure File_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Inst   : constant Class_Instance :=
                 Nth_Arg (Data, 1, Get_File_Class (Kernel));
      Info   : constant File_Info := Get_Data (Inst);
   begin
      Name_Parameters (Data, File_Search_Parameters);
      Common_Search_Command_Handler
        (Data, new File_Array'(1 => Get_File (Info)));
   end File_Search_Command_Handler;

   ------------------------------------
   -- Project_Search_Command_Handler --
   ------------------------------------

   procedure Project_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Project   : constant Project_Type := Get_Data (Data, 1);
      Recursive : Boolean;
   begin
      Name_Parameters (Data, File_Search_Parameters);
      Recursive := Nth_Arg (Data, 5, True);
      Common_Search_Command_Handler
        (Data, Get_Source_Files (Project, Recursive));
   end Project_Search_Command_Handler;

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   procedure Edit_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Id     : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Length : Natural := 0;
      Line   : Natural := 1;
      Column : Natural := 1;
      Force  : Boolean;

   begin
      if Command = "edit" or else Command = "create_mark" then
         if Command = "edit" then
            Name_Parameters (Data, Edit_Cmd_Parameters);
         else
            Name_Parameters (Data, Create_Mark_Parameters);
         end if;

         declare
            File : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel, Use_Source_Path => True);
            Position : Natural;
         begin
            Line   := Nth_Arg (Data, 2, Default => 1);
            Column := Nth_Arg (Data, 3, Default => 1);
            Length := Nth_Arg (Data, 4, Default => 0);

            if File /= VFS.No_File then
               if Command = "edit" then
                  Force := Nth_Arg (Data, 5, Default => False);
                  Position := Nth_Arg
                    (Data, 6, Default => Natural (Position_Default));

                  if Length = 0 then
                     Open_File_Editor
                       (Kernel,
                        File,
                        Line,
                        Column,
                        Enable_Navigation => False,
                        Force_Reload => Force,
                        Position => Child_Position (Position));
                  else
                     Open_File_Editor
                       (Kernel,
                        File,
                        Line,
                        Column,
                        Column + Length,
                        Enable_Navigation => False,
                        Force_Reload => Force);
                  end if;

               elsif Command = "create_mark" then
                  declare
                     Box         : Source_Box;
                     Child       : MDI_Child;
                     Mark_Record : Mark_Identifier_Record;
                  begin
                     Child := Find_Editor (Kernel, File);

                     --  Create a new mark record and insert it in the list.

                     Mark_Record.File := File;
                     Mark_Record.Id   := Id.Next_Mark_Id;
                     Mark_Record.Line := Line;
                     Id.Next_Mark_Id  := Id.Next_Mark_Id + 1;

                     Mark_Record.Length := Length;

                     if Child /= null then
                        Box := Source_Box (Get_Widget (Child));
                        Mark_Record.Mark :=
                          Create_Mark
                            (Get_Buffer (Box.Editor),
                             Editable_Line_Type (Line),
                             Column);
                     else
                        Mark_Record.Line := Line;
                        Mark_Record.Column := Column;
                        Add_Unique_Sorted
                          (Id.Unopened_Files, Full_Name (File).all);
                     end if;

                     Mark_Identifier_List.Append
                       (Id.Stored_Marks, Mark_Record);

                     Set_Return_Value (Data, Image (Mark_Record.Id));
                  end;
               end if;
            end if;
         end;

      elsif Command = "indent" then
         Name_Parameters (Data, Indent_Cmd_Parameters);
         declare
            Current_Line_Only : constant Boolean := Nth_Arg (Data, 1, False);
            Child : constant MDI_Child := Find_Current_Editor (Kernel);
            Box   : Source_Box;
         begin
            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if not Get_Editable (Get_View (Box.Editor))
                 or else not Do_Indentation
                   (Get_Buffer (Box.Editor), Current_Line_Only)
               then
                  Set_Error_Msg (Data, -"Could not indent selection");
               end if;
            end if;
         end;

      elsif Command = "indent_buffer" then
         declare
            Child    : constant MDI_Child := Find_Current_Editor (Kernel);
            Box      : Source_Box;
            Buffer   : Source_Buffer;
            From, To : Gtk_Text_Iter;

         begin
            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               Buffer := Get_Buffer (Box.Editor);

               Get_Start_Iter (Buffer, From);
               Get_End_Iter (Buffer, To);

               if not Get_Editable (Get_View (Box.Editor))
                 or else not Do_Indentation
                   (Get_Buffer (Box.Editor), From, To)
               then
                  Set_Error_Msg (Data, -"Could not indent buffer");
               end if;
            end if;
         end;

      elsif Command = "refill" then
         declare
            Child : constant MDI_Child := Find_Current_Editor (Kernel);
            Box   : Source_Box;

         begin
            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if not Get_Editable (Get_View (Box.Editor))
                 or else not Do_Refill (Get_Buffer (Box.Editor))
               then
                  Set_Error_Msg (Data, -"Could not refill buffer");
               end if;
            end if;
         end;

      elsif Command = "cut"
        or else Command = "copy"
        or else Command = "paste"
        or else Command = "select_all"
      then
         declare
            Source : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI
                         (Find_Current_Editor (Kernel));

         begin
            if Source /= null then
               if Command = "cut" then
                  Cut_Clipboard (Source);
                  External_End_Action (Get_Buffer (Source));
               elsif Command = "copy" then
                  Copy_Clipboard (Source);
               elsif Command = "paste" then
                  Paste_Clipboard (Source);
                  External_End_Action (Get_Buffer (Source));
               else
                  Select_All (Get_Buffer (Source));
               end if;
            end if;
         end;

      elsif Command = "select_text" then
         Name_Parameters (Data, Select_Text_Cmd_Parameters);

         declare
            Child        : constant MDI_Child := Find_Current_Editor (Kernel);
            Buffer       : Source_Buffer;
            First_Line   : constant Natural := Nth_Arg (Data, 1);
            Start_Column : constant Natural := Nth_Arg (Data, 3, Default => 1);
            Last_Line    : Natural := Nth_Arg (Data, 2);
            End_Column   : Natural := Nth_Arg (Data, 4, Default => 0);
         begin
            if Child /= null then
               if End_Column = 0 then
                  --  End column not specified, in this case select the
                  --  whole line
                  End_Column := 1;
                  Last_Line  := Last_Line + 1;
               end if;

               Buffer := Get_Buffer (Source_Box (Get_Widget (Child)).Editor);

               if Is_Valid_Position
                 (Buffer, Gint (First_Line - 1), Gint (Start_Column - 1))
               then
                  Select_Region
                    (Buffer,
                     Editable_Line_Type (First_Line),
                     Start_Column,
                     Editable_Line_Type (Last_Line),
                     End_Column);
               end if;
            end if;
         end;

      elsif Command = "close"
        or else Command = "undo"
        or else Command = "redo"
      then
         declare
            Filename : constant Virtual_File :=
              Create (Full_Filename => Nth_Arg (Data, 1));
         begin
            if Command = "close" then
               if Is_Absolute_Path (Filename) then
                  Close_File_Editors (Kernel, Filename);
               else
                  Close_File_Editors
                    (Kernel,
                     Create
                       (Get_Full_Path_From_File
                          (Get_Registry (Kernel).all,
                           Full_Name (Filename).all,
                           True, False)));
               end if;
            else
               declare
                  Child : MDI_Child;
                  Box   : Source_Box;
               begin
                  Child := Find_Editor (Kernel, Filename);

                  if Child = null then
                     Set_Error_Msg (Data, -"file not open");
                  else
                     Box := Source_Box (Get_Widget (Child));

                     if Command = "redo" then
                        Redo (Box.Editor);
                     elsif Command = "undo" then
                        Undo (Box.Editor);
                     end if;
                  end if;
               end;
            end if;
         end;

      elsif Command = "goto_mark" then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);

            Child       : constant MDI_Child :=
              Find_Editor (Kernel, Mark_Record.File);
         begin
            Push_Current_Editor_Location_In_History (Kernel);
            if Child /= null then
               Raise_Child (Child);
               Set_Focus_Child (Child);
               Grab_Focus (Source_Box (Get_Widget (Child)).Editor);

               --  If the Length is null, we set the length to 1, otherwise
               --  the cursor is not visible.

               Scroll_To_Mark
                 (Source_Box (Get_Widget (Child)).Editor,
                  Mark_Record.Mark,
                  Mark_Record.Length);

            else
               if Mark_Record.File /= VFS.No_File
                 and then Is_In_List
                 (Id.Unopened_Files, Full_Name (Mark_Record.File).all)
               then
                  Open_File_Editor (Kernel,
                                    Mark_Record.File,
                                    Mark_Record.Line,
                                    Mark_Record.Column,
                                    Mark_Record.Column + Mark_Record.Length);

                  --  At this point, Open_File_Editor should have caused the
                  --  propagation of the File_Edited signal, which provokes a
                  --  call to Fill_Marks in File_Edited_Cb.
                  --  Therefore the Mark_Record might not be valid beyond this
                  --  point.
               end if;
            end if;
         end;

      elsif Command = "delete_mark" then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);
            Node        : Mark_Identifier_List.List_Node;
            Prev        : Mark_Identifier_List.List_Node;
            Child       : constant MDI_Child :=
              Find_Editor (Kernel, Mark_Record.File);

            use Mark_Identifier_List;
         begin
            if Child /= null
              and then Mark_Record.Mark /= null
            then
               Delete_Mark
                 (Get_Buffer
                    (Source_Box (Get_Widget (Child)).Editor),
                  Mark_Record.Mark);
            end if;

            Node := First (Id.Stored_Marks);

            if Mark_Identifier_List.Data (Node).Id = Mark_Record.Id then
               Next (Id.Stored_Marks);
            else
               Prev := Node;
               Node := Next (Node);

               while Node /= Null_Node loop
                  if Mark_Identifier_List.Data (Node).Id
                    = Mark_Record.Id
                  then
                     Remove_Nodes (Id.Stored_Marks, Prev, Node);
                     exit;
                  end if;

                  Prev := Node;
                  Node := Next (Node);
               end loop;
            end if;
         end;

      elsif Command = "get_chars" then
         Name_Parameters (Data, Get_Chars_Args);

         declare
            File   : constant String  := Nth_Arg (Data, 1);
            Line   : constant Integer := Nth_Arg (Data, 2, 0);
            Column : constant Integer := Nth_Arg (Data, 3, 1);
            Before : constant Integer := Nth_Arg (Data, 4, Default => -1);
            After  : constant Integer := Nth_Arg (Data, 5, Default => -1);
            Child  : constant MDI_Child :=
              Find_Editor (Kernel, Create (File, Kernel));
         begin
            Set_Return_Value
              (Data,
               Get_Chars
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                  Editable_Line_Type (Line),
                  Natural (Column),
                  Before, After));
         end;

      elsif Command = "replace_text" then
         declare
            File   : constant String  := Nth_Arg (Data, 1);
            Line   : constant Integer := Nth_Arg (Data, 2);
            Column : constant Integer := Nth_Arg (Data, 3);
            Text   : constant String  := Nth_Arg (Data, 4);
            Before : constant Integer := Nth_Arg (Data, 5, Default => -1);
            After  : constant Integer := Nth_Arg (Data, 6, Default => -1);
            Editor : constant Source_Box := Open_File
              (Kernel, Create (File, Kernel), Create_New => False);
         begin
            if Editor /= null then
               if Get_Writable (Editor.Editor) then
                  Replace_Slice
                    (Get_Buffer (Editor.Editor),
                     Text,
                     Editable_Line_Type (Line), Natural (Column),
                     Before, After);
               else
                  Set_Error_Msg
                    (Data,
                     -("Attempting to edit a non-writable file: ") & File);
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;

         end;

      elsif Command = "insert_text" then
         declare
            Child  : constant MDI_Child := Find_Current_Editor (Kernel);
            Buffer : Source_Buffer;
            Text   : constant String  := Nth_Arg (Data, 1);
            Line   : Editable_Line_Type;
            Column : Positive;
         begin
            if Child /= null then
               Buffer := Get_Buffer (Source_Box (Get_Widget (Child)).Editor);

               Get_Cursor_Position (Buffer, Line, Column);
               Insert (Buffer, Line, Column, Text);
            end if;
         end;

      elsif Command = "get_line"
        or else Command = "get_column"
        or else Command = "get_file"
      then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);
            Buffer      : Source_Buffer;
            Child       : constant MDI_Child :=
              Find_Editor (Kernel, Mark_Record.File);
         begin
            if Mark_Record.File = VFS.No_File then
               Set_Error_Msg (Data, -"mark not found");
            else
               if Child /= null then
                  Buffer := Get_Buffer
                    (Source_Box (Get_Widget (Child)).Editor);
               end if;

               if Command = "get_line" then
                  if Buffer /= null then
                     Set_Return_Value
                       (Data,
                        Integer (Src_Editor_Buffer.Line_Information.Get_Line
                                   (Buffer, Mark_Record.Mark)));
                  else
                     Set_Return_Value (Data, Mark_Record.Line);
                  end if;
               elsif Command = "get_column" then
                  if Buffer /= null then
                     Set_Return_Value
                       (Data,
                        Src_Editor_Buffer.Line_Information.Get_Column
                          (Buffer, Mark_Record.Mark));
                  else
                     Set_Return_Value (Data, Mark_Record.Column);
                  end if;
               else
                  Set_Return_Value (Data, Full_Name (Mark_Record.File).all);
               end if;
            end if;
         end;

      elsif Command = "get_last_line" then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            if Child = null then
               declare
                  A : GNAT.OS_Lib.String_Access := Read_File (File);
                  N : Natural := 0;
               begin
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

                     Set_Return_Value (Data, N);
                  else
                     Set_Error_Msg (Data, -"file not found or not opened");
                  end if;
               end;
            else
               Set_Return_Value
                 (Data,
                  Get_Last_Line (Source_Box (Get_Widget (Child)).Editor));
            end if;
         end;

      elsif Command = "block_get_start"
        or else Command = "block_get_end"
        or else Command = "block_get_name"
        or else Command = "block_get_type"
        or else Command = "block_get_level"
        or else Command = "subprogram_name"
      then
         declare
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
            Line   : constant Editable_Line_Type :=
              Editable_Line_Type (Natural'(Nth_Arg (Data, 2)));

         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to get block information for non" &
                      " open file : ") & Base_Name (File));
            else
               if Command = "block_get_start" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Start
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_end" then
                  Set_Return_Value
                    (Data,
                     Get_Block_End
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_name" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Name
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_type" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Type
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               elsif Command = "block_get_level" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Level
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               else
                  --  subprogram_name
                  Set_Return_Value
                    (Data,
                     Get_Subprogram_Name
                       (Source_Box (Get_Widget (Child)).Editor, Line));
               end if;
            end if;
         end;

      elsif Command = "cursor_get_line"
        or else Command = "cursor_get_column"
      then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to get cursor position for non open file: ")
                  & Base_Name (File));
            else
               declare
                  Line   : Editable_Line_Type;
                  Column : Positive;
               begin
                  Get_Cursor_Position
                    (Get_Buffer
                       (Source_Box (Get_Widget (Child)).Editor), Line, Column);

                  if Command = "cursor_get_line" then
                     Set_Return_Value (Data, Integer (Line));
                  else
                     Set_Return_Value (Data, Column);
                  end if;
               end;
            end if;
         end;

      elsif Command = "cursor_set_position" then
         declare
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
            Line   : constant Editable_Line_Type :=
              Editable_Line_Type (Integer'(Nth_Arg (Data, 2)));
            Column : Natural := Nth_Arg (Data, 3, Default => 0);
         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to set cursor position for non open file: ")
                  & Base_Name (File));
            else
               if Column = 0 then
                  --  Column has not been specified, set it to the first non
                  --  white space character.
                  --  Do we really always want this behavior ???

                  declare
                     Chars : constant String :=
                       Get_Chars
                         (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                          Line);
                  begin
                     --  Set the column to 1, if line is empty we want to set
                     --  the cursor on the first column.

                     Column := 1;

                     for K in Chars'Range loop
                        Column := K;
                        exit when Chars (K) /= ' '
                          and then Chars (K) /= ASCII.HT;
                     end loop;

                     if Column /= 1 then
                        --  Adjust column number.
                        Column := Column - Chars'First + 1;
                     end if;
                  end;
               end if;

               Set_Cursor_Position
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                  Line, Column);
            end if;
         end;

      elsif Command = "cursor_center" then
         declare
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            Scroll_To_Cursor_Location
              (Get_View (Source_Box (Get_Widget (Child)).Editor),
               Center => True);
         end;

      elsif Command = "get_buffer" then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
            A     : GNAT.OS_Lib.String_Access;

         begin
            if Child /= null then
               A := Src_Editor_Buffer.Get_String
                 (Get_Buffer (Source_Box (Get_Widget (Child)).Editor));

               Set_Return_Value (Data, A.all);

               Free (A);
            else
               --  The buffer is not currently open, read directly from disk.

               A := Read_File (File);

               if A /= null then
                  declare
                     Length        : constant Integer := A'Length;
                     Result_String : String (1 .. Length * 2 + 1);
                     Ignore, Bytes : Natural;
                  begin
                     Glib.Convert.Convert
                       (A.all,
                        "UTF-8", Get_Pref (Kernel, Default_Charset),
                        Ignore, Bytes, Result => Result_String);
                     Set_Return_Value (Data, Result_String (1 .. Bytes));
                  end;

                  Free (A);
               else
                  Set_Error_Msg (Data, -"file not found");
               end if;
            end if;
         end;

      elsif Command = "save_buffer" then
         declare
            File    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child   : constant MDI_Child := Find_Editor (Kernel, File);
            To_File : Virtual_File := VFS.No_File;
            Result  : Boolean;
         begin
            if Number_Of_Arguments (Data) >= 2 then
               To_File := Create (Nth_Arg (Data, 2), Kernel);
            end if;

            if Child /= null then
               if To_File /= VFS.No_File then
                  Save_To_File
                    (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                     To_File,
                     Result,
                     True);

               else
                  Save_To_File
                    (Get_Buffer (Source_Box (Get_Widget (Child)).Editor),
                     File,
                     Result,
                     False);
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;
         end;

      elsif Command = "save" then
         Name_Parameters (Data, Save_Cmd_Parameters);
         declare
            Interactive : constant Boolean :=
              Nth_Arg (Data, 1, Default => True);
            All_Save : constant Boolean := Nth_Arg (Data, 2, Default => True);
            Child    : MDI_Child;
         begin
            if All_Save then
               if not Save_MDI_Children (Kernel, Force => not Interactive) then
                  Set_Error_Msg (Data, -"cancelled");
               end if;
            else
               Child := Find_Current_Editor (Kernel);
               if Child = null then
                  Set_Error_Msg (Data, -"no file selected");
               elsif not Save_MDI_Children
                 (Kernel, Children => (1 => Child), Force => not Interactive)
               then
                  Set_Error_Msg (Data, -"cancelled");
               end if;
            end if;
         end;

      elsif Command = "add_blank_lines" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Line        : constant Integer := Nth_Arg (Data, 2);
            Number      : constant Integer := Nth_Arg (Data, 3);
            Child       : MDI_Child;
            Box         : Source_Box;
            Mark_Record : Mark_Identifier_Record;
            Highlight_Category : Natural := 0;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Number_Of_Arguments (Data) >= 4 then
               Highlight_Category :=
                 Line_Highlighting.Lookup_Category (Nth_Arg (Data, 4));
            end if;

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if Line >= 0 and then Number > 0 then
                  --  Create a new mark record and insert it in the list.
                  Mark_Record.Line := 0;
                  Mark_Record.File := Filename;
                  Mark_Record.Id := Id.Next_Mark_Id;

                  Id.Next_Mark_Id := Id.Next_Mark_Id + 1;
                  Mark_Record.Length := 0;
                  Mark_Record.Mark :=
                    Add_Blank_Lines
                      (Get_Buffer (Box.Editor),
                       Editable_Line_Type (Line),
                       Highlight_Category, "", Number);
                  Mark_Identifier_List.Append (Id.Stored_Marks, Mark_Record);
                  Set_Return_Value (Data, Image (Mark_Record.Id));
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;
         end;

      elsif Command = "remove_blank_lines" then
         declare
            Identifier  : constant String := Nth_Arg (Data, 1);
            Mark_Record : constant Mark_Identifier_Record :=
              Find_Mark (Identifier);
            Child       : MDI_Child;
            Number      : Integer := 0;
            Box         : Source_Box;
         begin
            Child := Find_Editor (Kernel, Mark_Record.File);

            if Number_Of_Arguments (Data) >= 3 then
               Number := Nth_Arg (Data, 2);
            end if;

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               Src_Editor_Buffer.Line_Information.Remove_Blank_Lines
                 (Get_Buffer (Box.Editor), Mark_Record.Mark, Number);
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "block_fold" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Line        : constant Integer := Nth_Arg (Data, 2, 0);
            Child       : MDI_Child;
            Box         : Source_Box;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if Line = 0 then
                  Src_Editor_Buffer.Line_Information.Fold_All
                    (Get_Buffer (Box.Editor));
               else
                  Src_Editor_Buffer.Line_Information.Fold_Block
                    (Get_Buffer (Box.Editor), Editable_Line_Type (Line));
               end if;
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "block_unfold" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Line        : constant Integer := Nth_Arg (Data, 2, 0);
            Child       : MDI_Child;
            Box         : Source_Box;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));

               if Line = 0 then
                  Src_Editor_Buffer.Line_Information.Fold_All
                    (Get_Buffer (Box.Editor));
               else
                  Src_Editor_Buffer.Line_Information.Unfold_Line
                    (Get_Buffer (Box.Editor), Editable_Line_Type (Line));
               end if;
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "set_background_color" then
         declare
            Filename    : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Color       : constant String := Nth_Arg (Data, 2);
            Box         : Source_Box;
            Child       : MDI_Child;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               Modify_Base
                 (Get_View (Box.Editor), State_Normal, Parse (Color));
            end if;
         end;

      elsif Command = "set_synchronized_scrolling" then
         declare
            Filename_1 : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Filename_2 : constant Virtual_File :=
              Create (Nth_Arg (Data, 2), Kernel);
            Child_1    : MDI_Child;
            Child_2    : MDI_Child;
            use Child_Triplet_Callback;
            Triplet    : Child_Triplet_Access;
         begin
            Child_1 := Find_Editor (Kernel, Filename_1);
            Child_2 := Find_Editor (Kernel, Filename_2);

            if Child_1 /= null and then Child_2 /= null then
               Triplet := new Child_Triplet'(Child_1, Child_2, null);

               Set_Synchronized_Editor
                 (Get_View (Source_Box (Get_Widget (Child_1)).Editor),
                  Get_View (Source_Box (Get_Widget (Child_2)).Editor));

               if Number_Of_Arguments (Data) > 2 then
                  declare
                     Filename_3 : constant Virtual_File :=
                       Create (Nth_Arg (Data, 3), Kernel);
                     Child_3 : constant MDI_Child :=
                       Find_Editor (Kernel, Filename_3);
                  begin
                     if Child_3 /= null then
                        Set_Synchronized_Editor
                          (Get_View
                             (Source_Box (Get_Widget (Child_2)).Editor),
                           Get_View
                             (Source_Box (Get_Widget (Child_3)).Editor));

                        Set_Synchronized_Editor
                          (Get_View
                             (Source_Box (Get_Widget (Child_3)).Editor),
                           Get_View
                             (Source_Box (Get_Widget (Child_1)).Editor));
                     end if;

                     Triplet (3) := Child_3;
                  end;

               else
                  Set_Synchronized_Editor
                    (Get_View (Source_Box (Get_Widget (Child_2)).Editor),
                     Get_View (Source_Box (Get_Widget (Child_1)).Editor));
               end if;

               for C in Triplet'Range loop
                  if Triplet (C) /= null then
                     Connect
                       (Triplet (C), "grab_focus",
                        Marshallers.Void_Marshaller.To_Marshaller
                          (On_Raise_Child'Access),
                        User_Data => Triplet);
                     Connect
                       (Triplet (C), "destroy",
                        Marshallers.Void_Marshaller.To_Marshaller
                          (On_Delete_Child'Access),
                        User_Data => Triplet);
                  end if;
               end loop;
            end if;
         end;

      elsif Command = "add_case_exception"
        or else Command = "remove_case_exception"
      then
         Name_Parameters (Data, Case_Exception_Cmd_Parameters);

         declare
            Name : constant String := Nth_Arg (Data, 1);
         begin
            if Command = "add_case_exception" then
               Add_Exception (Name);
            else
               Remove_Exception (Name);
            end if;
         end;

      elsif Command = "set_writable" then
         Name_Parameters (Data, Set_Writable_Cmd_Parameters);

         declare
            Filename : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Write    : constant Boolean := Nth_Arg (Data, 2);
            Child    : MDI_Child;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Set_Writable (Source_Box (Get_Widget (Child)).Editor, Write);
            end if;
         end;
      end if;
   end Edit_Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Editor_Class       : constant Class_Type := New_Class (Kernel, "Editor");
   begin
      Register_Command
        (Kernel, "edit",
         Class         => Editor_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 6,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "create_mark",
         Minimum_Args  => 1,
         Maximum_Args  => 4,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "highlight",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "add_blank_lines",
         Minimum_Args  => 3,
         Maximum_Args  => 4,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_blank_lines",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_fold",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_unfold",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "unhighlight",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "highlight_range",
         Minimum_Args  => 2,
         Maximum_Args  => 5,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "unhighlight_range",
         Minimum_Args  => 2,
         Maximum_Args  => 5,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "register_highlighting",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Line_Highlighting.Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "set_background_color",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "goto_mark",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "delete_mark",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_chars",
         Minimum_Args  => 1,
         Maximum_Args  => 5,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_line",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_column",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_last_line",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_start",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_end",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_name",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_type",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "block_get_level",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "subprogram_name",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_get_line",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_get_column",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_set_position",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cursor_center",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "get_buffer",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "save_buffer",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "replace_text",
         Minimum_Args  => 4,
         Maximum_Args  => 6,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "indent",
         Minimum_Args  => Indent_Cmd_Parameters'Length - 1,
         Maximum_Args  => Indent_Cmd_Parameters'Length,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "indent_buffer",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "refill",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "cut",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "copy",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "paste",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "select_all",
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "select_text",
         Minimum_Args  => 2,
         Maximum_Args  => 4,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "insert_text",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "undo",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "redo",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "close",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "save",
         Maximum_Args  => Save_Cmd_Parameters'Length,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "search",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_File_Class (Kernel),
         Handler      => File_Search_Command_Handler'Access);
      Register_Command
        (Kernel, "search_next",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_File_Class (Kernel),
         Handler      => Current_Search_Command_Handler'Access);

      Register_Command
        (Kernel, "search",
         Minimum_Args => 1,
         Maximum_Args => 5,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Search_Command_Handler'Access);

      Register_Command
        (Kernel, "set_synchronized_scrolling",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "add_case_exception",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_case_exception",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);

      Register_Command
        (Kernel, "set_writable",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
   end Register_Commands;

end Src_Editor_Module.Shell;
