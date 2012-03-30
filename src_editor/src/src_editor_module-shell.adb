------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Tags;                  use Ada.Tags;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;

with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Unicode;              use Glib.Unicode;

with Gdk.Color;                 use Gdk.Color;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_Tag;              use Gtk.Text_Tag;

with Casing_Exceptions;         use Casing_Exceptions;
with Commands;                  use Commands;
with Find_Utils;                use Find_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Charsets;       use GPS.Kernel.Charsets;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Styles;         use GPS.Kernel.Styles;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Language;                  use Language;
with Projects;                  use Projects;
with Src_Contexts;              use Src_Contexts;
with Src_Editor_Box;            use Src_Editor_Box;

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Text_Handling;
use Src_Editor_Buffer.Text_Handling;

with Src_Editor_Buffer.Blocks; use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Debug;

with Src_Editor_Module.Editors; use Src_Editor_Module.Editors;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;
with Src_Editor_View;           use Src_Editor_View;
with Traces;                    use Traces;

with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body Src_Editor_Module.Shell is
   Me : constant Debug_Handle := Create ("Editor.Shell");

   Editor_Location_Class_Name : constant String := "EditorLocation";

   Filename_Cst          : aliased constant String := "filename";
   File_Cst              : aliased constant String := "file";
   Line_Cst              : aliased constant String := "line";
   Col_Cst               : aliased constant String := "column";
   Buffer_Cst            : aliased constant String := "buffer";
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
   Start_Cst             : aliased constant String := "start";
   End_Cst               : aliased constant String := "end";
   Count_Cst             : aliased constant String := "count";
   Location_Cst          : aliased constant String := "location";
   From_Cst              : aliased constant String := "frm";
   To_Cst                : aliased constant String := "to";
   Append_Cst            : aliased constant String := "append";
   Text_Cst              : aliased constant String := "text";
   Read_Only_Cst         : aliased constant String := "read_only";
   Value_Cst             : aliased constant String := "value";
   Overlay_Cst           : aliased constant String := "overlay";
   Backward_Cst          : aliased constant String := "backward";
   Whole_Word_Cst        : aliased constant String := "whole_word";
   Dialog_On_Failure_Cst : aliased constant String := "dialog_on_failure";
   Open_Cst              : aliased constant String := "open";
   Title_Cst             : aliased constant String := "title";
   Short_Cst             : aliased constant String := "short";

   Action_Cst            : aliased constant String := "action";
   Secondary_Action_Cst  : aliased constant String := "secondary_action";
   Index_Cst             : aliased constant String := "index";

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
   Set_Title_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => File_Cst'Access,
      2 => Title_Cst'Access,
      3 => Filename_Cst'Access);

   Highlighter_Constructor_Args : constant Cst_Argument_List :=
     (Pattern_Cst'Access,
      Action_Cst'Access,
      Index_Cst'Access,
      Secondary_Action_Cst'Access);

   type Highlighter_Property is new Instance_Property_Record with record
      Highlighter : Highlighter_Record;
   end record;
   type Highlighter_Property_Access is access all Highlighter_Property;

   type Child_Triplet is array (1 .. 3) of Gtkada.MDI.MDI_Child;
   type Child_Triplet_Access is access Child_Triplet;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Child_Triplet, Child_Triplet_Access);

   package Child_Triplet_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Child_Triplet_Access);

   procedure Edit_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the source editor module

   procedure Current_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure File_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Project_Search_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Common_Search_Command_Handler
     (Data  : in out Callback_Data'Class;
      Files : GNATCOLL.VFS.File_Array_Access);
   --  Interactive command handler for the source editor module (Search part)

   procedure On_Raise_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access);
   --  Called when synchronized editor Child in Triplet is raised

   procedure On_Delete_Child
     (Child   : access Gtk_Widget_Record'Class;
      Triplet : Child_Triplet_Access);
   --  Called when synchronized editor Child in Triplet is deleted

   procedure Buffer_Cmds (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the EditorBuffer class

   procedure Location_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the EditorLocation class

   procedure Mark_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for EditorMark class

   procedure View_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for EditorView class

   procedure Overlay_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for EditorOverlay class

   function Create_Editor_Overlay
     (Script  : access Scripting_Language_Record'Class;
      Overlay : Editor_Overlay'Class) return Class_Instance;
   --  Manipulation of instances of the EditorOverlay class.
   --  Result must be freed unless you assign it to a Callback_Data

   function Create_Editor_Buffer
     (Script : access Scripting_Language_Record'Class;
      Buffer : Editor_Buffer'Class) return Class_Instance;
   --  Manipulation of instances of the EditorBuffer class.
   --  Result of Create_Editor_Buffer must be freed unless you assign it to
   --  a Callback_Data

   function Create_Editor_View
     (Script : access Scripting_Language_Record'Class;
      View   : Editor_View'Class) return Class_Instance;
   --  Return an instance of EditorView encapsulating View. Result must be
   --  freed unless you assign it to a Callback_Data.

   function Create_Editor_Location
     (Script   : access Scripting_Language_Record'Class;
      Location : Editor_Location'Class) return Class_Instance;
   --  Return an instance of EditorLocation

   function Get_Location
     (Data    : Callback_Data'Class;
      Arg     : Positive;
      Default : Editor_Location'Class := Nil_Editor_Location)
      return Editor_Location'Class;
   --  Return the iter stored in the Arg-th parameter.
   --  If no location could be obtain from the arguments, returns Default

   function Get_Mark
     (Data : Callback_Data'Class;
      Arg  : Positive) return Editor_Mark'Class;
   --  Return the mark stored in the Arg-th parameter.
   --  Set Mark to null if it wasn't a valid instance

   function Get_Buffer
     (Data   : Callback_Data'Class;
      Arg    : Positive) return GPS_Editor_Buffer'Class;
   --  Set the Buffer variable appropriately, or null if the buffer could
   --  not be found or is no longer valid.
   --  If the buffer is no longer valid, raises Editor_Exception

   function Get_Overlay
     (Data       : Callback_Data'Class;
      Arg        : Positive;
      Allow_Null : Boolean := False) return Editor_Overlay'Class;
   --  Get the EditorOverlay stored in Data

   function Get_View
     (Data   : Callback_Data'Class;
      Arg    : Positive) return Editor_View'Class;
   --  Return the view stored in Data

   procedure Hyper_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands related to hyper mode

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Data   : Callback_Data'Class;
      Arg    : Positive) return Editor_View'Class
   is
      EditorView : constant Class_Type :=
                     New_Class (Get_Kernel (Data), "EditorView");
      Inst       : constant Class_Instance := Nth_Arg (Data, Arg, EditorView);
   begin
      return View_From_Instance
        (Src_Editor_Buffer_Factory
           (Get_Buffer_Factory (Get_Kernel (Data)).all),
         Instance   => Inst);
   end Get_View;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Data   : Callback_Data'Class;
      Arg    : Positive) return GPS_Editor_Buffer'Class
   is
      EditorBuffer : constant Class_Type :=
                       New_Class (Get_Kernel (Data), "EditorBuffer");
      Inst    : constant Class_Instance := Nth_Arg (Data, Arg, EditorBuffer);
   begin
      return GPS_Editor_Buffer'Class
        (Buffer_From_Instance
           (Src_Editor_Buffer_Factory
              (Get_Buffer_Factory (Get_Kernel (Data)).all), Inst));
   end Get_Buffer;

   -----------------
   -- Get_Overlay --
   -----------------

   function Get_Overlay
     (Data       : Callback_Data'Class;
      Arg        : Positive;
      Allow_Null : Boolean := False) return Editor_Overlay'Class
   is
      EditorOverlay : constant Class_Type :=
                        New_Class (Get_Kernel (Data), "EditorOverlay");
      Inst          : constant Class_Instance :=
        Nth_Arg (Data, Arg, EditorOverlay, Allow_Null => Allow_Null);
   begin
      if Inst = No_Class_Instance then
         return Nil_Editor_Overlay;
      else
         return Overlay_From_Instance (Inst);
      end if;
   end Get_Overlay;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Data    : Callback_Data'Class;
      Arg     : Positive;
      Default : Editor_Location'Class := Nil_Editor_Location)
      return Editor_Location'Class
   is
      Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), Editor_Location_Class_Name);
      Inst  : constant Class_Instance := Nth_Arg
        (Data, Arg, Class, Allow_Null => True, Default => No_Class_Instance);
   begin
      if Inst = No_Class_Instance then
         return Default;
      else
         declare
            Loc : constant Editor_Location_Access := Get_Data
              (Inst, Class_Name => Editor_Location_Class_Name);
         begin
            if Loc /= null then
               return Loc.all;
            else
               return Default;
            end if;
         end;
      end if;
   end Get_Location;

   ---------------------------
   -- Create_Editor_Overlay --
   ---------------------------

   function Create_Editor_Overlay
     (Script  : access Scripting_Language_Record'Class;
      Overlay : Editor_Overlay'Class) return Class_Instance is
   begin
      return Instance_From_Overlay
        (Script, New_Class (Get_Kernel (Script), "EditorOverlay"), Overlay);
   end Create_Editor_Overlay;

   ------------------------
   -- Create_Editor_Mark --
   ------------------------

   function Create_Editor_Mark
     (Script : access Scripting_Language_Record'Class;
      Mark   : Editor_Mark'Class) return Class_Instance is
   begin
      return Instance_From_Mark
        (Script, New_Class (Get_Kernel (Script), "EditorMark"), Mark);
   end Create_Editor_Mark;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark
     (Data : Callback_Data'Class;
      Arg  : Positive) return Editor_Mark'Class
   is
      EditorMark : constant Class_Type :=
                     New_Class (Get_Kernel (Data), "EditorMark");
      Inst : constant Class_Instance := Nth_Arg (Data, Arg, EditorMark);
   begin
      return Mark_From_Instance
        (Src_Editor_Buffer_Factory
           (Get_Buffer_Factory (Get_Kernel (Data)).all), Inst);
   end Get_Mark;

   ----------------------------
   -- Create_Editor_Location --
   ----------------------------

   function Create_Editor_Location
     (Script   : access Scripting_Language_Record'Class;
      Location : Editor_Location'Class) return Class_Instance
   is
      EditorLoc : constant Class_Type :=
                    New_Class
                      (Get_Kernel (Script), Editor_Location_Class_Name);
      Inst      : constant Class_Instance := New_Instance (Script, EditorLoc);
   begin
      Set_Data (Inst, Editor_Location_Class_Name, Location);
      return Inst;
   end Create_Editor_Location;

   --------------------------
   -- Create_Editor_Buffer --
   --------------------------

   function Create_Editor_Buffer
     (Script : access Scripting_Language_Record'Class;
      Buffer : Editor_Buffer'Class) return Class_Instance is
   begin
      return Instance_From_Buffer
        (Script, New_Class (Get_Kernel (Script), "EditorBuffer"), Buffer);
   end Create_Editor_Buffer;

   ------------------------
   -- Create_Editor_View --
   ------------------------

   function Create_Editor_View
     (Script : access Scripting_Language_Record'Class;
      View   : Editor_View'Class) return Class_Instance is
   begin
      return Instance_From_View
        (Script, New_Class (Get_Kernel (Script), "EditorView"), View);
   end Create_Editor_View;

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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Raise_Child;

   -----------------------------------
   -- Common_Search_Command_Handler --
   -----------------------------------

   procedure Common_Search_Command_Handler
     (Data  : in out Callback_Data'Class;
      Files : File_Array_Access)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Pattern : constant String  := Nth_Arg (Data, 2);
      Casing  : constant Boolean := Nth_Arg (Data, 3, False);
      Regexp  : constant Boolean := Nth_Arg (Data, 4, False);
      Scope   : constant String  := Nth_Arg (Data, 5, "whole");
      Context : Files_Project_Context_Access;
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
               Match.Begin_Line,
               Match.Visible_Begin_Column));
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
         --  No need to delay, since the search is done in same process
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
      File    : constant Virtual_File := Get_Data (Inst);
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
               Match.Begin_Line,
               Match.Visible_Begin_Column));
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
      Info   : constant Virtual_File := Get_Data (Inst);
   begin
      Name_Parameters (Data, File_Search_Parameters);
      Common_Search_Command_Handler (Data, new File_Array'(1 => Info));
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
        (Data, Project.Source_Files (Recursive));
   end Project_Search_Command_Handler;

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   procedure Edit_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Marker_Record'Class, File_Marker);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Length : Natural := 0;
      Line   : Natural := 1;
      Column : Visible_Column_Type := 1;
      Force  : Boolean;
      Marker : File_Marker;

   begin
      if Command = "edit" or else Command = "create_mark" then
         if Command = "edit" then
            Name_Parameters (Data, Edit_Cmd_Parameters);
         else
            Name_Parameters (Data, Create_Mark_Parameters);
         end if;

         declare
            File     : constant Virtual_File :=
                         Create (Nth_Arg (Data, 1),
                                 Kernel, Use_Source_Path => True);
            Position : Natural;
         begin
            Line   := Nth_Arg (Data, 2, Default => 1);
            Column := Visible_Column_Type (Nth_Arg (Data, 3, Default => 1));
            Length := Nth_Arg (Data, 4, Default => 0);

            if File /= GNATCOLL.VFS.No_File then
               if Command = "edit" then
                  Force := Nth_Arg (Data, 5, Default => False);
                  Position := Nth_Arg
                    (Data, 6,
                     Default => Child_Position'Pos (Position_Automatic));

                  if Length = 0 then
                     Open_File_Editor
                       (Kernel,
                        File,
                        Line,
                        Column,
                        Enable_Navigation => False,
                        Force_Reload => Force,
                        Initial_Position => Child_Position'Val (Position));
                  else
                     Open_File_Editor
                       (Kernel,
                        File,
                        Line,
                        Column,
                        Column + Visible_Column_Type (Length),
                        Enable_Navigation => False,
                        Force_Reload => Force);
                  end if;

               elsif Command = "create_mark" then
                  --  ??? Wrong conversion here?
                  Marker := Create_File_Marker
                    (Kernel,
                     File,
                     Editable_Line_Type (Line),
                     Column, Length);
                  Set_Return_Value (Data, Get_Id (Marker));
               end if;
            end if;
         end;

      elsif Command = "print_line_info" then
         declare
            File  : constant Virtual_File :=
                      Create
                        (Nth_Arg (Data, 1), Kernel, Use_Source_Path => True);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
            Info  : Line_Info_Width_Array_Access;
            Box   : Source_Editor_Box;

            procedure Print_Line_Info (Info : Line_Information_Record);

            ---------------------
            -- Print_Line_Info --
            ---------------------

            procedure Print_Line_Info (Info : Line_Information_Record) is
               Print : Unbounded_String;
            begin
               if Info.Text = null then
                  Print := Print & "[no text], ";
               else
                  Print := Print & """" & Info.Text.all & """, ";
               end if;

               if Info.Tooltip_Text = null then
                  Print := Print & "[no tooltip], ";
               else
                  Print := Print & """" & Info.Tooltip_Text.all & """, ";
               end if;

               if Info.Associated_Command = null then
                  Print := Print & "[no command]";
               else
                  Print := Print & """"
                    & External_Tag (Info.Associated_Command.all'Tag) & """";
               end if;

               Set_Return_Value (Data, To_String (Print));
            end Print_Line_Info;

         begin
            if Child /= null then
               Box  := Source_Editor_Box (Get_Widget (Child));
               Line := Nth_Arg (Data, 2, Default => 1);
               Info := Get_Side_Information (Get_Buffer (Box),
                                             Editable_Line_Type (Line));
               Set_Return_Value_As_List (Data);

               for J in Info'Range loop
                  if not Info (J).Messages.Is_Empty
                    and then Info (J).Messages.First_Element.Get_Action /= null
                  then
                     Print_Line_Info
                       (Info (J).Messages.First_Element.Get_Action.all);
                  end if;

                  if Info (J).Action /= null then
                     Print_Line_Info (Info (J).Action.all);
                  end if;
               end loop;

            end if;
         end;

      elsif Command = "indent" then
         Name_Parameters (Data, Indent_Cmd_Parameters);
         declare
            Current_Line_Only : constant Boolean := Nth_Arg (Data, 1, False);
            Child             : constant MDI_Child :=
                                  Find_Current_Editor (Kernel);
            Box               : Source_Editor_Box;
         begin
            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));

               if not Get_Editable (Get_View (Box))
                 or else not Do_Indentation
                   (Get_Buffer (Box), Current_Line_Only)
               then
                  Set_Error_Msg (Data, -"Could not indent selection");
               end if;
            end if;
         end;

      elsif Command = "refill" then
         declare
            Child : constant MDI_Child := Find_Current_Editor (Kernel);
            Box   : Source_Editor_Box;

         begin
            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));

               if not Get_Editable (Get_View (Box))
                 or else not Do_Refill (Get_Buffer (Box))
               then
                  Set_Error_Msg (Data, -"Could not refill buffer");
               end if;
            end if;
         end;

      elsif Command = "indent_buffer" then
         declare
            Child    : constant MDI_Child := Find_Current_Editor (Kernel);
            Box      : Source_Editor_Box;
            Buffer   : Source_Buffer;
            From, To : Gtk_Text_Iter;

         begin
            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));
               Buffer := Get_Buffer (Box);

               Get_Start_Iter (Buffer, From);
               Get_End_Iter (Buffer, To);

               if not Get_Editable (Get_View (Box))
                 or else not Do_Indentation
                   (Get_Buffer (Box), From, To)
               then
                  Set_Error_Msg (Data, -"Could not indent buffer");
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
                  Cut_Clipboard (Get_Clipboard (Kernel), Get_View (Source));
                  External_End_Action (Get_Buffer (Source));
               elsif Command = "copy" then
                  Copy_Clipboard (Get_Clipboard (Kernel), Get_View (Source));
               elsif Command = "paste" then
                  Paste_Clipboard (Get_Clipboard (Kernel), Get_View (Source));
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

               Buffer := Get_Buffer (Source_Editor_Box (Get_Widget (Child)));

               if Is_Valid_Position
                 (Buffer, Gint (First_Line - 1), Gint (Start_Column - 1))
               then
                  Select_Region
                    (Buffer,
                     Editable_Line_Type (First_Line),
                     Visible_Column_Type (Start_Column),
                     Editable_Line_Type (Last_Line),
                     Visible_Column_Type (End_Column));
               end if;
            end if;
         end;

      elsif Command = "close"
        or else Command = "undo"
        or else Command = "redo"
      then
         declare
            Filename : Virtual_File :=
              Create (Full_Filename => Nth_Arg (Data, 1));
         begin
            if Command = "close" then
               if not Is_Absolute_Path (Filename) then
                  Filename := Get_Registry (Kernel).Tree.Create
                    (Full_Name (Filename),
                     Use_Object_Path => False);
               end if;

               Close_File_Editors (Kernel, Filename);
            else
               declare
                  Child : MDI_Child;
                  Box   : Source_Editor_Box;
               begin
                  Child := Find_Editor (Kernel, Filename);

                  if Child = null then
                     Set_Error_Msg (Data, -"file not open");
                  else
                     Box := Source_Editor_Box (Get_Widget (Child));

                     if Command = "redo" then
                        Redo (Box);
                     elsif Command = "undo" then
                        Undo (Box);
                     end if;
                  end if;
               end;
            end if;
         end;

      elsif Command = "goto_mark" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         Push_Current_Editor_Location_In_History (Kernel);
         Force := Go_To (Marker, Kernel);

      elsif Command = "mark_current_location" then
         Push_Current_Editor_Location_In_History (Kernel);

      elsif Command = "delete_mark" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         if Marker /= null then
            Destroy (Marker.all);
            Unchecked_Free (Marker);
         end if;

      elsif Command = "get_chars" then
         Name_Parameters (Data, Get_Chars_Args);

         declare
            File   : constant Filesystem_String  := Nth_Arg (Data, 1);
            Line   : constant Integer := Nth_Arg (Data, 2, 0);
            Column : constant Integer := Nth_Arg (Data, 3, 1);
            Before : constant Integer := Nth_Arg (Data, 4, Default => -1);
            After  : constant Integer := Nth_Arg (Data, 5, Default => -1);
            Child  : constant MDI_Child :=
              Find_Editor (Kernel, Create (File, Kernel));

            Real_Col : Character_Offset_Type;
         begin
            Real_Col := Collapse_Tabs
              (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
               Editable_Line_Type (Line),
               Visible_Column_Type (Column));
            Set_Return_Value
              (Data,
               Get_Chars
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
                  Editable_Line_Type (Line),
                  Real_Col,
                  Before, After));
         end;

      elsif Command = "replace_text" then
         declare
            File   : constant Filesystem_String  := Nth_Arg (Data, 1);
            Line   : constant Integer := Nth_Arg (Data, 2);
            Column : constant Integer := Nth_Arg (Data, 3);
            Text   : constant String  := Nth_Arg (Data, 4);
            Before : constant Integer := Nth_Arg (Data, 5, Default => -1);
            After  : constant Integer := Nth_Arg (Data, 6, Default => -1);
            Editor : constant Source_Editor_Box := Open_File
              (Kernel, Create (File, Kernel), Create_New => False,
               Line => 0, Column => 0, Column_End => 0);

            Real_Col : Character_Offset_Type;
         begin
            if Editor /= null then
               if Get_Writable (Get_Buffer (Editor)) then
                  Real_Col := Collapse_Tabs
                    (Get_Buffer (Editor),
                     Editable_Line_Type (Line),
                     Visible_Column_Type (Column));

                  Replace_Slice
                    (Get_Buffer (Editor),
                     Text,
                     Editable_Line_Type (Line), Real_Col,
                     Before, After);
               else
                  Set_Error_Msg
                    (Data,
                     -("Attempting to edit a non-writable file: ") & (+File));
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
            Column : Character_Offset_Type;
         begin
            if Child /= null then
               Buffer := Get_Buffer (Source_Editor_Box (Get_Widget (Child)));

               Get_Cursor_Position (Buffer, Line, Column);
               Insert (Buffer, Line, Column, Text);
            end if;
         end;

      elsif Command = "get_line" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         Set_Return_Value (Data, Natural (Get_Line (Marker)));

      elsif Command = "get_column" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         Set_Return_Value (Data, Natural (Get_Column (Marker)));

      elsif Command = "get_file" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         Set_Return_Value (Data, Full_Name (Get_File (Marker)));

      elsif Command = "get_last_line" then
         declare
            File  : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            if Child = null then
               declare
                  A : GNAT.Strings.String_Access := Read_File (File);
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
                  Get_Last_Line (Source_Editor_Box (Get_Widget (Child))));
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
                      " open file : ") & (+Base_Name (File)));
            else
               if Command = "block_get_start" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Start
                       (Source_Editor_Box (Get_Widget (Child)), Line));
               elsif Command = "block_get_end" then
                  Set_Return_Value
                    (Data,
                     Get_Block_End
                       (Source_Editor_Box (Get_Widget (Child)), Line));
               elsif Command = "block_get_name" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Name
                       (Source_Editor_Box (Get_Widget (Child)), Line));
               elsif Command = "block_get_type" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Type
                       (Source_Editor_Box (Get_Widget (Child)), Line));
               elsif Command = "block_get_level" then
                  Set_Return_Value
                    (Data,
                     Get_Block_Level
                       (Source_Editor_Box (Get_Widget (Child)), Line));
               else
                  --  Subprogram_name
                  Set_Return_Value
                    (Data,
                     Get_Subprogram_Name
                       (Source_Editor_Box (Get_Widget (Child)), Line));
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
                  & (+Base_Name (File)));
            else
               declare
                  Line   : Editable_Line_Type;
                  Column : Visible_Column_Type;
               begin
                  Get_Cursor_Position
                    (Get_Buffer
                       (Source_Editor_Box (Get_Widget (Child))), Line, Column);

                  if Command = "cursor_get_line" then
                     Set_Return_Value (Data, Integer (Line));
                  else
                     Set_Return_Value (Data, Integer (Column));
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
            Column : Visible_Column_Type :=
                       Visible_Column_Type (Nth_Arg (Data, 3, Default => 0));
            Real_Col : Character_Offset_Type;
         begin
            if Child = null then
               Set_Error_Msg
                 (Data,
                    -("Attempting to set cursor position for non open file: ")
                  & (+Base_Name (File)));
            else
               if Column = 0 then
                  --  Column has not been specified, set it to the first non
                  --  white space character.
                  --  Do we really always want this behavior ???

                  declare
                     Chars : constant String :=
                       Get_Chars
                         (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
                          Line);
                  begin
                     --  Set the column to 1, if line is empty we want to set
                     --  the cursor on the first column.

                     Column := 1;

                     for K in Chars'Range loop
                        Column := Visible_Column_Type (K);
                        exit when Chars (K) /= ' '
                          and then Chars (K) /= ASCII.HT;
                     end loop;

                     if Column /= 1 then
                        --  Adjust column number
                        Column := Column - Visible_Column_Type
                          (Chars'First) + 1;
                     end if;
                  end;
               end if;

               Real_Col := Collapse_Tabs
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
                  Line,
                  Column);

               Set_Cursor_Position
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
                  Line, Real_Col, Internal => False);
            end if;
         end;

      elsif Command = "cursor_center" then
         declare
            File  : constant Virtual_File :=
                      Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
         begin
            Scroll_To_Cursor_Location
              (Get_View (Source_Editor_Box (Get_Widget (Child))),
               Centering => Center);
         end;

      elsif Command = "get_buffer" then
         declare
            File  : constant Virtual_File :=
                      Create (Nth_Arg (Data, 1), Kernel);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
            A     : GNAT.Strings.String_Access;
            B     : GNAT.Strings.String_Access;

         begin
            if Child /= null then
               A := Src_Editor_Buffer.Get_String
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Child))));

               Set_Return_Value (Data, A.all);

               Free (A);
            else
               --  The buffer is not currently open, read directly from disk

               B := Read_File (File);

               if B /= null then
                  declare
                     Length        : constant Integer := B'Length;
                     Result_String : String (1 .. Length * 2 + 1);
                     Ignore, Bytes : Natural;
                     To_Charset    : constant String :=
                       Get_File_Charset (File);
                  begin
                     if To_Charset = "UTF-8" then
                        Set_Return_Value (Data, B.all);
                     else
                        Glib.Convert.Convert
                          (B.all,
                           "UTF-8",
                           To_Charset,
                           Ignore, Bytes, Result => Result_String);
                        Set_Return_Value (Data, Result_String (1 .. Bytes));
                     end if;
                  end;

                  Free (B);
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
            To_File : Virtual_File := GNATCOLL.VFS.No_File;
            Result  : Boolean;
         begin
            if Number_Of_Arguments (Data) >= 2 then
               To_File := Create (Nth_Arg (Data, 2), Kernel);
            end if;

            if Child /= null then
               if To_File /= GNATCOLL.VFS.No_File then
                  Save_To_File
                    (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
                     To_File,
                     Result,
                     True);

               else
                  Save_To_File
                    (Get_Buffer (Source_Editor_Box (Get_Widget (Child))),
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
            Box         : Source_Editor_Box;
            Highlight_Category : Natural := 0;
            Style       : Style_Access;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Number_Of_Arguments (Data) >= 4 then
               Style := Get_Or_Create_Style
                 (Kernel, Nth_Arg (Data, 4), False);

               if Style = null then
                  Set_Error_Msg (Data, -"No such style: " & Nth_Arg (Data, 4));
                  return;
               else
                  Highlight_Category :=
                    Line_Highlighting.Lookup_Category (Style);
               end if;
            end if;

            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));

               if Line >= 0 and then Number > 0 then
                  Marker := Create_File_Marker
                    (Kernel, Filename,
                     Add_Special_Blank_Lines
                       (Get_Buffer (Box),
                        Editable_Line_Type (Line),
                        Highlight_Category, Number, "", "", null));
                  Set_Return_Value (Data, Get_Id (Marker));
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;
         end;

      elsif Command = "remove_blank_lines" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         declare
            Child  : MDI_Child;
            Number : Integer := 0;
            Box    : Source_Editor_Box;
         begin
            if Number_Of_Arguments (Data) >= 2 then
               Number := Nth_Arg (Data, 2);
            end if;

            if Get_Mark (Marker) /= null then
               Child := Find_Editor (Kernel, Get_File (Marker));
               Box := Source_Editor_Box (Get_Widget (Child));
               Src_Editor_Buffer.Line_Information.Remove_Blank_Lines
                 (Get_Buffer (Box), Get_Mark (Marker), Number);
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
            Box         : Source_Editor_Box;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));

               if Line = 0 then
                  Src_Editor_Buffer.Line_Information.Fold_All
                    (Get_Buffer (Box));
               else
                  Compute_Blocks (Get_Buffer (Box));
                  Src_Editor_Buffer.Line_Information.Fold_Block
                    (Get_Buffer (Box), Editable_Line_Type (Line));
               end if;
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "block_unfold" then
         declare
            Filename : constant Virtual_File :=
                         Create (Nth_Arg (Data, 1), Kernel);
            Line     : constant Integer := Nth_Arg (Data, 2, 0);
            Child    : MDI_Child;
            Box      : Source_Editor_Box;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));

               if Line = 0 then
                  Src_Editor_Buffer.Line_Information.Fold_All
                    (Get_Buffer (Box));
               else
                  Src_Editor_Buffer.Line_Information.Unfold_Line
                    (Get_Buffer (Box), Editable_Line_Type (Line));
               end if;
            else
               Set_Error_Msg (Data, -"file not found or not open");
            end if;
         end;

      elsif Command = "set_background_color" then
         declare
            Filename : constant Virtual_File :=
                         Create (Nth_Arg (Data, 1), Kernel);
            Color    : constant String := Nth_Arg (Data, 2);
            Box      : Source_Editor_Box;
            Child    : MDI_Child;
            Col      : Gdk_Color;
         begin
            Child := Find_Editor (Kernel, Filename);

            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));
               Col := Parse (Color);
               Alloc (Gtk.Widget.Get_Default_Colormap, Col);
               Modify_Base (Get_View (Box), State_Normal, Col);
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
                 (Get_View (Source_Editor_Box (Get_Widget (Child_1))),
                  Get_View (Source_Editor_Box (Get_Widget (Child_2))));

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
                             (Source_Editor_Box (Get_Widget (Child_2))),
                           Get_View
                             (Source_Editor_Box (Get_Widget (Child_3))));

                        Set_Synchronized_Editor
                          (Get_View
                             (Source_Editor_Box (Get_Widget (Child_3))),
                           Get_View
                             (Source_Editor_Box (Get_Widget (Child_1))));
                     end if;

                     Triplet (3) := Child_3;
                  end;

               else
                  Set_Synchronized_Editor
                    (Get_View (Source_Editor_Box (Get_Widget (Child_2))),
                     Get_View (Source_Editor_Box (Get_Widget (Child_1))));
               end if;

               for C in Triplet'Range loop
                  if Triplet (C) /= null then
                     Connect
                       (Triplet (C), Signal_Grab_Focus,
                        Marshallers.Void_Marshaller.To_Marshaller
                          (On_Raise_Child'Access),
                        User_Data => Triplet);
                     Connect
                       (Triplet (C), Signal_Destroy,
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
            File  : constant Virtual_File :=
                      Create (Nth_Arg (Data, 1), Kernel);
            Write : constant Boolean := Nth_Arg (Data, 2);
            Child : MDI_Child;
         begin
            Child := Find_Editor (Kernel, File);

            if Child /= null then
               Set_Writable
                 (Source_Editor_Box (Get_Widget (Child)),
                  Write, Explicit => True);
            else
               Trace (Me, "Editor not found: " & Display_Full_Name (File));
            end if;
         end;

      elsif Command = "set_title" then
         Name_Parameters (Data, Set_Title_Cmd_Parameters);

         declare
            File     : constant Virtual_File :=
                         Create (Nth_Arg (Data, 1), Kernel);
            Title    : constant String := Nth_Arg (Data, 2);
            Filename : constant String := Nth_Arg (Data, 3, "");
            Child    : MDI_Child;
         begin
            Child := Find_Editor (Kernel, File);

            if Child /= null then
               if Filename = "" then
                  Set_Title (Child, Display_Full_Name (File), Title);
               else
                  Set_Title (Child, Filename, Title);
               end if;
            end if;
         end;
      end if;
   end Edit_Command_Handler;

   -----------------
   -- Buffer_Cmds --
   -----------------

   procedure Buffer_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel      : constant Kernel_Handle := Get_Kernel (Data);
      File_Inst   : Class_Instance;

   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, -("Cannot build instances of EditorBuffer."
                                & " Use EditorBuffer.get() instead"));

      elsif Command = "get" then
         Name_Parameters (Data, (1 => File_Cst'Access,
                                 2 => Force_Cst'Access,
                                 3 => Open_Cst'Access));
         File_Inst := Nth_Arg
           (Data, 1, Get_File_Class (Kernel),
            Default => No_Class_Instance, Allow_Null => True);

         Set_Return_Value
           (Data, Create_Editor_Buffer
              (Get_Script (Data),
               Get_Buffer_Factory (Kernel).Get
                 (File        => Get_Data (File_Inst),
                  Force       => Nth_Arg (Data, 2, Default => False),
                  Open_View   => Nth_Arg (Data, 3, Default => True),
                  Open_Buffer => False)));

      elsif Command = "get_new" then
         Set_Return_Value
           (Data, Create_Editor_Buffer
              (Get_Script (Data),
               Get_Buffer_Factory (Kernel).Get_New));

      elsif Command = "list" then
         declare
            use Buffer_Lists;
            List : constant Buffer_Lists.List :=
              Get_Buffer_Factory (Kernel).Buffers;
            C    : Buffer_Lists.Cursor := First (List);
         begin
            Set_Return_Value_As_List (Data);
            while Has_Element (C) loop
               Set_Return_Value
                 (Data, Create_Editor_Buffer
                    (Get_Script (Data), Buffer_Lists.Element (C)));
               Next (C);
            end loop;
         end;

      elsif Command = "file" then
         Set_Return_Value
           (Data, Create_File (Get_Script (Data), Get_Buffer (Data, 1).File));

      elsif Command = "current_view" then
         Set_Return_Value
           (Data, Create_Editor_View (Get_Script (Data),
                                      Get_Buffer (Data, 1).Current_View));

      elsif Command = "views" then
         declare
            use View_Lists;
            Views : constant View_Lists.List := Get_Buffer (Data, 1).Views;
            C     : View_Lists.Cursor := First (Views);
         begin
            Set_Return_Value_As_List (Data);
            while Has_Element (C) loop
               Set_Return_Value
                 (Data, Create_Editor_View
                    (Get_Script (Data), View_Lists.Element (C)));
               Next (C);
            end loop;
         end;

      elsif Command = "close" then
         Name_Parameters (Data, (1 => Force_Cst'Access));
         Get_Buffer (Data, 1).Close (Force => Nth_Arg (Data, 2, False));

      elsif Command = "save" then
         Name_Parameters (Data, (1 => Interactive_Cst'Access,
                                 2 => File_Cst'Access));
         File_Inst := Nth_Arg
           (Data, 3, Get_File_Class (Kernel),
            Default => No_Class_Instance, Allow_Null => True);
         Get_Buffer (Data, 1).Save
           (Interactive => Nth_Arg (Data, 2, False),
            File        => Get_Data (File_Inst));

      elsif Command = "characters_count" then
         Set_Return_Value (Data, Get_Buffer (Data, 1).Characters_Count);

      elsif Command = "lines_count" then
         Set_Return_Value (Data, Get_Buffer (Data, 1).Lines_Count);

      elsif Command = "select" then
         Name_Parameters (Data, (1 => Start_Cst'Access,
                                 2 => End_Cst'Access));
         Get_Buffer (Data, 1).Select_Text
           (From => Get_Location (Data, 2),
            To   => Get_Location (Data, 3));

      elsif Command = "unselect" then
         Get_Buffer (Data, 1).Unselect;

      elsif Command = "selection_start" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Buffer (Data, 1).Selection_Start));

      elsif Command = "selection_end" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Buffer (Data, 1).Selection_End));

      elsif Command = "beginning_of_buffer" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Buffer (Data, 1).Beginning_Of_Buffer));

      elsif Command = "end_of_buffer" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Buffer (Data, 1).End_Of_Buffer));

      elsif Command = "is_modified" then
         Set_Return_Value (Data, Get_Buffer (Data, 1).Is_Modified);

      elsif Command = "get_chars" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Set_Return_Value
           (Data, Get_Buffer (Data, 1).Get_Chars
            (From => Get_Location (Data, 2), To => Get_Location (Data, 3)));

      elsif Command = "insert" then
         Name_Parameters
           (Data, (1 => Location_Cst'Access, 2 => Text_Cst'Access));
         Get_Buffer (Data, 1).Insert
           (Get_Location (Data, 2), Nth_Arg (Data, 3));

      elsif Command = "delete" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Data, 1).Delete
           (From => Get_Location (Data, 2), To => Get_Location (Data, 3));

      elsif Command = "copy" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access,
                                 3 => Append_Cst'Access));
         Get_Buffer (Data, 1).Copy
           (From   => Get_Location (Data, 2),
            To     => Get_Location (Data, 3),
            Append => Nth_Arg (Data, 4, False));

      elsif Command = "cut" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access,
                                 3 => Append_Cst'Access));
         Get_Buffer (Data, 1).Cut
           (From   => Get_Location (Data, 2),
            To     => Get_Location (Data, 3),
            Append => Nth_Arg (Data, 4, False));

      elsif Command = "paste" then
         Name_Parameters (Data, (1 => Location_Cst'Access));
         Get_Buffer (Data, 1).Paste (Get_Location (Data, 2));

      elsif Command = "blocks_fold" then
         Get_Buffer (Data, 1).Blocks_Fold;

      elsif Command = "blocks_unfold" then
         Get_Buffer (Data, 1).Blocks_Unfold;

      elsif Command = "indent" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Data, 1).Indent
           (From => Get_Location (Data, 2), To => Get_Location (Data, 3));

      elsif Command = "refill" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Data, 1).Refill
           (From => Get_Location (Data, 2), To => Get_Location (Data, 3));

      elsif Command = "get_mark" then
         Name_Parameters (Data, (2 => Name_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Mark
              (Get_Script (Data),
               Get_Buffer (Data, 1).Get_Mark (Name => Nth_Arg (Data, 2))));

      elsif Command = "create_overlay" then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Overlay
              (Get_Script (Data),
               Get_Buffer (Data, 1).Create_Overlay (Nth_Arg (Data, 2, ""))));

      elsif Command = "apply_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access,
                                 2 => From_Cst'Access,
                                 3 => To_Cst'Access));
         Get_Buffer (Data, 1).Apply_Overlay
           (Get_Overlay (Data, 2),
            Get_Location (Data, 3), Get_Location (Data, 4));

      elsif Command = "remove_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access,
                                 2 => From_Cst'Access,
                                 3 => To_Cst'Access));
         Get_Buffer (Data, 1).Remove_Overlay
           (Get_Overlay (Data, 2),
            Get_Location (Data, 3), Get_Location (Data, 4));

      elsif Command = "start_undo_group" then
         Get_Buffer (Data, 1).Start_Undo_Group;

      elsif Command = "finish_undo_group" then
         Get_Buffer (Data, 1).Finish_Undo_Group;

      elsif Command = "undo" then
         Get_Buffer (Data, 1).Undo;

      elsif Command = "redo" then
         Get_Buffer (Data, 1).Redo;

      elsif Command = "set_read_only" then
         Name_Parameters (Data, (1 => Read_Only_Cst'Access));
         Get_Buffer (Data, 1).Set_Read_Only
           (Read_Only => Nth_Arg (Data, 2, True));

      elsif Command = "is_read_only" then
         Set_Return_Value (Data, Get_Buffer (Data, 1).Is_Read_Only);

      elsif Command = "add_special_line" then
         Set_Return_Value
           (Data, Create_Editor_Mark
              (Get_Script (Data),
               Get_Buffer (Data, 1).Add_Special_Line
               (Start_Line => Nth_Arg (Data, 2),
                Text       => Nth_Arg (Data, 3),
                Category   => Nth_Arg (Data, 4, ""),
                Name       => Nth_Arg (Data, 5, ""))));

      elsif Command = "remove_special_lines" then
         Get_Buffer (Data, 1).Remove_Special_Lines
           (Mark  => Get_Mark (Data, 2),
            Lines => Nth_Arg (Data, 3, 0));

      else
         Set_Error_Msg (Data, -"Command not implemented: " & Command);
      end if;

   exception
      when E : Editor_Exception =>
         Set_Error_Msg (Data, Exception_Message (E));
   end Buffer_Cmds;

   -------------------
   -- Location_Cmds --
   -------------------

   procedure Location_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      EditorLoc  : constant Class_Type :=
                     New_Class (Get_Kernel (Data), Editor_Location_Class_Name);
      Count       : Gint;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Buffer_Cst'Access,
                                 2 => Line_Cst'Access,
                                 3 => Col_Cst'Access));
         Set_Data
           (Nth_Arg (Data, 1, EditorLoc),
            Editor_Location_Class_Name,
            Get_Buffer (Data, 2).New_Location
            (Line   => Integer'Max (1, Nth_Arg (Data, 3)),
             Column =>
               Visible_Column_Type (Integer'Max (1, Nth_Arg (Data, 4)))));

      elsif Command = Comparison_Method then
         declare
            Loc1 : constant Editor_Location'Class := Get_Location (Data, 1);
            Loc2 : constant Editor_Location'Class := Get_Location (Data, 2);
         begin
            if Loc1 = Nil_Editor_Location
              or else Loc2 = Nil_Editor_Location
              or else Loc1.Buffer /= Loc2.Buffer
            then
               Set_Error_Msg (Data, -"EditorLocation not in the same buffer");
            else
               Set_Return_Value (Data, Integer (Compare (Loc1, Loc2)));
            end if;
         end;

      elsif Command = "line" then
         Set_Return_Value (Data, Get_Location (Data, 1).Line);

      elsif Command = "column" then
         Set_Return_Value (Data, Integer (Get_Location (Data, 1).Column));

      elsif Command = "offset" then
         Set_Return_Value (Data, Get_Location (Data, 1).Offset);

      elsif Command = "buffer" then
         Set_Return_Value
           (Data, Create_Editor_Buffer
              (Get_Script (Data), Get_Location (Data, 1).Buffer));

      elsif Command = "beginning_of_line" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Location (Data, 1).Beginning_Of_Line));

      elsif Command = "end_of_line" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Location (Data, 1).End_Of_Line));

      elsif Command = "search" then
         Name_Parameters
           (Data,
            ( --  1 =>  Self
             2 => Pattern_Cst'Access,
             3 => Backward_Cst'Access,
             4 => Case_Cst'Access,
             5 => Regexp_Cst'Access,
             6 => Whole_Word_Cst'Access,
             7 => Scope_Cst'Access,
             8 => Dialog_On_Failure_Cst'Access));

         declare
            Loc    : constant Editor_Location'Class := Get_Location (Data, 1);
            Starts : Editor_Location'Class := Loc;
            Ends   : Editor_Location'Class := Loc;
            Found  : Boolean;
         begin
            Loc.Search
              (Pattern           => Nth_Arg (Data, 2),
               Backward          => Nth_Arg (Data, 3, False),
               Case_Sensitive    => Nth_Arg (Data, 4, False),
               Regexp            => Nth_Arg (Data, 5, False),
               Whole_Word        => Nth_Arg (Data, 6, False),
               Scope             => Nth_Arg (Data, 7, "Whole"),
               Dialog_On_Failure => Nth_Arg (Data, 8, True),
               Success           => Found,
               Starts            => Starts,
               Ends              => Ends);
            if Found then
               Set_Return_Value_As_List (Data);
               Set_Return_Value
                 (Data,
                  Create_Editor_Location (Get_Script (Data), Starts));
               Set_Return_Value
                 (Data,
                  Create_Editor_Location (Get_Script (Data), Ends));
            end if;
         end;

      elsif Command = "forward_char" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data),
               Get_Location (Data, 1).Forward_Char (Nth_Arg (Data, 2, 1))));

      elsif Command = Addition_Method then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data),
               Get_Location (Data, 1).Forward_Char (Nth_Arg (Data, 2, 1))));

      elsif Command = Substraction_Method then
         Name_Parameters (Data, (1 => Count_Cst'Access));

         begin
            --  The second argument is an integer ?
            Count := Gint (Integer'(Nth_Arg (Data, 2)));
            Set_Return_Value
              (Data, Create_Editor_Location
                 (Get_Script (Data),
                  Get_Location (Data, 1).Forward_Char (-Integer (Count))));
         exception
            when Invalid_Parameter =>
               declare
                  Loc1 : constant Editor_Location'Class :=
                    Get_Location (Data, 1);
                  Loc2 : constant Editor_Location'Class :=
                    Get_Location (Data, 2);
               begin
                  if Loc1.Buffer /= Loc2.Buffer then
                     Set_Error_Msg
                       (Data, -"Locations not in the same buffer");
                  else
                     Set_Return_Value (Data, Loc1.Offset - Loc2.Offset);
                  end if;
               end;
         end;

      elsif Command = "forward_word" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data),
               Get_Location (Data, 1).Forward_Word (Nth_Arg (Data, 2, 1))));

      elsif Command = "starts_word" then
         Set_Return_Value (Data, Get_Location (Data, 1).Starts_Word);

      elsif Command = "ends_word" then
         Set_Return_Value (Data, Get_Location (Data, 1).Ends_Word);

      elsif Command = "inside_word" then
         Set_Return_Value (Data, Get_Location (Data, 1).Inside_Word);

      elsif Command = "forward_line" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data),
               Get_Location (Data, 1).Forward_Line (Nth_Arg (Data, 2, 1))));

      elsif Command = "create_mark" then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Mark
              (Get_Script (Data),
               Get_Location (Data, 1).Create_Mark (Nth_Arg (Data, 2, ""))));

      elsif Command = "get_char" then
         declare
            Unichar : constant Integer :=
              Get_Location (Data, 1).Get_Char;
            Buffer  : String (1 .. 6);
            Last    : Natural;
         begin
            Unichar_To_UTF8 (Gunichar (Unichar), Buffer, Last);
            Set_Return_Value (Data, Buffer (1 .. Last));
         end;

      elsif Command = "block_fold" then
         Get_Location (Data, 1).Block_Fold;

      elsif Command = "block_unfold" then
         Get_Location (Data, 1).Block_Unfold;

      elsif Command = "block_end_line" then
         Set_Return_Value (Data, Get_Location (Data, 1).Block_End.Line);

      elsif Command = "block_start_line" then
         Set_Return_Value (Data, Get_Location (Data, 1).Block_Start.Line);

      elsif Command = "block_start" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Location (Data, 1).Block_Start));

      elsif Command = "block_end" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Location (Data, 1).Block_End));

      elsif Command = "block_type" then
         Set_Return_Value
           (Data, Language_Category'Image (Get_Location (Data, 1).Block_Type));

      elsif Command = "block_name" then
         Set_Return_Value (Data, Get_Location (Data, 1).Block_Name (False));

      elsif Command = "subprogram_name" then
         Set_Return_Value (Data, Get_Location (Data, 1).Block_Name (True));

      elsif Command = "block_level" then
         Set_Return_Value (Data, Get_Location (Data, 1).Block_Level);

      elsif Command = "get_overlays" then
         declare
            use Overlay_Lists;
            List : constant Overlay_Lists.List :=
              Get_Location (Data, 1).Get_Overlays;
            C    : Overlay_Lists.Cursor := First (List);
         begin
            Set_Return_Value_As_List (Data);
            while Has_Element (C) loop
               Set_Return_Value
                 (Data, Create_Editor_Overlay
                    (Get_Script (Data), Overlay_Lists.Element (C)));
               Next (C);
            end loop;
         end;

      elsif Command = "has_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access));
         Set_Return_Value
           (Data, Get_Location (Data, 1).Has_Overlay (Get_Overlay (Data, 2)));

      elsif Command = "forward_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data),
               Get_Location
                 (Data, 1).Forward_Overlay (Get_Overlay (Data, 2, True))));

      elsif Command = "backward_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access));
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data),
               Get_Location
                 (Data, 1).Backward_Overlay (Get_Overlay (Data, 2, True))));
      end if;
   end Location_Cmds;

   ---------------
   -- Mark_Cmds --
   ---------------

   procedure Mark_Cmds
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, "Cannot create an EditorMark directly");

      elsif Command = Destructor_Method then
         --  Do not delete named marks, since we can still access them
         --  through get_mark() anyway
         begin
            declare
               M : constant Editor_Mark'Class := Get_Mark (Data, 1);
            begin
               if M.Name = "" then
                  M.Delete;
               end if;
            end;
         exception
            when Editor_Exception =>
               null;  --  The gtk+ mark might already have been destroyed, so
                        --  this isn't an error
         end;

      elsif Command = "delete" then
         Get_Mark (Data, 1).Delete;

      elsif Command = "is_present" then
         Set_Return_Value (Data, Get_Mark (Data, 1).Is_Present);

      elsif Command = "location" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_Mark (Data, 1).Location));

      elsif Command = "move" then
         Name_Parameters (Data, (1 => Location_Cst'Access));
         Get_Mark (Data, 1).Move (Get_Location (Data, 2));
      end if;
   end Mark_Cmds;

   ---------------
   -- View_Cmds --
   ---------------

   procedure View_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      EditorView : constant Class_Type :=
                     New_Class (Get_Kernel (Data), "EditorView");
      Inst       : Class_Instance;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Buffer_Cst'Access));
         Inst := Nth_Arg (Data, 1, EditorView);
         Set_Data (Inst, Get_Buffer (Data, 2).New_View);

      elsif Command = "buffer" then
         Set_Return_Value
           (Data, Create_Editor_Buffer
              (Get_Script (Data), Get_View (Data, 1).Buffer));

      elsif Command = "set_read_only" then
         Name_Parameters (Data, (1 => Read_Only_Cst'Access));
         Get_View (Data, 1).Set_Read_Only (Nth_Arg (Data, 2, True));

      elsif Command = "is_read_only" then
         Set_Return_Value (Data, Get_View (Data, 1).Is_Read_Only);

      elsif Command = "center" then
         Get_View (Data, 1).Center (Get_Location (Data, 2));

      elsif Command = "title" then
         Name_Parameters (Data, (1 => Short_Cst'Access));
         Set_Return_Value
           (Data, Get_View (Data, 1).Title (Short => Nth_Arg (Data, 2, True)));

      elsif Command = "goto" then
         Get_View (Data, 1).Cursor_Goto
           (Get_Location (Data, 2),
            Centering => Minimal,
            Extend_Selection => Nth_Arg (Data, 3, False));

      elsif Command = "cursor" then
         Set_Return_Value
           (Data, Create_Editor_Location
              (Get_Script (Data), Get_View (Data, 1).Cursor));
      end if;
   end View_Cmds;

   ------------------
   -- Overlay_Cmds --
   ------------------

   procedure Overlay_Cmds
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data,
            -("Cannot create instances directly, "
              & " use EditorBuffer.create_overlay()"));

      elsif Command = "name" then
         Set_Return_Value (Data, Get_Overlay (Data, 1).Name);

      elsif Command = "get_property" then
         Name_Parameters (Data, (1 => Name_Cst'Access));

         declare
            Name : constant String := Nth_Arg (Data, 2);
         begin
            if Name = "foreground"
              or else Name = "background"
              or else Name = "font"
              or else Name = "weight"
              or else Name = "style"
            then
               Set_Return_Value
                 (Data, String'(Get_Overlay (Data, 1).Get_Property (Name)));

            elsif Name = "editable" then
               Set_Return_Value
                 (Data, Boolean'(Get_Overlay (Data, 1).Get_Property (Name)));
            end if;
         end;

      elsif Command = "set_property" then
         Name_Parameters (Data, (1 => Name_Cst'Access, 2 => Value_Cst'Access));

         declare
            Name : constant String := Nth_Arg (Data, 2);
         begin
            if Name = "foreground"
              or else Name = "background"
              or else Name = "font"
              or else Name = "weight"
              or else Name = "style"
            then
               Get_Overlay (Data, 1).Set_Property
                 (Name, String'(Nth_Arg (Data, 3)));

            elsif Name = "editable" then
               Get_Overlay (Data, 1).Set_Property
                 (Name, Boolean'(Nth_Arg (Data, 3)));
            end if;
         end;

      end if;
   end Overlay_Cmds;

   ---------------------------
   -- Hyper_Command_Handler --
   ---------------------------

   procedure Hyper_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel            : constant Kernel_Handle := Get_Kernel (Data);
      EditorHighlighter : constant Class_Type :=
                            New_Class (Kernel, "EditorHighlighter");
      H                 : Highlighter_Record;

      function Get_Data
        (Data : Callback_Data'Class; N : Positive) return Highlighter_Record;
      --  Retrieve an instance from a class

      --------------
      -- Get_Data --
      --------------

      function Get_Data
        (Data : Callback_Data'Class; N : Positive) return Highlighter_Record is
         Inst : constant Class_Instance :=
                  Nth_Arg (Data, N, EditorHighlighter);
      begin
         return Highlighter_Property_Access
           (Instance_Property'
              (Get_Data (Inst, "EditorHighlighter"))).Highlighter;
      end Get_Data;

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Highlighter_Constructor_Args);

         declare
            Inst    : constant Class_Instance :=
                        Nth_Arg (Data, 1, EditorHighlighter);
            Pattern : constant String := Nth_Arg (Data, 2);
            Action  : constant Subprogram_Type := Nth_Arg (Data, 3);
            Index   : constant Integer := Nth_Arg (Data, 4, 0);
            Altern  : constant Subprogram_Type := Nth_Arg (Data, 5, null);
         begin
            if Action = null then
               Set_Error_Msg
                 (Data,
                  -"Could not find action """ & Nth_Arg (Data, 3) & """");
               return;
            end if;

            if Pattern = "" then
               Set_Error_Msg
                 (Data,
                  -"Cannot register a highlighter for an empty pattern");
               return;
            end if;

            declare
            begin
               H.Pattern := new Pattern_Matcher'(Compile (Pattern));
            exception
               when Expression_Error =>
                  Set_Error_Msg
                    (Data,
                     -"Cannot register highlighter: invalid pattern "
                     & Pattern);
                  return;
            end;

            --  ??? Verify that no pattern matcher with the same string has
            --  been registered.

            H.Pattern_String := new String'(Pattern);
            H.Paren_Count := Paren_Count (H.Pattern.all);
            H.Index := Index;
            H.Action := Action;
            H.Alternate := Altern;

            Set_Data
              (Inst, "EditorHighlighter",
               Highlighter_Property'(Highlighter => H));

            Register_Highlighter (H);
         end;

      elsif Command = "remove" then
         if Kernel.In_Hyper_Mode then
            Set_Error_Msg
              (Data,
               -"Cannot remove a highlighter while the hyper mode is enabled");
            return;
         end if;

         H := Get_Data (Data, 1);

         Unregister_Highlighter (H);
      end if;
   end Hyper_Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      EditorLoc    : constant Class_Type :=
                       New_Class (Kernel, Editor_Location_Class_Name);
      Editor_Class : constant Class_Type := New_Class (Kernel, "Editor");
      EditorBuffer : constant Class_Type := New_Class (Kernel, "EditorBuffer");
      EditorMark   : constant Class_Type := New_Class (Kernel, "EditorMark");
      EditorView   : constant Class_Type :=
                       New_Class
                         (Kernel, "EditorView", Get_GUI_Class (Kernel));
      EditorOverlay : constant Class_Type :=
                        New_Class (Kernel, "EditorOverlay");

      EditorHighlighter : constant Class_Type := New_Class
        (Kernel, "EditorHighlighter");
   begin
      --  EditorOverlay

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Overlay_Cmds'Access, EditorOverlay);
      Register_Command
        (Kernel, "get_property", 1, 1, Overlay_Cmds'Access, EditorOverlay);
      Register_Command
        (Kernel, "set_property", 2, 2, Overlay_Cmds'Access, EditorOverlay);
      Register_Command
        (Kernel, "name", 0, 0, Overlay_Cmds'Access, EditorOverlay);

      --  EditorLocation

      Register_Command
        (Kernel, Constructor_Method, 3, 3, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, Comparison_Method, 1, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, Addition_Method, 1, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, Substraction_Method, 1, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "line", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "column", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "offset", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "buffer", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "beginning_of_line", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "end_of_line", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "forward_char", 0, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "forward_word", 0, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "forward_line", 0, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "create_mark", 0, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "get_char", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_fold", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_unfold", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_end_line", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_start_line", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_start", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_end", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_level", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_name", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "block_type", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "starts_word", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "ends_word", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "inside_word", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "subprogram_name", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "get_overlays", 0, 0, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "has_overlay", 1, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "forward_overlay", 0, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "backward_overlay", 0, 1, Location_Cmds'Access, EditorLoc);
      Register_Command
        (Kernel, "search", 1, 7, Location_Cmds'Access, EditorLoc);

      --  EditorMark

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Mark_Cmds'Access, EditorMark);
      Register_Command
        (Kernel, Destructor_Method, 0, 0, Mark_Cmds'Access, EditorMark);
      Register_Command
        (Kernel, "delete", 0, 0, Mark_Cmds'Access, EditorMark);
      Register_Command
        (Kernel, "is_present", 0, 0, Mark_Cmds'Access, EditorMark);
      Register_Command
        (Kernel, "location", 0, 0, Mark_Cmds'Access, EditorMark);
      Register_Command (Kernel, "move", 1, 1, Mark_Cmds'Access, EditorMark);

      --  EditorBuffer

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "get", 0, 3, Buffer_Cmds'Access, EditorBuffer, True);
      Register_Command
        (Kernel, "get_new", 0, 0, Buffer_Cmds'Access, EditorBuffer, True);
      Register_Command
        (Kernel, "list", 0, 0, Buffer_Cmds'Access, EditorBuffer, True);
      Register_Command
        (Kernel, "create_overlay",  0, 1, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "apply_overlay",  1, 3, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "remove_overlay",  1, 3, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "file", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "beginning_of_buffer",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "end_of_buffer", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "current_view", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "views", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "close", 0, 1, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "save", 0, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "characters_count", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "lines_count", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "select", 0, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "unselect", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "selection_start", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "selection_end", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "copy", 0, 3, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "cut", 0, 3, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "paste", 1, 1, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "get_mark", 1, 1, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "is_modified", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "blocks_fold", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "blocks_unfold", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "undo", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "redo", 0, 0, Buffer_Cmds'Access, EditorBuffer);
--        Register_Command
--          (Kernel, "add_gap", 3, 3, Buffer_Cmds'Access, EditorBuffer);
--        Register_Command
--          (Kernel, "remove_gap", 1, 2, Buffer_Cmds'Access, EditorBuffer);
--        Register_Command
--          (Kernel, "synchronize_scrolling", 1, 2, Buffer_Cmds'Access,
--           EditorBuffer);
      Register_Command
        (Kernel, "get_chars", 0, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "insert", 2, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "delete", 0, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "indent", 0, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "refill", 0, 2, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "start_undo_group", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "finish_undo_group", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "set_read_only", 0, 1, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "is_read_only", 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "add_special_line", 2, 4, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel,
         "remove_special_lines", 2, 3, Buffer_Cmds'Access, EditorBuffer);

      --  EditorView

      Register_Command
        (Kernel, Constructor_Method, 1, 1, View_Cmds'Access, EditorView);
      Register_Command
        (Kernel, "buffer", 0, 0, View_Cmds'Access, EditorView);
      Register_Command
        (Kernel, "set_read_only", 0, 1, View_Cmds'Access, EditorView);
      Register_Command
        (Kernel, "is_read_only", 0, 0, View_Cmds'Access, EditorView);
      Register_Command (Kernel, "center", 0, 1, View_Cmds'Access, EditorView);
      Register_Command (Kernel, "goto", 1, 2, View_Cmds'Access, EditorView);
      Register_Command (Kernel, "cursor", 0, 0, View_Cmds'Access, EditorView);
      Register_Command (Kernel, "title", 0, 1, View_Cmds'Access, EditorView);

      --  Searching

      Register_Command
        (Kernel, "search", 1, 3, File_Search_Command_Handler'Access,
         Get_File_Class (Kernel));
      Register_Command
        (Kernel, "search_next", 1, 3, Current_Search_Command_Handler'Access,
         Get_File_Class (Kernel));
      Register_Command
        (Kernel, "search", 1, 5, Project_Search_Command_Handler'Access,
         Get_Project_Class (Kernel));

      --  The old Editor class
      Register_Command
        (Kernel, "edit", 1, 6, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "create_mark", 1, 4, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "print_line_info", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "highlight", 2, 3,
         Line_Highlighting.Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "add_blank_lines", 3, 4, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "remove_blank_lines", 1, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_fold", 1, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_unfold", 1, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "unhighlight", 2, 3,
         Line_Highlighting.Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "highlight_range", 2, 5,
         Line_Highlighting.Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "unhighlight_range", 2, 5,
         Line_Highlighting.Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "register_highlighting", 2, 3,
         Line_Highlighting.Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "set_background_color", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "goto_mark", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "delete_mark", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "get_chars", 1, 5, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "get_line", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "get_column", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "get_file", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "get_last_line", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_get_start", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_get_end", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_get_name", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_get_type", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "block_get_level", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "subprogram_name", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "cursor_get_line", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "cursor_get_column", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "cursor_set_position", 2, 3, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "cursor_center", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "get_buffer", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "save_buffer", 1, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "replace_text", 4, 6, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "indent",
         Minimum_Args  => Indent_Cmd_Parameters'Length - 1,
         Maximum_Args  => Indent_Cmd_Parameters'Length,
         Class         => Editor_Class,
         Static_Method => True,
         Handler       => Edit_Command_Handler'Access);
      Register_Command
        (Kernel, "indent_buffer", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "refill", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "cut", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "copy", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "paste", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "select_all", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "select_text", 2, 4, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "insert_text", 1, 1, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "undo", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "redo", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "close", 1, 1, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "save", 0, 2, Edit_Command_Handler'Access, Editor_Class,
         True);
      Register_Command
        (Kernel, "set_synchronized_scrolling", 2, 3,
         Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "add_case_exception", 1, 1,
         Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "remove_case_exception", 1, 1,
         Edit_Command_Handler'Access, Editor_Class, True);
      Register_Command
        (Kernel, "set_writable", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
      Register_Command
        (Kernel, "set_title", 2, 3, Edit_Command_Handler'Access,
         Editor_Class, True);

      Register_Command
        (Kernel, "mark_current_location", 0, 0, Edit_Command_Handler'Access,
         Editor_Class, True);

      --  Register the debug commands
      Src_Editor_Buffer.Debug.Register (Kernel);

      --  Register the commands related to hyper mode

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 2,
         Maximum_Args => 4,
         Handler      => Hyper_Command_Handler'Access,
         Class        => EditorHighlighter);

      Register_Command
        (Kernel, "remove",
         Handler      => Hyper_Command_Handler'Access,
         Class        => EditorHighlighter);
   end Register_Commands;

end Src_Editor_Module.Shell;
