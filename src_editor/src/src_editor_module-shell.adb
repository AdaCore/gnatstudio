-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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
with Ada.Unchecked_Conversion;
with Ada.Tags;                  use Ada.Tags;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with System;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Scripts.Gtkada;   use GNATCOLL.Scripts, GNATCOLL.Scripts.Gtkada;
with GNAT.Strings;

with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Glib.Unicode;              use Glib.Unicode;

with Gdk.Color;                 use Gdk.Color;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Object;                use Gtk.Object;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_Tag;              use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;        use Gtk.Text_Tag_Table;

with Pango.Enums;               use Pango.Enums;

with Casing_Exceptions;         use Casing_Exceptions;
with Commands;                  use Commands;
with Find_Utils;                use Find_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Charsets;       use GPS.Kernel.Charsets;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Language;                  use Language;
with Projects.Registry;         use Projects.Registry;
with Projects;                  use Projects;
with Src_Contexts;              use Src_Contexts;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Text_Handling;
use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Module.Line_Highlighting;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;
with Src_Editor_View;           use Src_Editor_View;
with Traces;                    use Traces;

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
      2 => Title_Cst'Access);

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
     (Script : access Scripting_Language_Record'Class;
      Tag    : Gtk_Text_Tag) return Class_Instance;
   --  Manipulation of instances of the EditorOverlay class.
   --  Result must be freed unless you assign it to a Callback_Data

   function Create_Editor_Buffer
     (Script : access Scripting_Language_Record'Class;
      Buffer : Source_Buffer) return Class_Instance;
   --  Manipulation of instances of the EditorBuffer class.
   --  Result of Create_Editor_Buffer must be freed unless you assign it to
   --  a Callback_Data

   function Create_Editor_View
     (Script : access Scripting_Language_Record'Class;
      View   : Source_Editor_Box) return Class_Instance;
   --  Return an instance of EditorView encapsulating View. Result must be
   --  freed unless you assign it to a Callback_Data.

   function Create_Editor_Mark
     (Script : access Scripting_Language_Record'Class;
      Mark   : Gtk_Text_Mark) return Class_Instance;
   --  Return an instance of EditorMark encapsulating Mark

   type Location_Info is record
      Buffer : Source_Buffer;
      Line   : Editable_Line_Type;
      Offset : Visible_Column_Type;
   end record;
   type Location_Info_Access is access Location_Info;

   type Location_Property is new Instance_Property_Record with record
      Location : Location_Info_Access;
   end record;
   type Location_Property_Access is access all Location_Property;
   overriding procedure Destroy (Property : in out Location_Property);

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Location_Info_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Location_Info_Access, System.Address);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Info, Location_Info_Access);

   procedure On_Buffer_Destroyed_For_Location (Data, Obj : System.Address);
   pragma Convention (C, On_Buffer_Destroyed_For_Location);
   --  Called when the buffer stored in an instance of EditorLocation is
   --  destroyed

   procedure Set_Location_Data
     (Inst   : Class_Instance;
      Buffer : Source_Buffer;
      Line   : Editable_Line_Type;
      Offset : Visible_Column_Type);
   --  Set the data stored in the instance of EditorLocation

   function Create_Editor_Location
     (Script   : access Scripting_Language_Record'Class;
      Location : Gtk_Text_Iter) return Class_Instance;
   function Create_Editor_Location
     (Script   : access Scripting_Language_Record'Class;
      Buffer   : Source_Buffer;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type) return Class_Instance;
   --  Return an instance of EditorLocation

   procedure Get_Location
     (Iter    : out Gtk_Text_Iter;
      Data    : Callback_Data'Class;
      Arg     : Positive;
      Default : Gtk_Text_Iter;
      Success : out Boolean);
   --  Return the iter stored in the Arg-th parameter.
   --  If no location could be obtain from the arguments, Iter is Set to
   --  Default and Success to False.

   procedure Get_Locations
     (Iter1                : out Gtk_Text_Iter;
      Iter2                : out Gtk_Text_Iter;
      Buffer               : in out Source_Buffer;
      Data                 : in out Callback_Data'Class;
      Arg1                 : Positive;
      Arg2                 : Positive;
      Compensate_Last_Iter : Boolean := True);
   --  Get the two location arguments from Data, defaulting to resp. the
   --  beginning and end of the buffer.
   --  Buffer is reset to null in case of errors, or if it was null when
   --  Get_Locations was called.
   --  If Compensate_Last_Iter is True, then the highest iterator is moved one
   --  additional character. This is used for gtk+ commands that take two
   --  iterators, since they stop to operate just before the last iterator.

   procedure Get_Mark
     (Mark : out Gtk_Text_Mark;
      Data : in out Callback_Data'Class;
      Arg  : Positive);
   --  Return the mark stored in the Arg-th parameter.
   --  Set Mark to null if it wasn't a valid instance

   procedure Get_Buffer
     (Buffer : in out Source_Buffer;
      Data   : in out Callback_Data'Class;
      Arg    : Positive);
   --  Set the Buffer variable appropriately, or null if the buffer could
   --  not be found or is no longer valid.
   --  If the buffer is no longer valid, null is returned and an error msg is
   --  set in Data.

   procedure Get_Overlay
     (Tag        : in out Gtk_Text_Tag;
      Data       : in out Callback_Data'Class;
      Arg        : Positive;
      Allow_Null : Boolean := False);
   --  Get the EditorOverlay stored in Data

   procedure Get_Box
     (Box  : in out Source_Editor_Box;
      Data : in out Callback_Data'Class;
      Arg  : Positive);
   --  Return the view stored in Data

   -------------
   -- Get_Box --
   -------------

   procedure Get_Box
     (Box  : in out Source_Editor_Box;
      Data : in out Callback_Data'Class;
      Arg  : Positive)
   is
      EditorView : constant Class_Type :=
                     New_Class (Get_Kernel (Data), "EditorView");
      Inst       : constant Class_Instance := Nth_Arg (Data, Arg, EditorView);
   begin
      Box := Source_Editor_Box (GObject'(Get_Data (Inst)));
      if Box = null then
         Set_Error_Msg (Data, "No associated view");
      end if;
   end Get_Box;

   ----------------
   -- Get_Buffer --
   ----------------

   procedure Get_Buffer
     (Buffer : in out Source_Buffer;
      Data   : in out Callback_Data'Class;
      Arg    : Positive)
   is
      EditorBuffer : constant Class_Type :=
                       New_Class (Get_Kernel (Data), "EditorBuffer");
      Inst      : constant Class_Instance := Nth_Arg (Data, Arg, EditorBuffer);
   begin
      Buffer := Source_Buffer (GObject'(Get_Data (Inst)));
      if Buffer = null then
         Set_Error_Msg (Data, "No associated buffer");
      end if;
   end Get_Buffer;

   -----------------
   -- Get_Overlay --
   -----------------

   procedure Get_Overlay
     (Tag        : in out Gtk_Text_Tag;
      Data       : in out Callback_Data'Class;
      Arg        : Positive;
      Allow_Null : Boolean := False)
   is
      EditorOverlay : constant Class_Type :=
                        New_Class (Get_Kernel (Data), "EditorOverlay");
      Inst          : Class_Instance;
   begin
      Inst := Nth_Arg (Data, Arg, EditorOverlay, Allow_Null => Allow_Null);
      if Inst = No_Class_Instance then
         Tag := null;
      else
         Tag := Gtk_Text_Tag (GObject'(Get_Data (Inst)));
         if Tag = null and then not Allow_Null then
            Set_Error_Msg (Data, "No associated overlay");
         end if;
      end if;
   end Get_Overlay;

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Iter    : out Gtk_Text_Iter;
      Data    : Callback_Data'Class;
      Arg     : Positive;
      Default : Gtk_Text_Iter;
      Success : out Boolean)
   is
      Class    : constant Class_Type :=
                   New_Class (Get_Kernel (Data), Editor_Location_Class_Name);
      Loc_Inst : Class_Instance;
      Info     : Location_Info_Access;
   begin
      Success := True;
      Loc_Inst := Nth_Arg
        (Data, Arg, Class, Allow_Null => True, Default => No_Class_Instance);

      if Loc_Inst = No_Class_Instance then
         Copy (Source => Default, Dest => Iter);
         Success := False;

      else
         Info := Location_Property_Access
           (Instance_Property'
              (Get_Data (Loc_Inst, Editor_Location_Class_Name))).Location;

         if Info.Buffer = null
           or else not Is_Valid_Position (Info.Buffer, Info.Line, Info.Offset)
         then
            Copy (Source => Default, Dest => Iter);
            Success := False;

         else
            Get_Iter_At_Screen_Position
              (Info.Buffer, Iter,
               Line   => Info.Line,
               Column => Info.Offset);
         end if;
      end if;
   end Get_Location;

   -------------------
   -- Get_Locations --
   -------------------

   procedure Get_Locations
     (Iter1                : out Gtk_Text_Iter;
      Iter2                : out Gtk_Text_Iter;
      Buffer               : in out Source_Buffer;
      Data                 : in out Callback_Data'Class;
      Arg1                 : Positive;
      Arg2                 : Positive;
      Compensate_Last_Iter : Boolean := True)
   is
      Success : Boolean;

      procedure Forward_Iter (Iter : in out Gtk_Text_Iter);
      --  Forward Iter one char

      ------------------
      -- Forward_Iter --
      ------------------

      procedure Forward_Iter (Iter : in out Gtk_Text_Iter) is
         Line, End_Line : Editable_Line_Type;
         Col, End_Col   : Character_Offset_Type;
      begin
         Get_Iter_Position (Buffer, Iter, Line, Col);
         Forward_Position (Buffer, Line, Col, 1, End_Line, End_Col);
         Get_Iter_At_Screen_Position (Buffer, Iter, End_Line, End_Col);
      end Forward_Iter;

   begin
      if Buffer /= null then
         Get_Start_Iter (Buffer, Iter1);
         Get_Location (Iter1, Data, Arg1, Iter1, Success);

         Get_End_Iter (Buffer, Iter2);
         Get_Location (Iter2, Data, Arg2, Iter2, Success);

         if Get_Buffer (Iter1) /= Get_Buffer (Iter2)
           or else Get_Buffer (Iter1) /= Gtk_Text_Buffer (Buffer)
         then
            Set_Error_Msg (Data, -"Locations are not in the correct buffer");
            Buffer := null;

         elsif Compensate_Last_Iter then
            --  All operations that take two iterators stop just before the
            --  second one. This is harder to use in scripts, though, so we
            --  compensate for that here.
            if Compare (Iter1, Iter2) <= 0 then
               --  ??? temporarily commented out, since it breaks ctrl-k in
               --  Emacs mode (F707-004)
               --  if not (Is_End (Iter2) or else Ends_Line (Iter2)) then
               if not Is_End (Iter2) then
                  Forward_Iter (Iter2);
               end if;

            else
               --  ??? temporarily commented out, since it breaks ctrl-k in
               --  Emacs mode (F707-004)
               --  if not (Is_End (Iter1) or else Ends_Line (Iter1)) then
               if not Is_End (Iter1) then
                  Forward_Iter (Iter1);
               end if;
            end if;
         end if;
      end if;
   end Get_Locations;

   ---------------------------
   -- Create_Editor_Overlay --
   ---------------------------

   function Create_Editor_Overlay
     (Script : access Scripting_Language_Record'Class;
      Tag    : Gtk_Text_Tag) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      Inst := Get_Instance (Script, Tag);
      if Inst = No_Class_Instance then
         Inst := New_Instance
           (Script, New_Class (Get_Kernel (Script), "EditorOverlay"));
         Set_Data (Inst, GObject (Tag));
      end if;
      return Inst;
   end Create_Editor_Overlay;

   ------------------------
   -- Create_Editor_Mark --
   ------------------------

   function Create_Editor_Mark
     (Script : access Scripting_Language_Record'Class;
      Mark   : Gtk_Text_Mark) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      Inst := Get_Instance (Script, Mark);
      if Inst = No_Class_Instance then
         Ref (Mark);
         Inst := New_Instance
           (Script, New_Class (Get_Kernel (Script), "EditorMark"));
         Set_Data (Inst, GObject (Mark));
      end if;
      return Inst;
   end Create_Editor_Mark;

   --------------
   -- Get_Mark --
   --------------

   procedure Get_Mark
     (Mark : out Gtk_Text_Mark;
      Data : in out Callback_Data'Class;
      Arg  : Positive)
   is
      EditorMark : constant Class_Type :=
                     New_Class (Get_Kernel (Data), "EditorMark");
      Inst       : constant Class_Instance := Nth_Arg (Data, Arg, EditorMark);
   begin
      Mark := Gtk_Text_Mark (GObject'(Get_Data (Inst)));

      if Mark /= null and then Get_Buffer (Mark) = null then
         --  The buffer was destroyed, so we might as well destroy the mark
         --  reference we have kept in this instance
         Mark := null;
      end if;

      if Mark = null then
         Set_Error_Msg (Data, "No associated mark");
      end if;
   end Get_Mark;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out Location_Property) is
   begin
      --  Make sure we will not get a signal when the buffer is destroyed later
      --  since we are no longer interested in it
      if Property.Location.Buffer /= null then
         Weak_Unref
           (Property.Location.Buffer, On_Buffer_Destroyed_For_Location'Access,
            Data => Convert (Property.Location));
      end if;
      Unchecked_Free (Property.Location);
   end Destroy;

   --------------------------------------
   -- On_Buffer_Destroyed_For_Location --
   --------------------------------------

   procedure On_Buffer_Destroyed_For_Location (Data, Obj : System.Address) is
      pragma Unreferenced (Obj);
      Inf : constant Location_Info_Access := Convert (Data);
   begin
      Inf.Buffer := null;
   end On_Buffer_Destroyed_For_Location;

   -----------------------
   -- Set_Location_Data --
   -----------------------

   procedure Set_Location_Data
     (Inst   : Class_Instance;
      Buffer : Source_Buffer;
      Line   : Editable_Line_Type;
      Offset : Visible_Column_Type)
   is
      Info : constant Location_Info_Access := new Location_Info'
        (Buffer => Buffer, Line => Line, Offset => Offset);
   begin
      Set_Data
        (Inst, Editor_Location_Class_Name,
         Location_Property'(Location => Info));
      Weak_Ref (Buffer, On_Buffer_Destroyed_For_Location'Access,
                Convert (Info));
   end Set_Location_Data;

   ----------------------------
   -- Create_Editor_Location --
   ----------------------------

   function Create_Editor_Location
     (Script   : access Scripting_Language_Record'Class;
      Location : Gtk_Text_Iter) return Class_Instance
   is
      EditorLoc : constant Class_Type :=
                    New_Class
                      (Get_Kernel (Script), Editor_Location_Class_Name);
      Inst      : constant Class_Instance := New_Instance (Script, EditorLoc);
      Line      : Editable_Line_Type;
      Column    : Visible_Column_Type;
   begin
      Get_Iter_Position
        (Source_Buffer (Get_Buffer (Location)), Location, Line, Column);
      Set_Location_Data
        (Inst, Source_Buffer (Get_Buffer (Location)), Line, Column);
      return Inst;
   end Create_Editor_Location;

   ----------------------------
   -- Create_Editor_Location --
   ----------------------------

   function Create_Editor_Location
     (Script : access Scripting_Language_Record'Class;
      Buffer : Source_Buffer;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Class_Instance
   is
      EditorLoc : constant Class_Type :=
                    New_Class
                      (Get_Kernel (Script), Editor_Location_Class_Name);
      Inst      : constant Class_Instance := New_Instance (Script, EditorLoc);
   begin
      Set_Location_Data (Inst, Buffer, Line, Column);
      return Inst;
   end Create_Editor_Location;

   --------------------------
   -- Create_Editor_Buffer --
   --------------------------

   function Create_Editor_Buffer
     (Script : access Scripting_Language_Record'Class;
      Buffer : Source_Buffer) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      if Buffer = null then
         return No_Class_Instance;
      else
         Inst := Get_Instance (Script, Buffer);
         if Inst = No_Class_Instance then
            Inst := New_Instance
              (Script, New_Class (Get_Kernel (Script), "EditorBuffer"));
            Set_Data (Inst, GObject (Buffer));
         end if;
         return Inst;
      end if;
   end Create_Editor_Buffer;

   ------------------------
   -- Create_Editor_View --
   ------------------------

   function Create_Editor_View
     (Script : access Scripting_Language_Record'Class;
      View   : Source_Editor_Box) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      if View = null then
         return No_Class_Instance;

      else
         Inst := Get_Instance (Script, View);
         if Inst = No_Class_Instance then
            Inst := New_Instance
              (Script, New_Class (Get_Kernel (Script), "EditorView"));
            Set_Data (Inst, GObject (View));
         end if;
         return Inst;
      end if;
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
        (Data, Get_Source_Files (Project, Recursive));
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
            File : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel, Use_Source_Path => True);
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
            File : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel, Use_Source_Path => True);
            Child : constant MDI_Child := Find_Editor (Kernel, File);
            Info  : Line_Info_Width_Array_Access;
            Box   : Source_Editor_Box;

            procedure Print_Line_Info (Info : Line_Information_Access);

            procedure Print_Line_Info (Info : Line_Information_Access) is
               Print : Unbounded_String;
            begin
               if Info = null then
                  return;
               end if;

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
                  Print_Line_Info (Info (J).Info);
               end loop;
            end if;
         end;

      elsif Command = "indent" then
         Name_Parameters (Data, Indent_Cmd_Parameters);
         declare
            Current_Line_Only : constant Boolean := Nth_Arg (Data, 1, False);
            Child : constant MDI_Child := Find_Current_Editor (Kernel);
            Box   : Source_Editor_Box;
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

      elsif Command = "delete_mark" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         if Marker /= null then
            Destroy (Marker.all);
            Unchecked_Free (Marker);
         end if;

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
            File   : constant String  := Nth_Arg (Data, 1);
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
         Set_Return_Value (Data, Full_Name (Get_File (Marker)).all);

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
                      " open file : ") & Base_Name (File));
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
                  --  subprogram_name
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
                  & Base_Name (File));
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
                  & Base_Name (File));
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
            File   : constant Virtual_File :=
              Create (Nth_Arg (Data, 1), Kernel);
            Child  : constant MDI_Child := Find_Editor (Kernel, File);
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
                     Add_Blank_Lines
                       (Get_Buffer (Box),
                        Editable_Line_Type (Line),
                        Highlight_Category, "", Number));
                  Set_Return_Value (Data, Get_Id (Marker));
               end if;
            else
               Set_Error_Msg (Data, -"file not open");
            end if;
         end;

      elsif Command = "remove_blank_lines" then
         Marker := Find_Mark (Nth_Arg (Data, 1));
         declare
            Child       : MDI_Child;
            Number      : Integer := 0;
            Box         : Source_Editor_Box;
         begin
            if Number_Of_Arguments (Data) >= 3 then
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
                  Src_Editor_Buffer.Line_Information.Fold_Block
                    (Get_Buffer (Box), Editable_Line_Type (Line));
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
            Box         : Source_Editor_Box;
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
               Trace (Me, "Editor not found: " & Full_Name (File).all);
            end if;
         end;

      elsif Command = "set_title" then
         Name_Parameters (Data, Set_Title_Cmd_Parameters);

         declare
            File  : constant Virtual_File :=
                      Create (Nth_Arg (Data, 1), Kernel);
            Title : constant String := Nth_Arg (Data, 2);
            Child : MDI_Child;
         begin
            Child := Find_Editor (Kernel, File);

            if Child /= null then
               Set_Title (Child, Full_Name (File).all, Title);
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
      Buffer      : Source_Buffer;
      Child       : MDI_Child;
      Box         : Source_Editor_Box;
      File        : Virtual_File;
      File_Inst   : Class_Instance;
      Mark        : Gtk_Text_Mark;
      Success     : Boolean;
      Tag         : Gtk_Text_Tag;
      Iter, Iter2 : aliased Gtk_Text_Iter;
      Force       : Boolean;

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
         Force := Nth_Arg (Data, 2, Default => False);

         if File_Inst = No_Class_Instance then
            File := GNATCOLL.VFS.No_File;
         else
            File := Get_Data (File_Inst);
         end if;

         if File /= GNATCOLL.VFS.No_File then
            Child := Find_Editor (Kernel, File);
         else
            Child := Find_Current_Editor (Kernel);
         end if;

         if Child = null then
            if Nth_Arg (Data, 3, Default => True) then
               Box := Open_File (Get_Kernel (Data), File,
                                 Line => 0, Column => 0, Column_End => 0);
            else
               Set_Return_Value (Data, No_Class_Instance);
               return;
            end if;
         else
            Box := Get_Source_Box_From_MDI (Child);

            if File /= GNATCOLL.VFS.No_File and Force then
               Check_Timestamp_And_Reload (Box, False, True);
            end if;
         end if;

         Set_Return_Value
           (Data, Create_Editor_Buffer
              (Get_Script (Data), Get_Buffer (Box)));

      elsif Command = "list" then
         declare
            Iter : Child_Iterator := First_Child (Get_MDI (Kernel));
            Child_Count : Natural := 0;
         begin
            while Get (Iter) /= null loop
               Child_Count := Child_Count + 1;
               Next (Iter);
            end loop;

            declare
               Buffers : array (1 .. Child_Count) of Source_Buffer;
               Index   : Integer := Buffers'First - 1;
               Found   : Boolean;
            begin
               Iter := First_Child (Get_MDI (Kernel));
               Set_Return_Value_As_List (Data);
               while Get (Iter) /= null loop
                  if Is_Source_Box (Get (Iter)) then
                     Buffer := Get_Buffer
                       (Get_Source_Box_From_MDI (Get (Iter)));
                     Found := False;
                     for J in Buffers'First .. Index loop
                        if Buffers (J) = Buffer then
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Index := Index + 1;
                        Buffers (Index) := Buffer;
                        Set_Return_Value
                          (Data, Create_Editor_Buffer
                             (Get_Script (Data), Buffer));
                     end if;
                  end if;
                  Next (Iter);
               end loop;
            end;
         end;

      elsif Command = "file" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Set_Return_Value
              (Data, Create_File (Get_Script (Data), Get_Filename (Buffer)));
         end if;

      elsif Command = "current_view" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            declare
               File : GNATCOLL.VFS.Virtual_File := Get_Filename (Buffer);
            begin
               if File = GNATCOLL.VFS.No_File then
                  File := Get_File_Identifier (Buffer);
               end if;

               Child := Find_Editor (Get_Kernel (Data), File);
            end;

            if Child = null then
               Set_Error_Msg (Data, -"Editor not found");
            else
               Set_Return_Value
                 (Data, Create_Editor_View
                    (Get_Script (Data),
                     Source_Editor_Box (Get_Widget (Child))));
            end if;
         end if;

      elsif Command = "views" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Set_Return_Value_As_List (Data);
            declare
               Views : constant Views_Array := Get_Views (Buffer);
            begin
               for V in Views'Range loop
                  Set_Return_Value
                    (Data, Create_Editor_View (Get_Script (Data), Views (V)));
               end loop;
            end;
         end if;

      elsif Command = "close" then
         Name_Parameters (Data, (1 => Force_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            declare
               Views : constant Views_Array := Get_Views (Buffer);
            begin
               for V in Views'Range loop
                  Close (Get_MDI (Get_Kernel (Data)), Views (V),
                         Force => Nth_Arg (Data, 2, False));
               end loop;
            end;
         end if;

      elsif Command = "save" then
         Name_Parameters (Data, (1 => Interactive_Cst'Access,
                                 2 => File_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            File_Inst := Nth_Arg
              (Data, 3, Get_File_Class (Kernel),
               Default => No_Class_Instance, Allow_Null => True);
            if File_Inst = No_Class_Instance then
               File := Get_Filename (Buffer);
               Success := Save_MDI_Children
                 (Get_Kernel (Data),
                  Children => (1 => Find_Editor
                                 (Get_Kernel (Data), Get_Filename (Buffer))),
                  Force    => not Nth_Arg (Data, 2, False));
            else
               File := Get_Data (File_Inst);
               Save_To_File (Buffer,
                             Filename => File,
                             Success  => Success);
            end if;
         end if;

      elsif Command = "characters_count" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            --  ??? This is incompatible with blank/folded lines
            Set_Return_Value (Data, Integer (Get_Char_Count (Buffer)));
         end if;

      elsif Command = "lines_count" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Get_End_Iter (Buffer, Iter);
            declare
               Line   : Editable_Line_Type;
               Column : Visible_Column_Type;
            begin
               Get_Iter_Position (Buffer, Iter, Line, Column);
               Set_Return_Value (Data, Integer (Line));
            end;
         end if;

      elsif Command = "select" then
         Name_Parameters (Data, (1 => Start_Cst'Access,
                                 2 => End_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Locations (Iter, Iter2, Buffer, Data, 2, 3, False);
         if Buffer /= null then
            Select_Region
              (Buffer,
               Cursor_Iter  => Iter2,
               Bound_Iter   => Iter);
         end if;

      elsif Command = "unselect" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Select_Region
              (Buffer,
               Start_Line   => Gint'(0),
               Start_Column => 0,
               End_Line     => 0,
               End_Column   => 0);
         end if;

      elsif Command = "selection_start"
        or else Command = "selection_end"
      then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            declare
               Mark, Cursor   : Gtk_Text_Mark;
               IMark, ICursor : Gtk_Text_Iter;
               Mark_First     : Boolean;
            begin
               Mark   := Get_Selection_Bound (Buffer);
               Cursor := Get_Insert (Buffer);
               Get_Iter_At_Mark (Buffer, IMark, Mark);
               Get_Iter_At_Mark (Buffer, ICursor, Cursor);

               if Command = "selection_start" then
                  Mark_First := Compare (IMark, ICursor) <= 0;
               else
                  Mark_First := Compare (IMark, ICursor) > 0;
               end if;

               if Mark_First then
                  Set_Return_Value
                    (Data, Create_Editor_Location
                       (Get_Script (Data), IMark));
               else
                  Set_Return_Value
                    (Data, Create_Editor_Location
                       (Get_Script (Data), ICursor));
               end if;
            end;
         end if;

      elsif Command = "beginning_of_buffer" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Get_Start_Iter (Buffer, Iter);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         end if;

      elsif Command = "end_of_buffer" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Get_End_Iter (Buffer, Iter);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         end if;

      elsif Command = "is_modified" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Set_Return_Value (Data, Get_Status (Buffer) = Modified);
         end if;

      elsif Command = "get_chars" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Get_Locations (Iter, Iter2, Buffer, Data, 2, 3);

            declare
               Begin_Line : Editable_Line_Type;
               Begin_Col  : Character_Offset_Type;
               End_Line   : Editable_Line_Type;
               End_Col    : Character_Offset_Type;
            begin
               Get_Iter_Position (Buffer, Iter, Begin_Line, Begin_Col);
               Get_Iter_Position (Buffer, Iter2, End_Line, End_Col);
               Set_Return_Value
                 (Data,
                  Get_Text (Buffer, Begin_Line, Begin_Col, End_Line, End_Col));
            end;
         end if;

      elsif Command = "insert" then
         Name_Parameters
           (Data, (1 => Location_Cst'Access, 2 => Text_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Get_Location (Iter, Data, 2, Iter, Success);

            if not Success then
               return;
            elsif Get_Buffer (Iter) = Gtk_Text_Buffer (Buffer) then
               if Get_Writable (Buffer) then
                  Insert (Buffer, Iter, Nth_Arg (Data, 3));
                  End_Action (Buffer);
               else
                  Set_Error_Msg (Data, -"Buffer is not writable");
               end if;
            else
               Set_Error_Msg (Data, -"Location is not in the same buffer");
            end if;
         end if;

      elsif Command = "delete" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Locations (Iter, Iter2, Buffer, Data, 2, 3);
         if Buffer /= null then
            if Get_Writable (Buffer) then
               Delete (Buffer, Iter, Iter2);
               End_Action (Buffer);
            else
               Set_Error_Msg (Data, -"Buffer is not writable");
            end if;
         end if;

      elsif Command = "copy"
        or else Command = "cut"
      then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access,
                                 3 => Append_Cst'Access));
         declare
            Append : constant Boolean := Nth_Arg (Data, 4, False);
         begin
            Get_Buffer (Buffer, Data, 1);
            Get_Locations (Iter, Iter2, Buffer, Data, 2, 3);
            if Buffer /= null then
               External_End_Action (Buffer);
               Select_Range (Buffer, Iter, Iter2);
               if Command = "copy" then
                  Copy_Clipboard (Get_Clipboard (Kernel), Buffer);
               else
                  if Get_Writable (Buffer) then
                     Cut_Clipboard (Get_Clipboard (Kernel), Buffer);
                     End_Action (Buffer);
                  else
                     Set_Error_Msg (Data, -"Buffer is not writable");
                  end if;
               end if;
               if Append then
                  Merge_Clipboard (Get_Clipboard (Kernel), 1, 2);
               end if;
            end if;
         end;

      elsif Command = "paste" then
         Name_Parameters (Data, (1 => Location_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Location (Iter, Data, 2, Iter, Success);
         if not Success then
            return;
         elsif Get_Buffer (Iter) /= Gtk_Text_Buffer (Buffer) then
            Set_Error_Msg (Data, -"Location is not in the same buffer");
         elsif Buffer /= null then
            if Get_Writable (Buffer) then
               Place_Cursor (Buffer, Iter);
               Paste_Clipboard (Get_Clipboard (Kernel), Buffer);
               End_Action (Buffer);
            else
               Set_Error_Msg (Data, -"Buffer is not writable");
            end if;
         end if;

      elsif Command = "blocks_fold" then
         Name_Parameters (Data, (1 => Line_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Fold_All (Buffer);
         end if;

      elsif Command = "blocks_unfold" then
         Name_Parameters (Data, (1 => Line_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Unfold_All (Buffer);
         end if;

      elsif Command = "indent" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Locations (Iter, Iter2, Buffer, Data, 2, 3);
         if Buffer /= null then
            if not Do_Indentation (Buffer, Iter, Iter2) then
               Set_Error_Msg (Data, -"Error while indenting");
            end if;
         end if;
         End_Action (Buffer);

      elsif Command = "refill" then
         Name_Parameters (Data, (1 => From_Cst'Access, 2 => To_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Locations (Iter, Iter2, Buffer, Data, 2, 3);
         if Buffer /= null then
            if Get_Writable (Buffer) then
               Select_Region
                 (Buffer,
                  Cursor_Iter  => Iter2,
                  Bound_Iter   => Iter);
               if not Do_Refill (Buffer) then
                  Set_Error_Msg (Data, -"Error while refilling buffer");
               end if;
            else
               Set_Error_Msg (Data, -"Buffer is not writable");
            end if;
         end if;
         End_Action (Buffer);

      elsif Command = "get_mark" then
         Name_Parameters (Data, (2 => Name_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Mark := Get_Mark (Buffer, Nth_Arg (Data, 2));
            if Mark /= null then
               Set_Return_Value
                 (Data, Create_Editor_Mark (Get_Script (Data), Mark));
            else
               Set_Error_Msg (Data, -"No such mark");
            end if;
         end if;

      elsif Command = "create_overlay" then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            declare
               Name : constant String := Nth_Arg (Data, 2, "");
            begin
               if Name /= "" then
                  Tag := Lookup (Get_Tag_Table (Buffer), Name);
               end if;

               if Tag = null then
                  Gtk_New (Tag, Name);
                  Add (Get_Tag_Table (Buffer), Tag);
               end if;

               Set_Return_Value
                 (Data, Create_Editor_Overlay (Get_Script (Data), Tag));
            end;
         end if;

      elsif Command = "apply_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access,
                                 2 => From_Cst'Access,
                                 3 => To_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Locations (Iter, Iter2, Buffer, Data, 3, 4);
         if Buffer /= null then
            Get_Overlay (Tag, Data, 2);
            if Tag /= null then
               Apply_Tag (Buffer, Tag, Iter, Iter2);
            end if;
         end if;

      elsif Command = "remove_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access,
                                 2 => From_Cst'Access,
                                 3 => To_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         Get_Locations (Iter, Iter2, Buffer, Data, 3, 4);
         if Buffer /= null then
            Get_Overlay (Tag, Data, 2);
            if Tag /= null then
               Remove_Tag (Buffer, Tag, Iter, Iter2);
            end if;
         end if;

      elsif Command = "start_undo_group" then
         Get_Buffer (Buffer, Data, 1);
         End_Action (Buffer);
         Start_Group (Get_Command_Queue (Buffer));

      elsif Command = "finish_undo_group" then
         Get_Buffer (Buffer, Data, 1);
         End_Action (Buffer);
         End_Group (Get_Command_Queue (Buffer));

      elsif Command = "undo" then
         Get_Buffer (Buffer, Data, 1);
         if Get_Writable (Buffer) then
            Undo (Buffer);
         else
            Set_Error_Msg (Data, -"Buffer is not writable");
         end if;

      elsif Command = "redo" then
         Get_Buffer (Buffer, Data, 1);
         if Get_Writable (Buffer) then
            Redo (Buffer);
         else
            Set_Error_Msg (Data, -"Buffer is not writable");
         end if;

      elsif Command = "set_read_only" then
         Name_Parameters (Data, (1 => Read_Only_Cst'Access));
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Set_Writable
              (Buffer, not Nth_Arg (Data, 2, True), Explicit => True);
         end if;

      elsif Command = "is_read_only" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            Set_Return_Value (Data, not Get_Writable (Buffer));
         end if;

      elsif Command = "add_special_line" then
         Get_Buffer (Buffer, Data, 1);
         if Buffer /= null then
            declare
               Line               : constant Integer := Nth_Arg (Data, 2);
               Text               : constant String  := Nth_Arg (Data, 3);
               Highlight_Category : Natural := 0;
               Style              : Style_Access;
            begin
               if Number_Of_Arguments (Data) >= 4 then
                  Style := Get_Or_Create_Style
                    (Kernel, Nth_Arg (Data, 4), False);

                  if Style = null then
                     Set_Error_Msg
                       (Data, -"No such style: " & Nth_Arg (Data, 4));
                     return;
                  else
                     Highlight_Category :=
                       Line_Highlighting.Lookup_Category (Style);
                  end if;
               end if;

               Mark := Add_Blank_Lines
                 (Buffer,
                  Editable_Line_Type (Line),
                  Highlight_Category,
                  Text,
                  1);

               Set_Return_Value
                 (Data, Create_Editor_Mark (Get_Script (Data), Mark));
            end;
         end if;

      else
         Set_Error_Msg (Data, -"Command not implemented: " & Command);
      end if;
   end Buffer_Cmds;

   -------------------
   -- Location_Cmds --
   -------------------

   procedure Location_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      EditorLoc  : constant Class_Type :=
                     New_Class (Get_Kernel (Data), Editor_Location_Class_Name);
      Buffer      : Source_Buffer;
      Inst        : Class_Instance;
      Iter, Iter2 : Gtk_Text_Iter;
      Mark        : Gtk_Text_Mark;
      Success     : Boolean;
      Success2    : Boolean;
      Count       : Gint;
      Block       : Block_Record;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Buffer_Cst'Access,
                                 2 => Line_Cst'Access,
                                 3 => Col_Cst'Access));
         Inst := Nth_Arg (Data, 1, EditorLoc);
         Get_Buffer (Buffer, Data, 2);
         Set_Location_Data
           (Inst,
            Buffer,
            Editable_Line_Type (Integer'Max (1, Nth_Arg (Data, 3))),
            Visible_Column_Type (Integer'Max (1, Nth_Arg (Data, 4))));

      elsif Command = Comparison_Method then
         Get_Location (Iter, Data, 1, Iter, Success);
         Get_Location (Iter2, Data, 2, Iter2, Success2);
         if Success /= Success2
           or else not Success
           or else Get_Buffer (Iter) /= Get_Buffer (Iter2)
         then
            Set_Error_Msg (Data, -"EditorLocation not in the same buffer");
         else
            Set_Return_Value (Data, Integer (Compare (Iter, Iter2)));
         end if;

      elsif Command = "line" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            declare
               Line : Editable_Line_Type;
               Col  : Character_Offset_Type;
            begin
               Get_Iter_Position
                 (Source_Buffer (Get_Buffer (Iter)), Iter, Line, Col);
               Set_Return_Value (Data, Integer (Line));
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "column" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            declare
               Line : Editable_Line_Type;
               Col  : Visible_Column_Type;
            begin
               Get_Iter_Position
                 (Source_Buffer (Get_Buffer (Iter)), Iter, Line, Col);
               Set_Return_Value (Data, Integer (Col));
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "offset" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Set_Return_Value (Data, Integer (Get_Offset (Iter)));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "buffer" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Set_Return_Value
              (Data, Create_Editor_Buffer (Get_Script (Data),
               Source_Buffer (Get_Buffer (Iter))));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "beginning_of_line" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Set_Line_Offset (Iter, 0);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "end_of_line" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            if not Ends_Line (Iter) then
               Forward_To_Line_End (Iter, Success);
            end if;
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

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
            Context : Current_File_Context_Access :=
              Current_File_Context_Access
                (Current_File_Factory
                     (Kernel          => Get_Kernel (Data),
                      All_Occurrences => False,
                      Scope           => Search_Scope'Value
                        (Nth_Arg (Data, 7, "Whole"))));
            Found       : Boolean;
            Match_From  : Gtk_Text_Iter;
            Match_Up_To : Gtk_Text_Iter;
         begin
            Set_Context
              (Context,
               Look_For => Nth_Arg (Data, 2),
               Options  => (Case_Sensitive => Nth_Arg (Data, 4, False),
                            Whole_Word     => Nth_Arg (Data, 6, False),
                            Regexp         => Nth_Arg (Data, 5, False)));
            Get_Location (Iter, Data, 1, Iter, Success);
            if Success then
               Search_In_Editor
                 (Context           => Context,
                  Start_At          => Iter,
                  Kernel            => Get_Kernel (Data),
                  Search_Backward   => Nth_Arg (Data, 3, False),
                  Dialog_On_Failure => Nth_Arg (Data, 8, True),
                  Match_From        => Match_From,
                  Match_Up_To       => Match_Up_To,
                  Found             => Found);
               if Found then
                  Set_Return_Value_As_List (Data);
                  Set_Return_Value
                    (Data,
                     Create_Editor_Location (Get_Script (Data), Match_From));
                  Set_Return_Value
                    (Data,
                     Create_Editor_Location (Get_Script (Data), Match_Up_To));
               end if;
            else
               Set_Error_Msg (Data, -"Invalid location");
            end if;

            Free (Search_Context_Access (Context));
         end;

      elsif Command = "forward_char" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            declare
               Chars : constant Gint := Gint (Integer'(Nth_Arg (Data, 2, 1)));
            begin
               if Chars >= 0 then
                  Forward_Chars (Iter, Chars, Success);
               else
                  Backward_Chars (Iter, -Chars, Success);
               end if;
            end;

            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = Addition_Method then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Forward_Chars (Iter, Gint (Integer'(Nth_Arg (Data, 2))), Success);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = Substraction_Method then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            begin
               --  The second argument is an integer ?
               Count := Gint (Integer'(Nth_Arg (Data, 2)));
               Forward_Chars (Iter, -Count, Success);
               Set_Return_Value
                 (Data, Create_Editor_Location (Get_Script (Data), Iter));
            exception
               when Invalid_Parameter =>
                  --  The second argument is another location ?
                  Get_Location (Iter2, Data, 2, Iter, Success);
                  if Success then
                     if Get_Buffer (Iter2) /= Get_Buffer (Iter) then
                        Set_Error_Msg
                          (Data, -"Locations not in the same buffer");
                     else
                        Set_Return_Value
                          (Data,
                           Integer (Get_Offset (Iter) - Get_Offset (Iter2)));
                     end if;
                  else
                     Set_Error_Msg (Data, -"Invalid location");
                  end if;
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "forward_word" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Forward_Word_Ends (Iter,
                               Count  => Gint (Integer'(Nth_Arg (Data, 2, 1))),
                               Result => Success);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "starts_word" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Set_Return_Value (Data, Src_Editor_Buffer.Starts_Word (Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "ends_word" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Set_Return_Value (Data, Src_Editor_Buffer.Ends_Word (Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "forward_line" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Forward_Lines (Iter,
                           Count  => Gint (Integer'(Nth_Arg (Data, 2, 1))),
                           Result => Success);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "create_mark" then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);

         if Success then
            if Nth_Arg (Data, 2, "") /= "" then
               Mark := Get_Mark (Get_Buffer (Iter), Nth_Arg (Data, 2));
            end if;

            if Mark = null then
               Mark := Create_Mark
                 (Get_Buffer (Iter),
                  Mark_Name => Nth_Arg (Data, 2, ""),
                  Where     => Iter);
            else
               Move_Mark (Get_Buffer (Iter), Mark, Where => Iter);
            end if;

            Set_Return_Value
              (Data, Create_Editor_Mark (Get_Script (Data), Mark));
         else
            Set_Error_Msg (Data, "Invalid location");
         end if;

      elsif Command = "get_char" then
         declare
            Unichar : Gunichar;
            Buffer  : String (1 .. 6);
            Last    : Natural;
         begin
            Get_Location (Iter, Data, 1, Iter, Success);
            if Success then
               Unichar := Get_Char (Iter);
               if Unichar = 0 then
                  Set_Error_Msg (Data, "Invalid buffer position");
               else
                  Unichar_To_UTF8 (Unichar, Buffer, Last);
                  Set_Return_Value (Data, Buffer (1 .. Last));
               end if;
            else
               Set_Error_Msg (Data, -"Invalid location");
            end if;
         end;

      elsif Command = "block_fold" then
         Get_Location (Iter, Data, 1, Iter, Success);

         if Success then
            declare
               Buffer : constant Source_Buffer :=
                          Source_Buffer (Get_Buffer (Iter));
            begin
               Fold_Block
                 (Buffer,
                  Get_Editable_Line (Buffer,
                    Buffer_Line_Type (Get_Line (Iter) + 1)));
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "block_unfold" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Buffer := Source_Buffer (Get_Buffer (Iter));
            Unfold_Line
              (Buffer,
               Get_Editable_Line (Buffer,
                 Buffer_Line_Type (Get_Line (Iter) + 1)));
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command'Length > 6
        and then Command (Command'First .. Command'First + 5) = "block_"
      then
         Get_Location (Iter, Data, 1, Iter, Success);

         if Success then
            declare
               Line  : constant Buffer_Line_Type :=
                         Buffer_Line_Type (Get_Line (Iter) + 1);
               Block : constant Block_Record :=
                         Get_Block (Source_Buffer (Get_Buffer (Iter)), Line);
               Iter2 : Gtk_Text_Iter;
            begin
               if Command = "block_end_line" then
                  Set_Return_Value (Data, Integer (Block.Last_Line));

               elsif Command = "block_start_line" then
                  Set_Return_Value (Data, Integer (Block.First_Line));

               elsif Command = "block_level" then
                  Set_Return_Value (Data, Block.Indentation_Level);

               elsif Command = "block_start" then
                  Set_Return_Value
                    (Data, Create_Editor_Location
                       (Get_Script (Data),
                        Source_Buffer (Get_Buffer (Iter)),
                        Line   => Block.First_Line,
                        Column => 1));

               elsif Command = "block_end" then
                  Get_Iter_At_Line_Offset
                    (Source_Buffer (Get_Buffer (Iter)), Iter2,
                     Line_Number => Gint (Block.Last_Line),
                     Char_Offset => 1);
                  Forward_Lines
                    (Iter2,
                     Count  => -1,
                     Result => Success);
                  Forward_To_Line_End (Iter2, Success);
                  Set_Return_Value
                    (Data, Create_Editor_Location
                       (Get_Script (Data), Iter2));

               elsif Command = "block_type" then
                  Set_Return_Value
                    (Data, Language_Category'Image (Block.Block_Type));

               elsif Command = "block_name" then
                  if Block.Name = null then
                     Set_Return_Value (Data, "");
                  else
                     Set_Return_Value (Data, Block.Name.all);
                  end if;
               end if;
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "subprogram_name" then
         Get_Location (Iter, Data, 1, Iter, Success);

         if Success then
            Buffer := Source_Buffer (Get_Buffer (Iter));
            Block := Get_Subprogram_Block
              (Buffer,
               Get_Editable_Line (Buffer,
                 Buffer_Line_Type (Get_Line (Iter) + 1)));
            if Block.Name = null then
               Set_Return_Value (Data, "");
            else
               Set_Return_Value (Data, Block.Name.all);
            end if;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "get_overlays" then
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            Set_Return_Value_As_List (Data);
            declare
               use Gtk.Text_Tag.Text_Tag_List;
               List     : GSlist := Get_Tags (Iter);
               Iterator : GSlist := List;
            begin
               while Iterator /= Null_List loop
                  Set_Return_Value
                    (Data, Create_Editor_Overlay
                       (Get_Script (Data), Get_Data (Iterator)));
                  Iterator := Next (Iterator);
               end loop;
               Free (List);
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "has_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            declare
               Tag : Gtk_Text_Tag;
            begin
               Get_Overlay (Tag, Data, 2);
               if Tag /= null then
                  Set_Return_Value (Data, Has_Tag (Iter, Tag));
               end if;
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "forward_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            declare
               Tag : Gtk_Text_Tag;
            begin
               Get_Overlay (Tag, Data, 2, Allow_Null => True);
               Forward_To_Tag_Toggle (Iter, Tag, Success);
               Set_Return_Value
                 (Data, Create_Editor_Location (Get_Script (Data), Iter));
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;

      elsif Command = "backward_overlay" then
         Name_Parameters (Data, (1 => Overlay_Cst'Access));
         Get_Location (Iter, Data, 1, Iter, Success);
         if Success then
            declare
               Tag : Gtk_Text_Tag;
            begin
               Get_Overlay (Tag, Data, 2, Allow_Null => True);
               Backward_To_Tag_Toggle (Iter, Tag, Success);
               Set_Return_Value
                 (Data, Create_Editor_Location (Get_Script (Data), Iter));
            end;
         else
            Set_Error_Msg (Data, -"Invalid location");
         end if;
      end if;
   end Location_Cmds;

   ---------------
   -- Mark_Cmds --
   ---------------

   procedure Mark_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Mark    : Gtk_Text_Mark;
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, "Cannot create an EditorMark directly");

      elsif Command = Destructor_Method then
         Get_Mark (Mark, Data, 1);
         if Mark /= null
           and then Get_Name (Mark) = ""
         then
            --  Do not delete named marks, since we can still access them
            --  through get_mark() anyway
            Trace (Me, "Deleting unnamed mark");
            Delete_Mark (Get_Buffer (Mark), Mark);
         end if;

      elsif Command = "delete" then
         Get_Mark (Mark, Data, 1);
         if Mark /= null then
            Trace (Me, "Deleting mark");
            Delete_Mark (Get_Buffer (Mark), Mark);

         end if;
      elsif Command = "location" then
         Get_Mark (Mark, Data, 1);
         if Mark /= null then
            Get_Iter_At_Mark (Get_Buffer (Mark), Iter, Mark);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         end if;

      elsif Command = "move" then
         Name_Parameters (Data, (1 => Location_Cst'Access));
         Get_Mark (Mark, Data, 1);
         if Mark /= null then
            Get_Location (Iter, Data, 2, Iter, Success);
            if Success then
               Move_Mark (Get_Buffer (Mark), Mark, Iter);
            else
               Set_Error_Msg (Data, -"Invalid location");
            end if;
         end if;
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
      Box        : Source_Editor_Box;
      Buffer     : Source_Buffer;
      Iter       : Gtk_Text_Iter;
      Success    : Boolean;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Buffer_Cst'Access));
         Inst := Nth_Arg (Data, 1, EditorView);
         Get_Buffer (Buffer, Data, 2);
         if Buffer /= null then
            declare
               Views : constant Views_Array := Get_Views (Buffer);
            begin
               Box := New_View (Get_Kernel (Data), Views (Views'First));
               Set_Data (Inst, GObject (Box));
            end;
         end if;

      elsif Command = "buffer" then
         Get_Box (Box, Data, 1);
         if Box /= null then
            Set_Return_Value
              (Data, Create_Editor_Buffer
                 (Get_Script (Data), Get_Buffer (Box)));
         end if;

      elsif Command = "set_read_only" then
         Name_Parameters (Data, (1 => Read_Only_Cst'Access));
         Get_Box (Box, Data, 1);
         if Box /= null then
            Set_Writable (Box, not Nth_Arg (Data, 2, True));
         end if;

      elsif Command = "is_read_only" then
         Get_Box (Box, Data, 1);
         if Box /= null then
            Set_Return_Value (Data, not Get_Writable (Get_Buffer (Box)));
         end if;

      elsif Command = "center" then
         Get_Box (Box, Data, 1);
         if Box /= null then
            Get_Cursor_Position (Get_View (Box), Iter);
            Get_Location (Iter, Data, 2, Iter, Success);

            if Success then
               declare
                  M : constant Gtk_Text_Mark :=
                        Create_Mark (Get_Buffer (Box), "", Iter);
               begin
                  Scroll_To_Mark
                    (Get_View (Box),
                     Mark          => M,
                     Within_Margin => 0.2,
                     Use_Align     => False,
                     Xalign        => 0.5,
                     Yalign        => 0.5);
                  Delete_Mark (Get_Buffer (Box), M);
               end;
            else
               Set_Error_Msg (Data, -"Invalid location");
            end if;
         end if;

      elsif Command = "goto" then
         Get_Box (Box, Data, 1);
         if Box /= null then
            Get_Location (Iter, Data, 2, Iter, Success);

            if Success then
               declare
                  Line : Editable_Line_Type;
                  Col  : Character_Offset_Type;
               begin
                  Get_Iter_Position (Get_Buffer (Box), Iter, Line, Col);

                  Set_Cursor_Location
                    (Box,
                     Line        => Line,
                     Column      => Col,
                     Force_Focus => False);
               end;

            end if;
         end if;

      elsif Command = "cursor" then
         Get_Box (Box, Data, 1);
         if Box /= null then
            Get_Cursor_Position (Get_View (Box), Iter);
            Set_Return_Value
              (Data, Create_Editor_Location (Get_Script (Data), Iter));
         end if;
      end if;
   end View_Cmds;

   ------------------
   -- Overlay_Cmds --
   ------------------

   procedure Overlay_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Tag : Gtk_Text_Tag;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data,
            -("Cannot create instances directly, "
              & " use EditorBuffer.create_overlay()"));

      elsif Command = "name" then
         Get_Overlay (Tag, Data, 1);
         if Tag /= null then
            Set_Return_Value
              (Data, Get_Property (Tag, Gtk.Text_Tag.Name_Property));
         end if;

      elsif Command = "get_property" then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Get_Overlay (Tag, Data, 1);
         if Tag /= null then
            declare
               Name : constant String := Nth_Arg (Data, 2);
               Color : Gdk_Color;
               W     : Weight;
               S     : Style;
            begin
               if Name = "foreground" then
                  Color := Get_Property (Tag, Foreground_Gdk_Property);
                  Set_Return_Value (Data, To_String (Color));

               elsif Name = "background" then
                  Color := Get_Property (Tag, Background_Gdk_Property);
                  Set_Return_Value (Data, To_String (Color));

               elsif Name = "font" then
                  Set_Return_Value
                    (Data, Get_Property (Tag, Font_Property));

               elsif Name = "weight" then
                  W := Get_Property (Tag, Weight_Property);
                  case W is
                     when Pango_Weight_Ultralight .. Pango_Weight_Light =>
                        Set_Return_Value (Data, "light");
                     when Pango_Weight_Normal .. Pango_Weight_Medium =>
                        Set_Return_Value (Data, "normal");
                     when others =>
                        Set_Return_Value (Data, "bold");
                  end case;

               elsif Name = "style" then
                  S := Get_Property (Tag, Gtk.Text_Tag.Style_Property);
                  case S is
                     when Pango_Style_Normal =>
                        Set_Return_Value (Data, "normal");
                     when Pango_Style_Oblique =>
                        Set_Return_Value (Data, "oblique");
                     when Pango_Style_Italic =>
                        Set_Return_Value (Data, "italic");
                  end case;

               elsif Name = "editable" then
                  Set_Return_Value
                    (Data, Get_Property (Tag, Gtk.Text_Tag.Editable_Property));

               else
                  Set_Error_Msg (Data, -"Invalid property");
               end if;
            end;
         end if;

      elsif Command = "set_property" then
         Name_Parameters (Data, (1 => Name_Cst'Access, 2 => Value_Cst'Access));
         Get_Overlay (Tag, Data, 1);
         if Tag /= null then
            declare
               Name : constant String := Nth_Arg (Data, 2);
            begin
               if Name = "foreground" then
                  Set_Property
                    (Tag, Foreground_Property, String'(Nth_Arg (Data, 3)));

               elsif Name = "background" then
                  Set_Property
                    (Tag, Background_Property, String'(Nth_Arg (Data, 3)));

               elsif Name = "font" then
                  Set_Property
                    (Tag, Font_Property, String'(Nth_Arg (Data, 3)));

               elsif Name = "weight" then
                  declare
                     Value : constant String := Nth_Arg (Data, 3);
                  begin
                     if Value = "light" then
                        Set_Property
                          (Tag, Weight_Property, Pango_Weight_Light);
                     elsif Value = "normal" then
                        Set_Property
                          (Tag, Weight_Property, Pango_Weight_Normal);
                     elsif Value = "bold" then
                        Set_Property
                          (Tag, Weight_Property, Pango_Weight_Bold);
                     else
                        Set_Error_Msg
                          (Data, -"Invalid weight: use light, normal or bold");
                     end if;
                  end;

               elsif Name = "style" then
                  declare
                     Value : constant String := Nth_Arg (Data, 3);
                  begin
                     if Value = "normal" then
                        Set_Property
                          (Tag, Gtk.Text_Tag.Style_Property,
                           Pango_Style_Normal);
                     elsif Value = "oblique" then
                        Set_Property
                          (Tag, Gtk.Text_Tag.Style_Property,
                           Pango_Style_Oblique);
                     elsif Value = "italic" then
                        Set_Property
                          (Tag, Gtk.Text_Tag.Style_Property,
                           Pango_Style_Italic);
                     else
                        Set_Error_Msg
                          (Data,
                           -"Invalid style, use normal, oblique, italic");
                     end if;
                  end;

               elsif Name = "editable" then
                  Set_Property
                    (Tag, Gtk.Text_Tag.Editable_Property,
                     Boolean'(Nth_Arg (Data, 3)));

               else
                  Set_Error_Msg (Data, -"Invalid property");
               end if;
            end;
         end if;

      end if;
   end Overlay_Cmds;

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
        (Kernel, "location", 0, 0, Mark_Cmds'Access, EditorMark);
      Register_Command (Kernel, "move", 1, 1, Mark_Cmds'Access, EditorMark);

      --  EditorBuffer

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "get", 0, 3, Buffer_Cmds'Access, EditorBuffer, True);
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
        (Kernel, "add_special_line", 2, 3, Buffer_Cmds'Access, EditorBuffer);

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
      Register_Command (Kernel, "goto", 1, 1, View_Cmds'Access, EditorView);
      Register_Command (Kernel, "cursor", 0, 0, View_Cmds'Access, EditorView);

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
        (Kernel, "set_title", 2, 2, Edit_Command_Handler'Access,
         Editor_Class, True);
   end Register_Commands;

end Src_Editor_Module.Shell;
