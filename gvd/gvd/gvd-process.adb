-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2006                       --
--                             AdaCore                               --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
pragma Warnings (Off);
with GNAT.TTY;                   use GNAT.TTY;
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Strings;
with System;                     use System;

with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Glib.Xml_Int;               use Glib.Xml_Int;

with Gtk.Arguments;              use Gtk.Arguments;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtk;                        use Gtk;

with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;

with Basic_Types;                use Basic_Types;
with Breakpoints_Editor;         use Breakpoints_Editor;
with Config;                     use Config;
with Debugger.Gdb;               use Debugger.Gdb;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Main_Window;            use GPS.Main_Window;
with GUI_Utils;                  use GUI_Utils;
with GVD.Call_Stack;             use GVD.Call_Stack;
with GVD.Canvas;                 use GVD.Canvas;
with GVD.Code_Editors;           use GVD.Code_Editors;
with GVD.Consoles;               use GVD.Consoles;
with GVD.Dialogs;                use GVD.Dialogs;
with GVD.Preferences;            use GVD.Preferences;
with GVD.Source_Editor;          use GVD.Source_Editor;
with GVD.Source_Editor.GPS;      use GVD.Source_Editor.GPS;
with GVD.Scripts;                use GVD.Scripts;
with GVD.Trace;                  use GVD.Trace;
with GVD.Types;                  use GVD.Types;
with GVD_Module;                 use GVD_Module;
with Language_Handlers;          use Language_Handlers;
with Pixmaps_IDE;                use Pixmaps_IDE;
with Process_Proxies;            use Process_Proxies;
with Projects;                   use Projects;
with Projects.Editor;            use Projects.Editor;
with Projects.Registry;          use Projects.Registry;
with Remote;                     use Remote;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;
with VFS;                        use VFS;

package body GVD.Process is
   Me : constant Debug_Handle := Create ("GVD.Process");

   type GPS_Proxy is new Process_Proxy with record
      Kernel : Kernel_Handle;
   end record;
   --  GPS specific proxy, used to redefine Set_Command_In_Process

   procedure Set_Command_In_Process
     (Proxy : access GPS_Proxy; In_Process : Boolean := True);
   --  Set the appropriate debugger menu items to the corresponding state.

   pragma Warnings (Off);
   --  This UC is safe aliasing-wise, so kill warning
   function To_Main_Debug_Window is new
     Ada.Unchecked_Conversion (System.Address, GPS_Window);
   pragma Warnings (On);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure First_Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Standard handler to add gdb's output to the debugger window.
   --  Simply strip CR characters if needed and then call Text_Output_Filter

   procedure Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Real handler called by First_Text_Output_Filter

   procedure Process_Graph_Command
     (Debugger : Visual_Debugger;
      Command  : String;
      Mode     : Command_Type);
   --  Process a "graph ..." command

   function On_Editor_Text_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the editor text

   procedure Initialize
     (Process : access Visual_Debugger_Record'Class;
      Window  : access GPS.Main_Window.GPS_Window_Record'Class;
      Source  : GVD.Source_Editor.Source_Editor);
   --  Internal initialize procedure.

   procedure Configure
     (Process         : access Visual_Debugger_Record'Class;
      Kind            : GVD.Types.Debugger_Type;
      Proxy           : Process_Proxy_Access;
      Executable      : VFS.Virtual_File;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "";
      Success         : out Boolean);
   --  Configure a visual debugger.
   --  Kind specifies which debugger should be launched.
   --  Currently, only gdb is supported.
   --
   --  Executable is the name of the executable module to debug.
   --  This function returns a Process_Tab_Access.
   --
   --  Debugger_Args are the optional parameters for the underlying debugger.
   --
   --  Executable_Args are the optional parameters for the debuggee.
   --
   --  See Debugger.Spawn for a documentation on Remote_Host, Remote_Target,
   --  Remote_Protocol and Debugger_Name.
   --
   --  Success is set to true is the debugger could be successfully started.

   procedure On_Console_Destroy
     (Process : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Called when the debugger console is destroyed, which also terminates the
   --  debugger itself

   type String_Access_Access is access all GNAT.Strings.String_Access;

   procedure Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type;
      Output         : String_Access_Access);
   --  Wrapper implementing common code for Process_User_Command routines

   ----------------
   -- Properties --
   ----------------

   type Breakpoint_Property_Record is new Property_Record with record
      Breakpoints : Breakpoint_Array_Ptr;
   end record;
   type Breakpoint_Property is access all Breakpoint_Property_Record'Class;

   procedure Save
     (Property : access Breakpoint_Property_Record;
      Node     : in out Glib.Xml_Int.Node_Ptr);
   procedure Load
     (Property : in out Breakpoint_Property_Record;
      From     : Glib.Xml_Int.Node_Ptr);
   procedure Destroy (Property : in out Breakpoint_Property_Record);
   --  See inherited documentation

   procedure Load_Breakpoints_From_Property
     (Process  : access Visual_Debugger_Record'Class;
      Property : Breakpoint_Property_Record'Class);
   --  Restore the breakpoints stored in the property

   procedure Save_Breakpoints_In_Properties
     (Process  : access Visual_Debugger_Record'Class;
      Property : in out Breakpoint_Property);
   --  Save the breakpoints currently set in Process into Property.
   --  Breakpoints that are set automatically by GPS are filtered out

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Property : in out Breakpoint_Property_Record) is
   begin
      Free (Property.Breakpoints);
   end Destroy;

   ----------
   -- Save --
   ----------

   procedure Save
     (Property : access Breakpoint_Property_Record;
      Node     : in out Glib.Xml_Int.Node_Ptr)
   is
      Breaks : Node_Ptr;
   begin
      Traces.Trace (Me, "Saving breakpoints for future sessions");
      if Property.Breakpoints /= null then
         for B in reverse Property.Breakpoints'Range loop
            declare
               Br : Breakpoint_Data renames Property.Breakpoints (B);
            begin
               Breaks := new Glib.Xml_Int.Node;
               Breaks.Next := Node.Child;
               Node.Child := Breaks;
               Breaks.Tag := new String'("breakpoint");
               if Br.The_Type /= Breakpoint then
                  Set_Attribute
                    (Breaks, "type", Breakpoint_Type'Image (Br.The_Type));
               end if;
               if Br.Disposition /= Keep then
                  Set_Attribute
                    (Breaks, "disposition",
                     Breakpoint_Disposition'Image (Br.Disposition));
               end if;
               if not Br.Enabled then
                  Set_Attribute (Breaks, "enabled", "false");
               end if;
               if Br.Expression /= null then
                  Set_Attribute (Breaks, "expression", Br.Expression.all);
               end if;
               if Br.File /= VFS.No_File then
                  Set_Attribute (Breaks, "file", Full_Name (Br.File).all);
               end if;
               if Br.Except /= null then
                  Set_Attribute (Breaks, "exception", Br.Except.all);
               end if;
               if Br.Subprogram /= null then
                  Set_Attribute (Breaks, "subprogram", Br.Subprogram.all);
               end if;
               if Br.Line /= 0 then
                  Set_Attribute (Breaks, "line", Integer'Image (Br.Line));
               end if;
               if Br.Address /= Invalid_Address then
                  Set_Attribute
                    (Breaks, "address", Address_To_String (Br.Address));
               end if;
               if Br.Ignore /= 0 then
                  Set_Attribute
                    (Breaks, "ignore", Integer'Image (Br.Ignore));
               end if;
               if Br.Condition /= null then
                  Set_Attribute (Breaks, "condition", Br.Condition.all);
               end if;
               if Br.Commands /= null then
                  Set_Attribute (Breaks, "command", Br.Commands.all);
               end if;
               if Br.Scope /= No_Scope then
                  Set_Attribute
                    (Breaks, "scope", Scope_Type'Image (Br.Scope));
               end if;
               if Br.Action /= No_Action then
                  Set_Attribute
                    (Breaks, "action", Action_Type'Image (Br.Action));
               end if;
            end;
         end loop;
      end if;
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load
     (Property : in out Breakpoint_Property_Record;
      From     : Glib.Xml_Int.Node_Ptr)
   is
      Breaks : Node_Ptr;
      Count  : Natural := 0;

      function Get_String (Attr : String) return GNAT.Strings.String_Access;
      --  return the value of Attr (or null if the Attr doesn't exist

      ----------------
      -- Get_String --
      ----------------

      function Get_String (Attr : String) return GNAT.Strings.String_Access is
         Value : constant String := Get_Attribute (Breaks, Attr, "");
      begin
         if Value = "" then
            return null;
         else
            return new String'(Value);
         end if;
      end Get_String;

   begin
      Traces.Trace (Me, "Restoring breakpoints from previous session");
      Breaks := From.Child;
      while Breaks /= null loop
         Count := Count + 1;
         Breaks := Breaks.Next;
      end loop;

      Property.Breakpoints := new Breakpoint_Array (1 .. Count);
      Count := Property.Breakpoints'First;

      Breaks := From.Child;
      while Breaks /= null loop
         declare
            Br : Breakpoint_Data renames Property.Breakpoints (Count);
         begin
            Br.The_Type := Breakpoint_Type'Value
              (Get_Attribute
                 (Breaks, "type", Breakpoint_Type'Image (Breakpoint)));
            Br.Disposition := Breakpoint_Disposition'Value
              (Get_Attribute
                 (Breaks, "disposition", Breakpoint_Disposition'Image (Keep)));
            Br.Enabled := Boolean'Value
              (Get_Attribute (Breaks, "enabled", "true"));
            Br.Expression := Get_String ("expression");
            Br.Except     := Get_String ("exception");
            Br.Subprogram := Get_String ("subprogram");
            Br.Line := Integer'Value (Get_Attribute (Breaks, "line", "0"));
            Br.Address    := String_To_Address
              (Get_Attribute (Breaks, "address", ""));
            Br.Ignore := Integer'Value (Get_Attribute (Breaks, "ignore", "0"));
            Br.Condition  := Get_String ("condition");
            Br.Commands   := Get_String ("command");
            Br.Scope      := Scope_Type'Value
              (Get_Attribute (Breaks, "scope", Scope_Type'Image (No_Scope)));
            Br.Action     := Action_Type'Value
              (Get_Attribute
                 (Breaks, "action", Action_Type'Image (No_Action)));
            if Get_Attribute (Breaks, "file", "") = "" then
               Br.File := VFS.No_File;
            else
               Br.File := Create
                 (Full_Filename => Get_Attribute (Breaks, "file"));
            end if;
         end;

         Count := Count + 1;
         Breaks := Breaks.Next;
      end loop;
   end Load;

   ------------------------------------
   -- Load_Breakpoints_From_Property --
   ------------------------------------

   procedure Load_Breakpoints_From_Property
     (Process  : access Visual_Debugger_Record'Class;
      Property : Breakpoint_Property_Record'Class) is
   begin
      if Process.Debugger = null
        or else Property.Breakpoints = null
      then
         return;
      end if;

      for B in Property.Breakpoints'Range loop
         declare
            Br      : Breakpoint_Data renames Property.Breakpoints (B);
            Created : Boolean := True;
            Id      : Breakpoint_Identifier := Breakpoint_Identifier'Last;
         begin
            if Br.Except /= null then
               Break_Exception
                 (Process.Debugger, Br.Except.all,
                  Temporary => Br.Disposition /= Keep, Mode => Internal,
                  Unhandled => False);
            elsif Br.Line /= 0 and then Br.File /= VFS.No_File then
               Break_Source
                 (Process.Debugger, Br.File, Br.Line,
                  Temporary => Br.Disposition /= Keep, Mode => Internal);
            elsif Br.Subprogram /= null then
               Break_Subprogram
                 (Process.Debugger, Br.Subprogram.all,
                  Temporary => Br.Disposition /= Keep, Mode => Internal);
            elsif Br.Address /= Invalid_Address then
               Break_Address
                 (Process.Debugger, Br.Address,
                  Temporary => Br.Disposition /= Keep, Mode => Internal);
            else
               Created := False;
            end if;

            if Created then
               if not Br.Enabled then
                  if Id = Breakpoint_Identifier'Last then
                     Id := Get_Last_Breakpoint_Id (Process.Debugger);
                  end if;
                  Enable_Breakpoint
                    (Process.Debugger, Id, Br.Enabled, Internal);
               end if;

               if Br.Condition /= null then
                  if Id = Breakpoint_Identifier'Last then
                     Id := Get_Last_Breakpoint_Id (Process.Debugger);
                  end if;
                  Set_Breakpoint_Condition
                    (Process.Debugger, Id, Br.Condition.all, Mode => Internal);
               end if;

               if Br.Commands /= null then
                  if Id = Breakpoint_Identifier'Last then
                     Id := Get_Last_Breakpoint_Id (Process.Debugger);
                  end if;
                  Set_Breakpoint_Command
                    (Process.Debugger, Id, Br.Commands.all, Internal);
               end if;

               if Br.Ignore /= 0 then
                  if Id = Breakpoint_Identifier'Last then
                     Id := Get_Last_Breakpoint_Id (Process.Debugger);
                  end if;
                  Set_Breakpoint_Ignore_Count
                    (Process.Debugger, Id, Br.Ignore, Internal);
               end if;

               if Br.Scope /= No_Scope or else Br.Action /= No_Action then
                  if Id = Breakpoint_Identifier'Last then
                     Id := Get_Last_Breakpoint_Id (Process.Debugger);
                  end if;
                  Set_Scope_Action
                    (Process.Debugger, Br.Scope, Br.Action, Id, Internal);
               end if;
            end if;
         end;
      end loop;

      if Property.Breakpoints /= null
        and then Property.Breakpoints'Length /= 0
      then
         Update_Breakpoints (Process, Force => True);
      end if;
   end Load_Breakpoints_From_Property;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Process : access Visual_Debugger_Record'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Process.Window.Kernel;
   end Get_Kernel;

   ----------------------------
   -- Set_Command_In_Process --
   ----------------------------

   procedure Set_Command_In_Process
     (Proxy      : access GPS_Proxy;
      In_Process : Boolean := True) is
   begin
      Set_Command_In_Process (Process_Proxy (Proxy.all)'Access, In_Process);

      if In_Process then
         Set_Sensitive (Proxy.Kernel, Debug_Busy);
      else
         Set_Sensitive (Proxy.Kernel, Debug_Available);
      end if;
   end Set_Command_In_Process;

   ---------------------------------
   -- On_Editor_Text_Delete_Event --
   ---------------------------------

   function On_Editor_Text_Delete_Event
     (Object : access GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Object, Params);
   begin
      return False;
   end On_Editor_Text_Delete_Event;

   -----------------------
   -- Add_Regexp_Filter --
   -----------------------

   procedure Add_Regexp_Filter
     (Process : access Visual_Debugger_Record'Class;
      Filter  : Regexp_Filter_Function;
      Regexp  : Pattern_Matcher) is
   begin
      Process.Filters :=
        new Regexp_Filter_List_Elem'
          (Filter => Filter,
           Regexp => new Pattern_Matcher'(Regexp),
           Next   => Process.Filters);
   end Add_Regexp_Filter;

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access GPS_Window_Record'Class;
      Descriptor        : GNAT.Expect.Process_Descriptor'Class)
      return Visual_Debugger
   is
      Process : Visual_Debugger;
      List    : Debugger_List_Link :=
                  Get_Debugger_List (Main_Debug_Window.Kernel);

   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            --  Note: The process might have been already killed when this
            --  function is called.

            if Get_Descriptor
              (Get_Process (Process.Debugger)).all = Descriptor
            then
               return Process;
            end if;
         end if;

         List := List.Next;
      end loop;

      raise Debugger_Not_Found;

   exception
      when Constraint_Error =>
         raise Debugger_Not_Found;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access Gtk.Window.Gtk_Window_Record'Class;
      Debugger          : access Debugger_Root'Class)
      return Visual_Debugger is
   begin
      return Convert (GPS_Window (Main_Debug_Window),
                      Get_Descriptor (Get_Process (Debugger)).all);
   end Convert;

   ------------------------
   -- Command_In_Process --
   ------------------------

   function Command_In_Process
     (Debugger : access Visual_Debugger_Record'Class) return Boolean is
   begin
      return Debugger.Debugger /= null
        and then Get_Process (Debugger.Debugger) /= null
        and then Command_In_Process (Get_Process (Debugger.Debugger));
   end Command_In_Process;

   -----------------
   -- Output_Text --
   -----------------

   procedure Output_Text
     (Process      : Visual_Debugger;
      Str          : String;
      Is_Command   : Boolean := False;
      Set_Position : Boolean := False)
   is
      pragma Unreferenced (Set_Position);
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Start   : Positive := Str'First;
   begin
      if Is_Command then
         Insert (Process.Debugger_Text, Str, False, True, True);
      else
         while Start <= Str'Last loop
            Match (Highlighting_Pattern (Process.Debugger),
                   Str (Start .. Str'Last),
                   Matched);

            if Matched (0) /= No_Match then
               if Matched (0).First - 1 >= Start then
                  Insert (Process.Debugger_Text,
                          Str (Start .. Matched (0).First - 1),
                          False, False);
               end if;

               Insert (Process.Debugger_Text,
                       Str (Matched (0).First .. Matched (0).Last),
                       False, True);
               Start := Matched (0).Last + 1;

            else
               Insert (Process.Debugger_Text,
                       Str (Start .. Str'Last),
                       False, False);
               Start := Str'Last + 1;
            end if;
         end loop;
      end if;

      Highlight_Child
        (Find_MDI_Child (Process.Window.MDI, Process.Debugger_Text));
   end Output_Text;

   ------------------------
   -- Final_Post_Process --
   ------------------------

   procedure Final_Post_Process
     (Process : access Visual_Debugger_Record'Class;
      Mode    : GVD.Types.Command_Type)
   is
      File_First  : Natural := 0;
      File_Last   : Positive;
      Line        : Natural := 0;
      First, Last : Natural := 0;
      Addr_First  : Natural := 0;
      Addr_Last   : Natural;
      Pc          : Address_Type;

   begin
      if Process.Post_Processing or else Process.Current_Output = null then
         return;
      end if;

      Process.Post_Processing := True;

      if Get_Parse_File_Name (Get_Process (Process.Debugger)) then
         Found_File_Name
           (Process.Debugger,
            Process.Current_Output
              (Process.Current_Output'First .. Process.Current_Output_Pos - 1),
            File_First, File_Last, First, Last, Line,
            Addr_First, Addr_Last);

         --  We have to make a temporary copy of the address, since
         --  the call to Load_File below might modify the current_output
         --  of the process, and thus make the address inaccessible afterwards.

         if Addr_First /= 0 then
            Pc := String_To_Address
              (Process.Current_Output (Addr_First .. Addr_Last));
         end if;
      end if;

      --  Do we have a file name or line number indication?

      if File_First /= 0 then
         --  Override the language currently defined in the editor.

         declare
            File_Name : constant Virtual_File := Create
              (Full_Filename =>
               --  ??? Normalize_Pathname only needed when to get absolute
               --  file name
                 Normalize_Pathname
                   (Process.Current_Output (File_First .. File_Last),
                    Resolve_Links => False));
         begin
            Load_File (Process.Editor_Text, File_Name);
         end;
      end if;

      if Line /= 0
        and then Mode /= Internal
      then
         Set_Line (Process.Editor_Text, Line, GObject (Process));
      end if;

      --  Change the current assembly source displayed, before updating
      --  the breakpoints. Otherwise, they won't be correctly updated for the
      --  newly displayed frame.

      if Addr_First /= 0 then
         Set_Address (Process.Editor_Text, Pc);
      end if;

      if (Line /= 0 and then Mode /= Internal) or else Addr_First /= 0 then
         Update_Assembly_View (Process.Editor_Text);
      end if;

      Highlight_Call_Stack_Frame (Process);

      --  Last step is to update the breakpoints once all the rest has been
      --  set up correctly.
      --  If there is no breakpoint defined, we force an update.

      if File_First /= 0 then
         if Process.Breakpoints = null then
            Update_Breakpoints (Process, Force => True);

         elsif Process.Breakpoints'Length > 0 then
            Update_Breakpoints
              (Process.Editor_Text, Process.Breakpoints.all);
         end if;
      end if;

      if Process.Debuggee_Console /= null then
         Cleanup_TTY_If_Needed (Process.Debuggee_Console);
      end if;

      Process.Post_Processing := False;
      Free (Process.Current_Output);

      --  Preserve the focus in the console for interactive execution

      if Process.Interactive_Command then
         if Process.Debugger_Text /= null then
            Set_Focus_Child
              (Find_MDI_Child (Process.Window.MDI, Process.Debugger_Text));
         end if;

         Process.Interactive_Command := False;
      end if;
   end Final_Post_Process;

   ------------------------------
   -- First_Text_Output_Filter --
   ------------------------------

   procedure First_Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address) is
   begin
      if Need_To_Strip_CR then
         Text_Output_Filter (Descriptor, Strip_CR (Str), Window);
      else
         Text_Output_Filter (Descriptor, Str, Window);
      end if;
   end First_Text_Output_Filter;

   ------------------------
   -- Text_Output_Filter --
   ------------------------

   procedure Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address)
   is
      Process        : constant Visual_Debugger :=
                         Convert (To_Main_Debug_Window (Window), Descriptor);
      Tmp_Str        : GNAT.Strings.String_Access;
      Current_Filter : Regexp_Filter_List;
      Matched        : Match_Array (0 .. Max_Paren_Count);
      First, Last    : Natural := 0;
      Last_Match     : Natural := 0;
      Min_Size       : Natural;
      New_Size       : Natural;

   begin
      --  Concatenate current output

      if Process.Current_Output = null then
         Process.Current_Output := new String (1 .. 1024);
         Process.Current_Output_Pos := 1;
         Process.Last_Match := 0;
      end if;

      Min_Size := Process.Current_Output_Pos + Str'Length;

      if Process.Current_Output'Last < Min_Size then
         New_Size := Process.Current_Output'Length * 2;

         while New_Size < Min_Size loop
            New_Size := New_Size * 2;
         end loop;

         Tmp_Str := new String (1 .. New_Size);
         Tmp_Str (1 .. Process.Current_Output_Pos - 1) :=
           Process.Current_Output (1 .. Process.Current_Output_Pos - 1);
         Free (Process.Current_Output);
         Process.Current_Output := Tmp_Str;
      end if;

      Process.Current_Output
        (Process.Current_Output_Pos ..
         Process.Current_Output_Pos + Str'Length - 1) := Str;
      Process.Current_Output_Pos := Process.Current_Output_Pos + Str'Length;

      --  Process the filters

      Current_Filter := Process.Filters;

      while Current_Filter /= null loop
         Match
           (Current_Filter.Regexp.all,
            Process.Current_Output
              (Process.Last_Match + 1 .. Process.Current_Output_Pos - 1),
            Matched);

         if Matched (0) /= No_Match then
            if Matched (0).Last > Last_Match then
               Last_Match := Matched (0).Last;
            end if;

            Current_Filter.Filter
              (Process,
               Process.Current_Output (1 .. Process.Current_Output_Pos - 1),
               Matched);
         end if;

         Current_Filter := Current_Filter.Next;
      end loop;

      if Last_Match /= 0 then
         Process.Last_Match := Last_Match;
      end if;

      --  Do not show the output if we have an internal or hidden command

      case Get_Command_Mode (Get_Process (Process.Debugger)) is
         when User | GVD.Types.Visible =>
            --  Strip every line starting with ^Z^Z.
            --  Note that this is GDB specific ???

            Outer_Loop :
            for J in Str'First + 1 .. Str'Last loop
               if Str (J) = ASCII.SUB and then Str (J - 1) = ASCII.SUB then
                  First := J - 1;

                  for K in J + 1 .. Str'Last loop
                     if Str (K) = ASCII.LF then
                        Last := K;
                        exit Outer_Loop;
                     end if;
                  end loop;

                  Last := Str'Last;
                  exit Outer_Loop;
               end if;
            end loop Outer_Loop;

            if First = 0 then
               Output_Text (Process, Str, Set_Position => True);
            else
               Output_Text (Process, Str (Str'First .. First - 1));
               Output_Text
               (Process, Str (Last + 1 .. Str'Last), Set_Position => True);
            end if;

         when Hidden | Internal =>
            null;
      end case;
   end Text_Output_Filter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Process : access Visual_Debugger_Record'Class;
      Window  : access GPS.Main_Window.GPS_Window_Record'Class;
      Source  : GVD.Source_Editor.Source_Editor)
   is
      Debugger_List : Debugger_List_Link;
      Debugger_Num  : Natural := 1;
   begin
      Initialize (Process);
      Ref (Process);
      Process.Window := Window.all'Access;

      Gtk_New_Hbox (Process.Editor_Text, Process);
      Gtkada.Handlers.Object_Return_Callback.Object_Connect
        (Process.Editor_Text, "delete_event",
         On_Editor_Text_Delete_Event'Access, Process);

      --  Initialize the code editor.
      --  This should be done before initializing the debugger, in case the
      --  debugger outputs a file name that should be displayed in the editor.
      --  The language of the editor will automatically be set by the output
      --  filter.

      Configure
        (Process.Editor_Text,
         Source,
         Get_Pref_Font (Default_Style),
         arrow_xpm, stop_xpm);

      Set_Current_Debugger (Window.Kernel, GObject (Process));

      if Get_Debugger_List (Window.Kernel) = null then
         Process.Debugger_Num := Debugger_Num;
         Set_First_Debugger
           (Window.Kernel,
            new Debugger_List_Node'
              (Next     => null,
               Debugger => Get_Current_Debugger (Window.Kernel)));

      else
         Debugger_Num := Debugger_Num + 1;
         Debugger_List := Get_Debugger_List (Window.Kernel);

         while Debugger_List.Next /= null loop
            Debugger_Num := Debugger_Num + 1;
            Debugger_List := Debugger_List.Next;
         end loop;

         Process.Debugger_Num := Debugger_Num;
         Debugger_List.Next := new Debugger_List_Node'
           (Next     => null,
            Debugger => Get_Current_Debugger (Window.Kernel));
      end if;
   end Initialize;

   ------------------------
   -- On_Console_Destroy --
   ------------------------

   procedure On_Console_Destroy
     (Process : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
      Proc : constant Visual_Debugger := Visual_Debugger (Process);
   begin
      Proc.Debugger_Text := null;
      Close_Debugger (Proc);
   end On_Console_Destroy;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Process         : access Visual_Debugger_Record'Class;
      Kind            : Debugger_Type;
      Proxy           : Process_Proxy_Access;
      Executable      : VFS.Virtual_File;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "";
      Success         : out Boolean)
   is
      Window      : constant GPS_Window :=
                      GPS_Window (Process.Window);
      Buttons     : Message_Dialog_Buttons;
      pragma Unreferenced (Buttons);
      WTX_Version : Natural := 0;
      Widget      : Gtk_Menu_Item;

   begin
      Set_Busy (Process, True);
      Attach_To_Debugger_Console (Process, Create_If_Necessary => True);

      --  Destroying the console should kill the debugger
      if Process.Debugger_Text /= null then
         Kernel_Callback.Object_Connect
           (Process.Debugger_Text, "destroy",
            On_Console_Destroy'Access,
            After       => True,
            User_Data   => null,
            Slot_Object => Process);
      end if;

      Process.Descriptor.Debugger := Kind;

      if Remote_Protocol = "" then
         Process.Descriptor.Remote_Target := new String'("");
         Process.Descriptor.Protocol := new String'("");
      else
         Process.Descriptor.Remote_Target := new String'(Remote_Target);
         Process.Descriptor.Protocol := new String'(Remote_Protocol);
      end if;

      Process.Descriptor.Program := Executable;
      Process.Descriptor.Debugger_Name := new String'(Debugger_Name);

      case Kind is
         when Gdb_Type =>
            Process.Debugger := new Gdb_Debugger;
         when others =>
            Set_Busy (Process, False);
            raise Debugger_Not_Supported;
      end case;

      --  Spawn the debugger.

      Spawn
        (Process.Debugger,
         Executable,
         Debugger_Args,
         Executable_Args,
         Proxy,
         Process.Window.all'Access,
         Remote_Target,
         Remote_Protocol,
         Debugger_Name);

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window.

      Add_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         First_Text_Output_Filter'Access, Output, Process.Window.all'Address);

      --  Initialize the debugger, and possibly get the name of the initial
      --  file.

      Initialize (Process.Debugger);

      --  Hide or show AE653 specific capabilities according to the debugger
      --  we are using

      Widget := Find_Menu_Item
        (Window.Kernel, -"/Debug/Data/Protection Domains");

      if Widget /= null then
         Info_WTX (Process.Debugger, WTX_Version);
         Set_Sensitive (Widget, WTX_Version >= 3);
      end if;

      --  If some unattached dialogs exist, claim them
      Attach_To_Call_Stack
        (Process, Create_If_Necessary => Get_Pref (Show_Call_Stack));
      Attach_To_Data_Window   (Process, Create_If_Necessary => False);
      Attach_To_Thread_Dialog (Process, Create_If_Necessary => False);
      Attach_To_Tasks_Dialog  (Process, Create_If_Necessary => False);
      Attach_To_PD_Dialog     (Process, Create_If_Necessary => False);

      --  If we have a debuggee console in the desktop, always use it.
      --  Otherwise, we only create one when the user has asked for it

      Attach_To_Debuggee_Console
        (Process,
         Create_If_Necessary =>
           Get_Pref (Execution_Window)
           and then Is_Local (Debug_Server)
           and then Support_TTY (Process.Debugger)
           and then GNAT.TTY.TTY_Supported);

      Grab_Focus (Process.Debugger_Text);

      Set_Busy (Process, False);
      Success := True;

   exception
      when Process_Died =>
         Set_Busy (Process, False);
         Buttons :=
           Message_Dialog
             (Expect_Out (Get_Process (Process.Debugger)) & ASCII.LF &
              (-"Could not launch the debugger"),
              Error, Button_OK, Button_OK);
         Process.Exiting := True;

         Close (Window.MDI, Process.Debugger_Text);

         Process.Exiting := False;
         Success := False;

      when Spawn_Error =>
         --  Do not display a dialog here since the Spawn procedure displays
         --  a dialog before raising Spawn_Error.

         Set_Busy (Process, False);
         Success := False;
   end Configure;

   ------------------------------------
   -- Save_Breakpoints_In_Properties --
   ------------------------------------

   procedure Save_Breakpoints_In_Properties
     (Process  : access Visual_Debugger_Record'Class;
      Property : in out Breakpoint_Property)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Breakpoint_Array, Breakpoint_Array_Ptr);
      Count : Natural := 0;
   begin
      --  Filter breakpoints that are created automatically by GPS as a
      --  result of preferences

      if Get_Pref (Break_On_Exception) then
         for B in reverse Process.Breakpoints'Range loop
            declare
               Br : Breakpoint_Data renames Process.Breakpoints (B);
            begin
               if Br.Except = null
                 or else Br.Except.all /= "all"
               then
                  Count := Count + 1;
               end if;
            end;
         end loop;

         Property.Breakpoints := new Breakpoint_Array (1 .. Count);
         Count := Property.Breakpoints'First;

         for B in reverse Process.Breakpoints'Range loop
            declare
               Br : Breakpoint_Data renames Process.Breakpoints (B);
            begin
               if Br.Except = null
                 or else Br.Except.all /= "all"
               then
                  Property.Breakpoints (Count) :=  Br;
                  Count := Count + 1;
               else
                  Free (Br);
               end if;
            end;
         end loop;

         Unchecked_Free (Process.Breakpoints);

      else
         Property.Breakpoints := Process.Breakpoints;
      end if;

      Process.Breakpoints  := null;

      Set_Property
        (File       => Get_Executable (Process.Debugger),
         Name       => "breakpoints",
         Property   => Property,
         Persistent => True);
   end Save_Breakpoints_In_Properties;

   --------------------
   -- Close_Debugger --
   --------------------

   procedure Close_Debugger (Process : access Visual_Debugger_Record) is
      Kernel        : constant Kernel_Handle := Process.Window.Kernel;
      Debugger_List : Debugger_List_Link := Get_Debugger_List (Kernel);
      Prev          : Debugger_List_Link;
      Editor        : Code_Editor;
      Property      : Breakpoint_Property;
      Window        : MDI_Window;
   begin
      if Process.Exiting then
         return;
      end if;

      Traces.Trace (Me, "Closing Debugger");
      while Debugger_List /= null
        and then Debugger_List.Debugger /= GObject (Process)
      loop
         Prev          := Debugger_List;
         Debugger_List := Debugger_List.Next;
      end loop;

      if Debugger_List = null then
         --  Should never happen
         return;
      end if;

      Process.Exiting := True;
      Push_State (Kernel, Busy);
      Run_Debugger_Hook (Process, Debugger_Terminated_Hook);

      --  Save the breakpoints if needed

      if Get_Pref (Preserve_State_On_Exit) then
         if Process.Breakpoints /= null then
            Traces.Trace (Me, "Saving breakpoints in properties");
            --  Take into account breakpoints that have been set manually
            --  through the console, when the breakpoints window is not shown

            Update_Breakpoints (Process, Force => True);
            Property             := new Breakpoint_Property_Record;
            Save_Breakpoints_In_Properties (Process, Property);
         else
            Remove_Property
              (File => Get_Executable (Process.Debugger),
               Name => "breakpoints");
         end if;
      end if;

      --  Memorize whether we should automatically start the call stack the
      --  next time GVD is started or not

      Set_Pref (Kernel, Show_Call_Stack, Process.Stack /= null);

      --  This might have been closed by the user

      Window := Get_MDI (Kernel);

      if Window /= null and then Process.Debugger_Text /= null then
         Close (Get_MDI (Kernel), Process.Debugger_Text, Force => True);
      end if;

      if Process.Breakpoints /= null then
         Free (Process.Breakpoints);
      end if;

      Editor := Process.Editor_Text;
      if Window /= null and then Get_Mode (Editor) /= Source then
         Gtkada.MDI.Close (Get_MDI (Kernel), Get_Asm (Editor));
      end if;

      Free_Debug_Info (GEdit (Get_Source (Process.Editor_Text)));

      Free (Process.Breakpoints);
      Unregister_Dialog (Process);
      Free (Process.Command_History);

      --  Close the underlying debugger

      if Process.Debugger /= null
        and then Get_Process (Process.Debugger) /= null
      then
         Close (Process.Debugger);
      end if;

      Process.Debugger := null;

      if Process.Timeout_Id /= 0 then
         Set_Busy (Process, False);
         Timeout_Remove (Process.Timeout_Id);
         Process.Timeout_Id := 0;
      end if;

      Process.Exiting := False;
      Unref (Process);

      if Prev = null then
         Set_First_Debugger (Kernel, Debugger_List.Next);

         if Debugger_List.Next = null then
            Set_Current_Debugger (Kernel, null);
         else
            Set_Current_Debugger (Kernel, Debugger_List.Next.Debugger);
         end if;
      else
         Prev.Next := Debugger_List.Next;
         Set_Current_Debugger (Kernel, Prev.Debugger);
      end if;

      Free (Debugger_List);

      if Get_Debugger_List (Kernel) = null then
         Debug_Terminate (Kernel);
      end if;

      Pop_State (Kernel);
   end Close_Debugger;

   ---------------------------
   -- Process_Graph_Command --
   ---------------------------

   procedure Process_Graph_Command
     (Debugger : Visual_Debugger;
      Command  : String;
      Mode     : Command_Type)
   is
      Busy  : constant Boolean :=
                Command_In_Process (Get_Process (Debugger.Debugger));
      Data  : History_Data;
      Index : Natural;

   begin
      Data.Mode := Mode;
      Data.Command := new String'(Command);
      Output_Message (Debugger, Command, Mode);

      if Mode /= Internal then
         Append (Debugger.Command_History, Data);
      end if;

      if Busy then
         --  Process graph disable|undisplay commands immediately since
         --  they do not require access to the underlying debugger, and
         --  queue other graph commands

         declare
            Lowered_Command : constant String := To_Lower (Command);
         begin
            Index := Lowered_Command'First;
            Skip_Blanks (Lowered_Command, Index);
            Index := Index + 6;  -- Skip "graph "
            Skip_Blanks (Lowered_Command, Index);

            if not (Looking_At (Lowered_Command, Index, "disable")
                    or else Looking_At (Lowered_Command, Index, "undisplay"))
            then
               --  Command requires access to the debugger, queue it

               Send (Debugger.Debugger, Command, Mode => Mode);
               return;
            end if;
         end;
      else
         Set_Busy (Debugger);
      end if;

      Process_Graph_Cmd (Debugger, Command);

      if Mode in Visible_Command and then not Busy then
         Display_Prompt (Debugger.Debugger);
      end if;

      if not Busy then
         Set_Busy (Debugger, False);
      end if;
   end Process_Graph_Command;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type;
      Output         : String_Access_Access)
   is
      Quit_String     : constant String := "quit     ";
      Lowered_Command : constant String := To_Lower (Command);
      First           : Natural := Lowered_Command'First;
      Busy            : Boolean;

      function Check (S : String) return String;

      --  Check validity of debugger command S, and modify it if needed

      -----------
      -- Check --
      -----------

      function Check (S : String) return String is
         Last : Integer := S'Last;
      begin
         --  ??? Should forbid commands that modify the configuration of the
         --  debugger, like "set annotate" for gdb, otherwise we can't be sure
         --  what to expect from the debugger.

         if S'Length = 0 then
            return "";
         else
            --  Strip trailing characters which will block GPS

            while Last >= S'First
              and then (S (Last) = '\' or else S (Last) = ASCII.HT)
            loop
                  Last := Last - 1;
            end loop;

            return S (S'First .. Last);
         end if;
      end Check;

   begin
      if Output /= null then
         Output.all := null;
      end if;

      if Debugger.Debugger = null then
         return;
      end if;

      Busy := Command_In_Process (Get_Process (Debugger.Debugger));

      if Output /= null and then Busy then
         Traces.Trace (Me, "Process_User_Command: Debugger is already busy");
         return;
      end if;

      if Output_Command then
         Output_Text (Debugger, Command & ASCII.LF, Is_Command => True);
      end if;

      --  Is this a command handled by a script ?

      declare
         Tmp : constant String := Run_Debugger_Hook_Until_Not_Empty
           (Debugger  => Debugger,
            Hook_Name => Debugger_Command_Action_Hook,
            Command   => Command);
      begin
         if Tmp /= "" then
            if Output_Command then
               Output_Text (Debugger, Tmp, Is_Command => False,
                            Set_Position => True);
            end if;

            if not Busy then
               Display_Prompt (Debugger.Debugger);
            end if;

            if Output /= null then
               Output.all := new String'(Tmp);
            end if;

            return;
         end if;
      end;

      --  Command has been converted to lower-cases, but the new version
      --  should be used only to compare with our standard list of commands.
      --  We should pass the original string to the debugger, in case we are
      --  in a case-sensitive language.

      --  Ignore the blanks at the beginning of lines

      Skip_Blanks (Lowered_Command, First);

      if Looking_At (Lowered_Command, First, "graph") then
         Process_Graph_Command (Debugger, Command, Mode);

      elsif Lowered_Command'Length in 1 .. Quit_String'Length
        and then Lowered_Command = Quit_String (1 .. Lowered_Command'Length)
      then
         if Busy
           and then not Separate_Execution_Window (Debugger.Debugger)
         then
            --  If the debugger does not have a separate execution window,
            --  send the command right away.

            Send
              (Debugger.Debugger,
               Command, Wait_For_Prompt => False, Mode => Mode);

         else
            Close_Debugger (Debugger);
         end if;

      else
         --  Regular debugger command, send it.
         --  If a dialog is currently displayed, do not wait for the debugger
         --  prompt, since the prompt won't be displayed before the user
         --  answers the question...

         if Output = null
           and then (Continuation_Line (Debugger.Debugger)
                     or else Debugger.Registered_Dialog /= null)
         then
            Send
              (Debugger.Debugger,
               Command, Wait_For_Prompt => False, Mode => Mode);
         else
            if Output = null then
               Send (Debugger.Debugger, Check (Command), Mode => Mode);
            else
               Output.all :=
                 new String'(Send
                   (Debugger.Debugger, Check (Command), Mode => Mode));

               if Output_Command
                 and then Debugger.Debugger /= null
               then
                  Display_Prompt (Debugger.Debugger);
               end if;
            end if;
         end if;
      end if;
   end Process_User_Command;

   procedure Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type := GVD.Types.Visible) is
   begin
      Process_User_Command (Debugger, Command, Output_Command, Mode, null);
   end Process_User_Command;

   function Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String
   is
      Result : aliased GNAT.Strings.String_Access;
   begin
      Process_User_Command
        (Debugger, Command, Output_Command, Mode, Result'Unchecked_Access);

      if Result = null then
         return "";
      else
         declare
            S : constant String := Result.all;
         begin
            GNAT.Strings.Free (Result);
            return S;
         end;
      end if;
   end Process_User_Command;

   ---------------------
   -- Register_Dialog --
   ---------------------

   procedure Register_Dialog
     (Process : access Visual_Debugger_Record;
      Dialog  : access Gtk.Dialog.Gtk_Dialog_Record'Class) is
   begin
      if Process.Registered_Dialog /= null then
         --  Typically happens when the filter used to create a dialog
         --  is called several times for the same dialog.

         Destroy (Process.Registered_Dialog);
      end if;

      Process.Registered_Dialog := Gtk_Dialog (Dialog);
   end Register_Dialog;

   -----------------------
   -- Unregister_Dialog --
   -----------------------

   procedure Unregister_Dialog
     (Process : access Visual_Debugger_Record) is
   begin
      if Process.Registered_Dialog /= null then
         Destroy (Process.Registered_Dialog);
         Process.Registered_Dialog := null;
      end if;
   end Unregister_Dialog;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Process : access GObject_Record'Class;
      Force   : Boolean)
   is
      Debugger  : constant Visual_Debugger := Visual_Debugger (Process);
      Bp_Editor : Breakpoint_Editor_Access;
   begin
      --  We only need to update the list of breakpoints when we have a
      --  temporary breakpoint (since its status might be changed upon
      --  reaching the line).

      if Command_In_Process (Get_Process (Debugger.Debugger)) then
         --  Will only happen when e.g. exiting and the debuggee
         --  is still running.

         return;
      end if;

      if Force or else Debugger.Has_Temporary_Breakpoint then
         Free (Debugger.Breakpoints);
         Debugger.Breakpoints := new Breakpoint_Array'
           (List_Breakpoints (Debugger.Debugger));

         --  Check whether there is any temporary breakpoint

         Debugger.Has_Temporary_Breakpoint := False;

         for J in Debugger.Breakpoints'Range loop
            if Debugger.Breakpoints (J).Disposition /= Keep
              and then Debugger.Breakpoints (J).Enabled
            then
               Debugger.Has_Temporary_Breakpoint := True;
               exit;
            end if;
         end loop;

         --  Update the breakpoints in the editor
         Update_Breakpoints (Debugger.Editor_Text, Debugger.Breakpoints.all);

         --  Update the breakpoints dialog if necessary
         Bp_Editor := Breakpoint_Editor_Access
           (Get_Breakpoints_Editor (Debugger.Window.Kernel));

         if Bp_Editor /= null and then Mapped_Is_Set (Bp_Editor) then
            Update_Breakpoint_List (Bp_Editor);
         end if;
      end if;
   end Update_Breakpoints;

   -----------------------------
   -- Toggle_Breakpoint_State --
   -----------------------------

   function Toggle_Breakpoint_State
     (Process        : access Visual_Debugger_Record;
      Breakpoint_Num : Breakpoint_Identifier) return Boolean is
   begin
      --  ??? Maybe we should also update the icons in the code_editor to have
      --  an icon of a different color ?

      if Process.Breakpoints /= null then
         for J in Process.Breakpoints'Range loop
            if Process.Breakpoints (J).Num = Breakpoint_Num then
               Process.Breakpoints (J).Enabled :=
                 not Process.Breakpoints (J).Enabled;
               Enable_Breakpoint
                 (Process.Debugger, Breakpoint_Num,
                  Process.Breakpoints (J).Enabled,
                  Mode => GVD.Types.Visible);
               return Process.Breakpoints (J).Enabled;
            end if;
         end loop;
      end if;

      return False;
   end Toggle_Breakpoint_State;

   -------------------------
   -- Get_Current_Process --
   -------------------------

   function Get_Current_Process
     (Main_Window : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Visual_Debugger is
   begin
      return Visual_Debugger
        (Get_Current_Debugger (GPS_Window (Main_Window).Kernel));
   end Get_Current_Process;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy
     (Debugger      : access Visual_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False) is
   begin
      Set_Busy_Cursor (Get_Window (Debugger.Window), Busy, Force_Refresh);
   end Set_Busy;

   -------------
   -- Get_Num --
   -------------

   function Get_Num (Tab : Visual_Debugger) return Gint is
   begin
      return Gint (Tab.Debugger_Num);
   end Get_Num;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Process : access Visual_Debugger_Record'Class)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Process.Debugger_Text);
   end Get_Console;

   ---------------------------------
   -- Set_Current_Source_Location --
   ---------------------------------

   procedure Set_Current_Source_Location
     (Process : access Visual_Debugger_Record;
      File    : VFS.Virtual_File;
      Line    : Integer) is
   begin
      Process.Current_File := File;
      Process.Current_Line := Line;
   end Set_Current_Source_Location;

   -----------------------------
   -- Get_Current_Source_File --
   -----------------------------

   function Get_Current_Source_File
     (Process : access Visual_Debugger_Record) return VFS.Virtual_File is
   begin
      return Process.Current_File;
   end Get_Current_Source_File;

   -----------------------------
   -- Get_Current_Source_Line --
   -----------------------------

   function Get_Current_Source_Line
     (Process : access Visual_Debugger_Record) return Integer is
   begin
      return Process.Current_Line;
   end Get_Current_Source_Line;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : VFS.Virtual_File;
      Project : Projects.Project_Type;
      Args    : String) return Visual_Debugger
   is
      Top          : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process      : Visual_Debugger;
      Edit         : GVD.Source_Editor.GPS.GEdit;
      Module       : VFS.Virtual_File;
      Program_Args : GNAT.Strings.String_Access;
      Blank_Pos    : Natural;
      Proxy        : Process_Proxy_Access;
      Success      : Boolean;
      Property     : Breakpoint_Property_Record;
      Exec         : GNAT.Strings.String_Access;

      procedure Check_Extension (Module : in out Virtual_File);
      --  Check for a missing extension in module, and add it if needed
      --  Extensions currently checked in order: .exe, .out, .vxe

      ---------------------
      -- Check_Extension --
      ---------------------

      procedure Check_Extension (Module : in out Virtual_File) is
         type Extension_Array is array (Positive range <>) of String (1 .. 4);
         Extensions : constant Extension_Array := (".exe", ".out", ".vxe");
         Tmp        : Virtual_File;

      begin
         if Module = VFS.No_File or else Is_Regular_File (Module) then
            return;
         end if;

         for J in Extensions'Range loop
            Tmp := Create
              (Full_Filename => Full_Name (Module).all & Extensions (J));

            if Is_Regular_File (Tmp) then
               Module := Tmp;
               return;
            end if;
         end loop;
      end Check_Extension;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Process := new Visual_Debugger_Record;
      GVD.Source_Editor.GPS.Gtk_New (Edit, Top);
      GVD.Process.Initialize
        (Process, Top, GVD.Source_Editor.Source_Editor (Edit));

      Program_Args := new String'("");

      if File /= VFS.No_File then
         Module := File;

      elsif Args /= "" then
         Blank_Pos := Ada.Strings.Fixed.Index (Args, " ");

         if Blank_Pos = 0 then
            Exec := Locate_Exec_On_Path (Args);

            if Exec /= null then
               Module := Create
                 (Full_Filename =>
                    Normalize_Pathname (Exec.all, Get_Current_Dir));
               Free (Exec);

            else
               Module := Create
                 (Full_Filename => Normalize_Pathname (Args, Get_Current_Dir));
            end if;

         else
            Exec := Locate_Exec_On_Path (Args (Args'First .. Blank_Pos - 1));

            if Exec /= null then
               Module := Create
                 (Full_Filename =>
                    Normalize_Pathname (Exec.all, Get_Current_Dir));
               Free (Exec);

            else
               Module := Create
                 (Full_Filename =>
                    Normalize_Pathname (Args (Args'First .. Blank_Pos - 1),
                                        Get_Current_Dir));
            end if;

            Free (Program_Args);
            Program_Args := new String'(Args (Blank_Pos + 1 .. Args'Last));
         end if;

      else
         Module := VFS.No_File;
      end if;

      Check_Extension (Module);

      declare
         Args : GNAT.OS_Lib.Argument_List_Access :=
           GNAT.OS_Lib.Argument_String_To_List
             (Get_Attribute_Value
                (Project, Debugger_Command_Attribute,
                 Default => "gdb"));

      begin
         Proxy := new GPS_Proxy;
         GPS_Proxy (Proxy.all).Kernel := Kernel_Handle (Kernel);
         Configure
           (Process         => Process,
            Kind            => Gdb_Type,
            Proxy           => Proxy,
            Executable      => Module,
            Debugger_Args   => Args (2 .. Args'Last),
            Executable_Args => Program_Args.all,
            Remote_Target   =>
              Get_Attribute_Value (Project, Program_Host_Attribute),
            Remote_Protocol =>
              Get_Attribute_Value (Project, Protocol_Attribute),
            Debugger_Name   => Args (1).all,
            Success         => Success);
         GNAT.OS_Lib.Free (Args);

         if not Success then
            Close_Debugger (Process);
            Pop_State (Kernel_Handle (Kernel));
            return null;
         end if;
      end;

      Set_Sensitive (Kernel, Debug_Available);
      Setup_Side_Columns (Kernel);

      --  Force the creation of the project if needed
      Load_Project_From_Executable (Kernel, Get_Current_Process (Top));

      --  Restore the breakpoints
      if Get_Pref (Preserve_State_On_Exit) then
         Get_Property
           (Property, Get_Executable (Process.Debugger),
            Name => "breakpoints", Found => Success);
         if Success then
            Load_Breakpoints_From_Property (Process, Property);
         end if;
      end if;

      Run_Debugger_Hook (Process, Debugger_Started_Hook);

      Pop_State (Kernel_Handle (Kernel));
      return Process;

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Traces.Trace (Exception_Handle,
                       "Unexpected exception: " & Exception_Information (E));
         return Process;
   end Spawn;

   ----------------------------------
   -- Load_Project_From_Executable --
   ----------------------------------

   procedure Load_Project_From_Executable
     (Kernel   : access Kernel_Handle_Record'Class;
      Debugger : access Visual_Debugger_Record'Class)
   is
      Project : Project_Type := Get_Project (Kernel);
      Exec    : Virtual_File;

   begin
      --  Do nothing unless the current project was already generated from an
      --  executable

      if Status (Project) /= From_Executable then
         return;
      end if;

      Exec := Get_Executable (Debugger.Debugger);

      declare
         List : Argument_List := Get_Attribute_Value
           (Project, Main_Attribute);
      begin
         for L in List'Range loop
            if List (L).all = Full_Name (Exec).all then
               Free (List);
               return;
            end if;
         end loop;
         Free (List);
      end;

      --  No handling of desktop is done here, we want to leave all windows
      --  as-is.

      declare
         Debugger_Name : constant String :=
                           Get_Attribute_Value
                             (Project,
                              Debugger_Command_Attribute, Default => "");
         Target        : constant String :=
                           Get_Attribute_Value
                             (Project, Program_Host_Attribute, Default => "");
         Protocol      : constant String :=
                           Get_Attribute_Value
                             (Project, Protocol_Attribute, Default => "");
      begin
         Unload_Project (Project_Registry (Get_Registry (Kernel).all));

         if Exec /= VFS.No_File then
            Project := Create_Project
              (Project_Registry (Get_Registry (Kernel).all),
               "debugger_" & Base_Name (Exec), Get_Current_Dir);
         else
            Project := Create_Project
              (Project_Registry (Get_Registry (Kernel).all),
               "debugger_no_file", Get_Current_Dir);
         end if;

         if Debugger_Name /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Debugger_Command_Attribute,
               Value              => Debugger_Name);
         end if;

         if Target /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Program_Host_Attribute,
               Value              => Target);
         end if;

         if Protocol /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Protocol_Attribute,
               Value              => Protocol);
         end if;
      end;

      declare
         List       : GNAT.Strings.String_List :=
                        Source_Files_List (Debugger.Debugger);
         Bases      : GNAT.OS_Lib.Argument_List (List'Range);
         Dirs       : GNAT.OS_Lib.Argument_List (List'Range);
         Dirs_Index : Natural := Dirs'First;
         Main       : GNAT.OS_Lib.Argument_List (1 .. 1);
         Langs      : GNAT.OS_Lib.Argument_List (List'Range);
         Lang_Index : Natural := Langs'First;

      begin
         for L in List'Range loop
            Bases (L) := new String'(Base_Name (List (L).all));
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Source_Files_Attribute,
            Values             => Bases);
         Free (Bases);

         for L in List'Range loop
            declare
               Dir   : constant String := GNAT.OS_Lib.Normalize_Pathname
                 (Dir_Name (List (L).all),
                  Dir_Name (Exec).all,
                  Resolve_Links => False);
               Found : Boolean := False;
            begin
               for D in Dirs'First .. Dirs_Index - 1 loop
                  if Dirs (D).all = Dir then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Dirs (Dirs_Index) := new String'(Dir);
                  Dirs_Index := Dirs_Index + 1;
               end if;
            end;
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Source_Dirs_Attribute,
            Values             => Dirs (Dirs'First .. Dirs_Index - 1));
         Free (Dirs);

         for L in List'Range loop
            declare
               Lang : constant String := Get_Language_From_File
                 (Get_Language_Handler (Kernel),
                  Create (Full_Filename => List (L).all));
               Found : Boolean := False;
            begin
               if Lang /= "" then
                  for La in Langs'First .. Lang_Index - 1 loop
                     if Langs (La).all = Lang then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Langs (Lang_Index) := new String'(Lang);
                     Lang_Index := Lang_Index + 1;
                  end if;
               end if;
            end;
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Languages_Attribute,
            Values             => Langs (Langs'First .. Lang_Index - 1));
         Free (Langs);

         if Exec /= VFS.No_File then
            Update_Attribute_Value_In_Scenario
              (Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Obj_Dir_Attribute,
               Value              => Dir_Name (Exec).all);
            Update_Attribute_Value_In_Scenario
              (Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Exec_Dir_Attribute,
               Value              => Dir_Name (Exec).all);

            Main (Main'First) := new String'(Full_Name (Exec).all);
            Update_Attribute_Value_In_Scenario
              (Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Main_Attribute,
               Values             => Main);
            Free (Main);
         end if;
         Free (List);
      end;

      --  Is the information for this executable already cached ? If yes,
      --  we simply reuse it to avoid the need to interact with the debugger.

      Load_Custom_Project
        (Project_Registry (Get_Registry (Kernel).all), Project);
      Set_Status (Get_Project (Kernel), From_Executable);
      Run_Hook (Kernel, Project_Changed_Hook);
      Recompute_View (Kernel);
   end Load_Project_From_Executable;

end GVD.Process;
