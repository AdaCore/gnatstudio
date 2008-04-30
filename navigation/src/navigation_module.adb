-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2007, AdaCore                 --
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

with GNAT.OS_Lib;
with GNATCOLL.Scripts;               use GNATCOLL.Scripts;
with GNAT.Strings;               use GNAT.Strings;

with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Glib.Xml_Int;               use Glib.Xml_Int;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Separator_Tool_Item;    use Gtk.Separator_Tool_Item;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Tool_Button;            use Gtk.Tool_Button;
with Gtk.Toolbar;                use Gtk.Toolbar;
with Gtk.Widget;                 use Gtk.Widget;

with Projects;                   use Projects;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Location_View;          use GPS.Location_View;
with GPS.Intl;                   use GPS.Intl;
with XML_Parsers;                use XML_Parsers;

with Traces;                     use Traces;
with String_Utils;               use String_Utils;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;
with Language;                   use Language;

package body Navigation_Module is
   Me : constant Debug_Handle := Create ("Navigation");

   Max_Locations_In_History : constant := 200;
   --  Maximum number of locations stored in the history

   Navigation_Module_Name : constant String := "Navigation";
   Navigation_Module_ID : Module_ID;

   type Location_Marker_Array is array (Natural range <>) of Location_Marker;
   type Location_Marker_Array_Access is access Location_Marker_Array;

   type Navigation_Module_Record is new Module_ID_Record with record
      Markers        : Location_Marker_Array_Access;
      --  The list of markers from the history of locations

      Current_Marker : Natural := 0;
      --  The current position in Markers

      Last_Marker    : Natural := 0;
      --  The last marker set in Markers

      Back_Button    : Gtk.Widget.Gtk_Widget;
      Forward_Button : Gtk.Widget.Gtk_Widget;
      --  Back and forward buttons on the toolbar.
      --  ??? This might be put elsewhere.
   end record;
   type Navigation_Module is access all Navigation_Module_Record'Class;

   type Shell_Marker_Record is new Location_Marker_Record with record
      Script  : Scripting_Language;
      Command : GNAT.Strings.String_Access;
   end record;
   type Shell_Marker is access all Shell_Marker_Record'Class;

   function Create_Shell_Marker
     (Script  : access Scripting_Language_Record'Class;
      Command : String) return Shell_Marker;
   --  Create a new marker associated with a shell command

   function Go_To
     (Marker : access Shell_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean;
   procedure Destroy (Marker : in out Shell_Marker_Record);
   function To_String (Marker : access Shell_Marker_Record) return String;
   function Save (Marker : access Shell_Marker_Record) return Xml_Int.Node_Ptr;
   --  See inherited documentation

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Marker_Record'Class, Location_Marker);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Marker_Array, Location_Marker_Array_Access);

   procedure Destroy (Id : in out Navigation_Module_Record);
   --  Free memory associated to Id

   procedure Refresh_Location_Buttons
     (Handle : access Kernel_Handle_Record'Class);
   --  Refresh the active/inactive state of the location buttons

   procedure On_Back
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   procedure On_Forward
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callbacks for the back/forward buttons

   procedure On_Other_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open the spec if a body or separate is currently selected, and the spec
   --  otherwise.

   procedure On_Next_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Next Result menu

   procedure On_Previous_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Previous Result menu

   procedure On_Start_Statement
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Start Statement menu

   procedure On_End_Statement
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->End Statement menu

   procedure On_Next_Subprogram
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Next Subprogram menu

   procedure On_Previous_Subprogram
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Previous Subprogram menu

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the navigation module

   --  Interfaces to GPS commands used by some navigation procedures

   function Get_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural;
   --  Returns current line in File

   procedure Set_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural;
      Center : Boolean := False);
   --  Set current File's line, if Center is True the cursor is centered
   --  on the view.

   function Get_Last_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural;
   --  Returns last line index

   function Get_Block_Start
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural;
   --  Returns first line for block enclosing Line

   function Get_Block_End
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural;
   --  Returns last line for block enclosing Line

   function Get_Block_Type
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Language_Category;
   --  Returns type for block enclosing Line

   procedure On_Marker_Added_In_History
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a new marker is added in the history

   procedure Check_Marker_History_Status
     (Kernel           : access Kernel_Handle_Record'Class;
      Can_Move_Back    : out Boolean;
      Can_Move_Forward : out Boolean);
   --  Check whether it is possible to keep moving backward or forward in the
   --  list of markers.

   procedure Move_In_Marker_History
     (Kernel    : access Kernel_Handle_Record'Class;
      Move_Back : Boolean);
   --  Move backward or forward in the list of markers. The effect is
   --  immediately visible in the GPS interface.

   procedure Go_To_Current_Marker (Kernel : access Kernel_Handle_Record'Class);
   --  Go to the location pointed to by the current marker in the history

   procedure Save_History_Markers
     (Kernel : access Kernel_Handle_Record'Class);
   --  Save all locations to an XML file

   procedure Load_History_Markers
     (Kernel : access Kernel_Handle_Record'Class);
   --  Load all locations from an XML file

   --------------------------------
   -- On_Marker_Added_In_History --
   --------------------------------

   procedure On_Marker_Added_In_History
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D      : constant Marker_Hooks_Args_Access :=
                 Marker_Hooks_Args_Access (Data);
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      if Module.Markers = null then
         Module.Markers := new Location_Marker_Array
           (1 .. Max_Locations_In_History);
         Module.Current_Marker := 0;
         Module.Last_Marker := 0;
      end if;

      Module.Current_Marker := Module.Current_Marker + 1;

      if Module.Current_Marker > Module.Markers'Last then
         Destroy (Module.Markers (Module.Markers'First).all);
         Unchecked_Free (Module.Markers (Module.Markers'First));
         Module.Markers (Module.Markers'First .. Module.Markers'Last - 1) :=
           Module.Markers (Module.Markers'First + 1 .. Module.Markers'Last);
         Module.Current_Marker := Module.Markers'Last;
      end if;

      Module.Markers (Module.Current_Marker) := Location_Marker (D.Marker);
      Module.Last_Marker := Module.Current_Marker;
      Refresh_Location_Buttons (Kernel);
   end On_Marker_Added_In_History;

   --------------------------
   -- Save_History_Markers --
   --------------------------

   procedure Save_History_Markers
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename    : constant String := Get_Home_Dir (Kernel) & "locations.xml";
      M           : constant Navigation_Module :=
                      Navigation_Module (Navigation_Module_ID);
      File, Child : Node_Ptr;
      Success     : Boolean;

   begin
      if M.Markers /= null then
         Trace (Me, "Saving " & Filename);
         File     := new Node;
         File.Tag := new String'("Locations");

         for Index in M.Markers'First .. M.Last_Marker loop
            Child := Save (M.Markers (Index));
            if Child /= null then
               if Index = M.Current_Marker then
                  Set_Attribute (Child, "current", "true");
               end if;
               Add_Child (File, Child, Append => True);
            end if;
         end loop;

         Print (File, Filename, Success);
         Free (File);

         if not Success then
            Report_Preference_File_Error (Kernel, Filename);
         end if;
      end if;
   end Save_History_Markers;

   --------------------------
   -- Load_History_Markers --
   --------------------------

   procedure Load_History_Markers
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename    : constant String := Get_Home_Dir (Kernel) & "locations.xml";
      M           : constant Navigation_Module :=
                      Navigation_Module (Navigation_Module_ID);
      File, Child : Node_Ptr;
      Marker      : Location_Marker;
      Err         : String_Access;
   begin
      if GNAT.OS_Lib.Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);
         else
            M.Markers := new Location_Marker_Array
              (1 .. Max_Locations_In_History);
            M.Current_Marker := 0;
            M.Last_Marker := 0;

            Child := File.Child;
            while Child /= null loop
               Marker := Create_Marker (Kernel, Child);

               if Marker /= null then
                  M.Last_Marker := M.Last_Marker + 1;
                  M.Markers (M.Last_Marker) := Marker;
                  if Get_Attribute (Child, "current", "false") = "true" then
                     M.Current_Marker := M.Last_Marker;
                  end if;

                  exit when M.Last_Marker = Max_Locations_In_History;
               end if;

               Child := Child.Next;
            end loop;

            Free (File);
         end if;
      end if;
   end Load_History_Markers;

   ---------------------------------
   -- Check_Marker_History_Status --
   ---------------------------------

   procedure Check_Marker_History_Status
     (Kernel           : access Kernel_Handle_Record'Class;
      Can_Move_Back    : out Boolean;
      Can_Move_Forward : out Boolean)
   is
      pragma Unreferenced (Kernel);
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      Can_Move_Back := Module.Markers /= null
        and then Module.Current_Marker > Module.Markers'First;
      Can_Move_Forward := Module.Markers /= null
        and then Module.Current_Marker < Module.Last_Marker;
   end Check_Marker_History_Status;

   ----------------------------
   -- Move_In_Marker_History --
   ----------------------------

   procedure Move_In_Marker_History
     (Kernel    : access Kernel_Handle_Record'Class;
      Move_Back : Boolean)
   is
      pragma Unreferenced (Kernel);
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      if Move_Back then
         if Module.Markers /= null
           and then Module.Current_Marker > Module.Markers'First
         then
            Module.Current_Marker := Module.Current_Marker - 1;
         end if;
      else
         if Module.Markers /= null
           and then Module.Current_Marker < Module.Last_Marker
         then
            Module.Current_Marker := Module.Current_Marker + 1;
         end if;
      end if;
   end Move_In_Marker_History;

   --------------------------
   -- Go_To_Current_Marker --
   --------------------------

   procedure Go_To_Current_Marker
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Module : constant Navigation_Module :=
                 Navigation_Module (Navigation_Module_ID);
   begin
      if Module.Markers /= null
        and then Module.Current_Marker >= Module.Markers'First
        and then Module.Current_Marker <= Module.Last_Marker
      then
         if not Go_To (Module.Markers (Module.Current_Marker), Kernel) then
            Destroy (Module.Markers (Module.Current_Marker).all);
            Unchecked_Free (Module.Markers (Module.Current_Marker));
            Module.Markers (Module.Current_Marker .. Module.Last_Marker - 1) :=
              Module.Markers (Module.Current_Marker + 1 .. Module.Last_Marker);
            Module.Last_Marker := Module.Last_Marker - 1;
         end if;
      end if;
   end Go_To_Current_Marker;

   ----------------------
   -- Get_Current_Line --
   ----------------------

   function Get_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural
   is
      S_Line : constant String :=
                 Execute_GPS_Shell_Command
                   (Kernel, "Editor.cursor_get_line",
                    (1 => Full_Name (File).all'Unrestricted_Access));
   begin
      return Natural'Value (S_Line);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Current_Line;

   ----------------------
   -- Set_Current_Line --
   ----------------------

   procedure Set_Current_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural;
      Center : Boolean := False)
   is
      Line_Img : aliased String := Image (Line);
   begin
      Execute_GPS_Shell_Command
        (Kernel,
         "Editor.cursor_set_position",
         (Full_Name (File).all'Unrestricted_Access,
          Line_Img'Unchecked_Access));

      if Center then
         Execute_GPS_Shell_Command
           (Kernel,
            "Editor.cursor_center",
            (1 => Full_Name (File).all'Unrestricted_Access));
      end if;
   end Set_Current_Line;

   -------------------
   -- Get_Last_Line --
   -------------------

   function Get_Last_Line
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Natural
   is
      S_Line : constant String :=
                 Execute_GPS_Shell_Command
                   (Kernel, "Editor.get_last_line",
                    (1 => Full_Name (File).all'Unrestricted_Access));
   begin
      return Natural'Value (S_Line);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Last_Line;

   -------------------
   -- Get_Block_End --
   -------------------

   function Get_Block_End
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural
   is
      Line_Img : aliased String := Image (Line);
      S_Line   : constant String :=
                   Execute_GPS_Shell_Command
                     (Kernel,
                      "Editor.block_get_end",
                      (Full_Name (File).all'Unrestricted_Access,
                       Line_Img'Unchecked_Access));
   begin
      return Natural'Value (S_Line);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Block_End;

   ---------------------
   -- Get_Block_Start --
   ---------------------

   function Get_Block_Start
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Natural
   is
      Line_Img : aliased String := Image (Line);
      S_Line   : constant String :=
                   Execute_GPS_Shell_Command
                     (Kernel,
                      "Editor.block_get_start",
                      (Full_Name (File).all'Unrestricted_Access,
                       Line_Img'Unchecked_Access));
   begin
      return Natural'Value (S_Line);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Block_Start;

   --------------------
   -- Get_Block_Type --
   --------------------

   function Get_Block_Type
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Line   : Natural) return Language_Category
   is
      Line_Img : aliased String := Image (Line);
      B_Type   : constant String :=
                   Execute_GPS_Shell_Command
                     (Kernel,
                      "Editor.block_get_type",
                      (Full_Name (File).all'Unrestricted_Access,
                       Line_Img'Unchecked_Access));
   begin
      return Language_Category'Value (B_Type);
   exception
      when Constraint_Error =>
         return Cat_Unknown;
   end Get_Block_Type;

   -----------
   -- Go_To --
   -----------

   function Go_To
     (Marker : access Shell_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Kernel);
      Errors : Boolean;
   begin
      Execute_Command
        (Script       => Marker.Script,
         Command      => Marker.Command.all,
         Hide_Output  => True,
         Show_Command => False,
         Errors       => Errors);
      return not Errors;
   end Go_To;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Marker : in out Shell_Marker_Record) is
   begin
      Free (Marker.Command);
      Destroy (Location_Marker_Record (Marker));
   end Destroy;

   ---------------
   -- To_String --
   ---------------

   function To_String (Marker : access Shell_Marker_Record) return String is
   begin
      return Marker.Command.all;
   end To_String;

   ----------
   -- Save --
   ----------

   function Save
     (Marker : access Shell_Marker_Record) return Xml_Int.Node_Ptr
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("shell_mark");
      N.Value := new String'(Marker.Command.all);
      return N;
   end Save;

   -------------------------
   -- Create_Shell_Marker --
   -------------------------

   function Create_Shell_Marker
     (Script  : access Scripting_Language_Record'Class;
      Command : String) return Shell_Marker is
   begin
      return new Shell_Marker_Record'
        (Location_Marker_Record with
         Script  => Scripting_Language (Script),
         Command => new String'(Command));
   end Create_Shell_Marker;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Push_Marker_In_History
        (Get_Kernel (Data),
         Create_Shell_Marker
           (Script => Get_Script (Data),
            Command => Nth_Arg (Data, 1)));
   end Command_Handler;

   -------------
   -- On_Back --
   -------------

   procedure On_Back
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Move_In_Marker_History (Kernel, Move_Back => True);
      Go_To_Current_Marker (Kernel);
      Refresh_Location_Buttons (Kernel);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Back;

   ----------------
   -- On_Forward --
   ----------------

   procedure On_Forward
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Move_In_Marker_History (Kernel, Move_Back => False);
      Go_To_Current_Marker (Kernel);
      Refresh_Location_Buttons (Kernel);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Forward;

   ------------------------
   -- On_Start_Statement --
   ------------------------

   procedure On_Start_Statement
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context := Get_Current_Context (Kernel);
      File    : Virtual_File;
      Line    : Natural;           -- Current line being processed
      B_Start : Natural;           -- Block's first line
      B_Type  : Language_Category; -- Block's category

   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         File := File_Information (Context);

         Line   := Get_Current_Line (Kernel, File);
         B_Type := Get_Block_Type (Kernel, File, Line);

         if B_Type in Construct_Category
           or else B_Type in Enclosing_Entity_Category
         then
            B_Start := Get_Block_Start (Kernel, File, Line);

            if B_Start /= 0 then
               if B_Start /= Line then
                  Set_Current_Line (Kernel, File, B_Start);
               else
                  --  We are already at the start of the block, look for
                  --  enclosing block.
                  declare
                     EB_Start : Natural;
                  begin
                     EB_Start := Get_Block_Start (Kernel, File, Line - 1);

                     if EB_Start /= 0 then
                        Set_Current_Line (Kernel, File, EB_Start);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Start_Statement;

   ------------------------
   -- On_End_Statement --
   ------------------------

   procedure On_End_Statement
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context := Get_Current_Context (Kernel);
      File    : Virtual_File;
      Line    : Natural;           -- Current line being processed
      B_End   : Natural;           -- Block's first line
      B_Type  : Language_Category; -- Block's category

   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         File := File_Information (Context);

         Line   := Get_Current_Line (Kernel, File);
         B_Type := Get_Block_Type (Kernel, File, Line);

         if B_Type in Construct_Category
           or else B_Type in Enclosing_Entity_Category
         then
            B_End := Get_Block_End (Kernel, File, Line);

            if B_End /= 0 then
               if B_End /= Line then
                  Set_Current_Line (Kernel, File, B_End);
               else
                  --  We are already at the end of the block, look for
                  --  enclosing block.
                  declare
                     EB_End : Natural;
                  begin
                     EB_End := Get_Block_End (Kernel, File, Line + 1);

                     if EB_End /= 0 then
                        Set_Current_Line (Kernel, File, EB_End);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_End_Statement;

   ------------------------
   -- On_Next_Subprogram --
   ------------------------

   procedure On_Next_Subprogram
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context   : constant Selection_Context := Get_Current_Context (Kernel);
      File      : Virtual_File;
      Line      : Natural;           -- Current line being processed
      Last_Line : Natural;           -- Last line in the buffer
      B_Start   : Natural;           -- Block's first line
      B_Type    : Language_Category; -- Block's category

   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         File := File_Information (Context);

         Line      := Get_Current_Line (Kernel, File);
         Last_Line := Get_Last_Line (Kernel, File);

         while Line < Last_Line loop
            Line := Line + 1;

            B_Start := Get_Block_Start (Kernel, File, Line);
            B_Type  := Get_Block_Type (Kernel, File, Line);

            exit when B_Start = Line and then B_Type in Subprogram_Category;
         end loop;

         if Line < Last_Line then
            --  A block has been found, set cursor to its first line
            Set_Current_Line (Kernel, File, Line, Center => True);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Next_Subprogram;

   --------------------
   -- On_Next_Result --
   --------------------

   procedure On_Next_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Results : constant Location_View :=
                  Get_Or_Create_Location_View (Kernel, False);
   begin
      if Results /= null then
         Next_Item (Results);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Next_Result;

   ----------------------------
   -- On_Previous_Subprogram --
   ----------------------------

   procedure On_Previous_Subprogram
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context := Get_Current_Context (Kernel);
      File    : Virtual_File;
      Line    : Natural;           -- Current line being processed
      B_Start : Natural;           -- Block's first line
      B_Type  : Language_Category; -- Block's category

   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         File := File_Information (Context);

         Line := Get_Current_Line (Kernel, File);

         while Line > 1 loop
            Line := Line - 1;

            B_Type  := Get_Block_Type (Kernel, File, Line);
            B_Start := Get_Block_Start (Kernel, File, Line);

            exit when B_Start = Line and then B_Type in Subprogram_Category;
         end loop;

         if Line > 1 then
            --  A block has been found, set cursor to its first line
            Set_Current_Line (Kernel, File, Line, Center => True);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Previous_Subprogram;

   ------------------------
   -- On_Previous_Result --
   ------------------------

   procedure On_Previous_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Results : constant Location_View :=
                  Get_Or_Create_Location_View (Kernel, False);
   begin
      if Results /= null then
         Next_Item (Results, Backwards => True);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Previous_Result;

   -------------------
   -- On_Other_File --
   -------------------

   procedure On_Other_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context := Get_Current_Context (Kernel);
   begin
      Push_State (Kernel, Busy);

      if Has_File_Information (Context) then
         declare
            Other_File : constant Virtual_File := Create
              (Other_File_Base_Name
                 (Project_Information (Context), File_Information (Context)),
               Project_Information (Context));
         begin
            if Dir_Name (Other_File).all /= "" then
               Open_File_Editor (Kernel, Other_File, Line => 0);
            else
               Trace (Me, "Other file not found for "
                      & Full_Name (File_Information (Context)).all);
            end if;
         end;
      else
         Insert (Kernel, -"There is no selected file", Mode => Error);
      end if;

      Pop_State (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Kernel);
   end On_Other_File;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar            : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Navigate           : constant String := "/_" & (-"Navigate");
      Src_Action_Context : constant Action_Filter :=
                             Lookup_Filter (Kernel, "Source editor");
      Menu_Item          : Gtk_Menu_Item;
      Button             : Gtk_Tool_Button;
      Space              : Gtk_Separator_Tool_Item;
      --  Memory is never freed, but this is needed for the whole life of
      --  the application.
   begin
      Navigation_Module_ID := new Navigation_Module_Record;

      Register_Module
        (Module       => Navigation_Module_ID,
         Kernel       => Kernel,
         Module_Name  => Navigation_Module_Name,
         Priority     => High_Priority);

      Register_Menu
        (Kernel,
         Navigate,
         Ref_Item => -"Edit",
         Add_Before => False);

      Register_Command
        (Kernel, "add_location_command",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Command_Handler'Access);

      Register_Menu (Kernel, Navigate, -"Goto _File Spec<->Body",
                     Stock_Convert, On_Other_File'Access);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Navigate, Menu_Item);
      Register_Menu (Kernel, Navigate, -"_Start Of Statement",
                     Stock_Go_Up, On_Start_Statement'Access,
                     null, GDK_Up, Mod1_Mask,
                     Filter => Src_Action_Context);
      Register_Menu (Kernel, Navigate, -"_End Of Statement",
                     Stock_Go_Down, On_End_Statement'Access,
                     null, GDK_Down, Mod1_Mask,
                     Filter => Src_Action_Context);
      Register_Menu (Kernel, Navigate, -"Pre_vious Subprogram", "",
                     On_Previous_Subprogram'Access, null,
                     GDK_Up, Control_Mask,
                     Filter => Src_Action_Context);
      Register_Menu (Kernel, Navigate, -"Ne_xt Subprogram", "",
                     On_Next_Subprogram'Access, null, GDK_Down, Control_Mask,
                     Filter => Src_Action_Context);

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Navigate, Menu_Item);
      Register_Menu
        (Kernel, Navigate, -"Previous _Tag", "", On_Previous_Result'Access,
         Accel_Key  => GDK_less,
         Accel_Mods => Control_Mask);
      Register_Menu
        (Kernel, Navigate, -"N_ext Tag", "", On_Next_Result'Access,
         Accel_Key  => GDK_greater,
         Accel_Mods => Control_Mask);

      Register_Hook_No_Args (Kernel, Marker_Added_In_History_Hook);

      Register_Menu
        (Kernel, Navigate, -"Bac_k", "gps-navigate-back",
         On_Back'Access);
      Register_Menu
        (Kernel, Navigate, -"For_ward", "gps-navigate-forward",
         On_Forward'Access);
      Add_Hook (Kernel, Marker_Added_In_History_Hook,
                Wrapper (On_Marker_Added_In_History'Access),
                Name => "navigation.maker_added");

      Gtk_New (Space);
      Set_Draw (Space, True);
      Insert (Toolbar, Space, 9);

      Gtk_New_From_Stock (Button, "gps-navigate-back");
      Set_Tooltip (Button, Get_Tooltips (Kernel),
                   -"Goto Previous Location");
      Insert (Toolbar, Button, 10);
      Kernel_Callback.Connect
        (Button, Signal_Clicked, On_Back'Access, Kernel_Handle (Kernel));
      Navigation_Module (Navigation_Module_ID).Back_Button :=
        Gtk_Widget (Button);

      Gtk_New_From_Stock (Button, "gps-navigate-forward");
      Set_Tooltip (Button, Get_Tooltips (Kernel),
                   -"Goto Next Location");
      Insert (Toolbar, Button, 11);
      Kernel_Callback.Connect
        (Button, Signal_Clicked, On_Forward'Access, Kernel_Handle (Kernel));
      Navigation_Module (Navigation_Module_ID).Forward_Button :=
        Gtk_Widget (Button);

      Load_History_Markers (Kernel);
      Refresh_Location_Buttons (Kernel);
   end Register_Module;

   ------------------------------
   -- Refresh_Location_Buttons --
   ------------------------------

   procedure Refresh_Location_Buttons
     (Handle : access Kernel_Handle_Record'Class)
   is
      Data : constant Navigation_Module :=
               Navigation_Module (Navigation_Module_ID);
      Can_Move_Back, Can_Move_Forward : Boolean;
   begin
      Check_Marker_History_Status (Handle, Can_Move_Back, Can_Move_Forward);
      Set_Sensitive (Data.Back_Button, Can_Move_Back);
      Set_Sensitive (Data.Forward_Button, Can_Move_Forward);
   end Refresh_Location_Buttons;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Navigation_Module_Record) is
   begin
      Save_History_Markers (Get_Kernel (Id));

      if Id.Markers /= null then
         for M in Id.Markers'Range loop
            if Id.Markers (M) /= null then
               Destroy (Id.Markers (M).all);
               Unchecked_Free (Id.Markers (M));
            end if;
         end loop;
         Unchecked_Free (Id.Markers);
      end if;
   end Destroy;

end Navigation_Module;
