-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2004                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib.Xml_Int;        use Glib.Xml_Int;
with XML_Parsers;

with Gtk.Widget;          use Gtk.Widget;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;

with Gtkada.Canvas;       use Gtkada.Canvas;

with Debugger;
with GVD.Main_Window;     use GVD.Main_Window;
with GVD.Process;         use GVD.Process;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Call_Stack;      use GVD.Call_Stack;
with GVD.Code_Editors;    use GVD.Code_Editors;
with GVD.Trace;           use GVD.Trace;
with String_Utils;        use String_Utils;

with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Interactive_Consoles; use Interactive_Consoles;

package body GVD.Window_Settings is

   -----------------
   -- Local types --
   -----------------

   type String_Gint is new String;

   -----------------------
   -- List of constants --
   -----------------------
   --  Note: Below is the list of all the window settings that can be
   --  set. It is recommended to always access the preferences through these
   --  constant strings, since these are subject to change.
   --  Also, the type of the constant gives the type of the value associated
   --  with the setting.

   Main_Debug_Window_Height : constant String_Gint :=
     "Main_Debug_Window_Height";
   Main_Debug_Window_Width  : constant String_Gint :=
     "Main_Debug_Window_Width";

   ---------------------
   -- Local variables --
   ---------------------

   Current_Window_Settings : Node_Ptr;

   -----------------------
   -- Local subprograms --
   -----------------------
   procedure Set
     (Var : String_Gint; Value : Gint; Override : Boolean := False);
   --  Create a new entry in the current window settings, or modify the value
   --  of the existing one (only if Override is True)

   procedure Set (Var : String; Value : String; Override : Boolean := False);
   --  Ditto but with no type checking on Var.

   function Get_Setting (Name : String_Gint) return Gint;
   pragma Inline (Get_Setting);
   --  Retrieve the value of a given setting.

   --------------------------
   -- Load_Window_Settings --
   --------------------------

   procedure Load_Window_Settings
     (File_Name         : String)
   is
      Err : GNAT.OS_Lib.String_Access;
   begin
      if Current_Window_Settings /= null then
         Free (Current_Window_Settings);
      end if;

      XML_Parsers.Parse (File_Name, Current_Window_Settings, Err);

      if Current_Window_Settings = null then
         Output_Line (Err.all);
         Free (Err);
      end if;
   end Load_Window_Settings;

   --------------------------
   -- Save_Window_Settings --
   --------------------------

   procedure Save_Window_Settings
     (File_Name         : String;
      Main_Debug_Window : Gtk_Widget)
   is
      use type Debugger.Debugger_Access;

      Top           : constant GVD_Main_Window :=
        GVD_Main_Window (Main_Debug_Window);
      Process       : Visual_Debugger;

      Debugger_List : Debugger_List_Link := Top.First_Debugger;
      Debugger_Num  : Integer := 0;

   begin
      if Current_Window_Settings = null then
         Current_Window_Settings := new Node;
         Current_Window_Settings.Tag := new String'("GVD_Window_Settings");
      end if;

      Set (Main_Debug_Window_Width, Gint (Get_Allocation_Width (Top)), True);
      Set (Main_Debug_Window_Height, Gint (Get_Allocation_Height (Top)), True);

      while Debugger_List /= null loop
         Process := Visual_Debugger (Debugger_List.Debugger);

         if Process.Debugger /= null then
            declare
               Img : constant String := Image (Debugger_Num);
            begin
               if Process.Data_Canvas /= null then
                  Set (String_Gint ("Data_Height" & Img),
                       Gint (Get_Allocation_Height (Process.Data_Canvas)),
                       True);
                  Set (String_Gint ("Data_Width" & Img),
                       Gint (Get_Allocation_Width (Process.Data_Canvas)),
                       True);
               end if;

               if not Top.TTY_Mode then
                  if Process.Debugger_Text /= null then
                     Set (String_Gint ("Command_Height" & Img),
                          Gint (Get_Allocation_Height
                                  (Process.Debugger_Text)),
                          True);
                  end if;

                  if Process.Editor_Text /= null then
                     Set (String_Gint ("Editor_Height" & Img),
                          Gint (Get_Allocation_Height (Process.Editor_Text)),
                          True);
                  end if;
               end if;

               if Process.Stack /= null then
                  Set (String_Gint ("Stack_Mask" & Img),
                       Gint (Get_List_Mask (Process.Stack)),
                       True);
               end if;

               if Top.Standalone
                 and then Get_Pref (GVD_Prefs, Display_Explorer)
               then
                  Set (String_Gint ("Explorer_Width" & Img),
                       Gint (Get_Allocation_Width
                             (Get_Explorer_Scroll (Process.Editor_Text))),
                       True);
               end if;
            end;
         end if;

         Debugger_List := Debugger_List.Next;
         Debugger_Num := Debugger_Num + 1;
      end loop;

      Print (Current_Window_Settings, File_Name => File_Name);
   end Save_Window_Settings;

   -----------------
   -- Get_Setting --
   -----------------

   function Get_Setting (Name : String_Gint) return Gint is
      Node : constant Node_Ptr :=
        Find_Tag (Current_Window_Settings.Child, String (Name));
   begin
      if Node = null
        or else Node.Value = null
      then
         return -1;
      else
         return Gint'Value (Node.Value.all);
      end if;
   end Get_Setting;

   ---------
   -- Set --
   ---------

   procedure Set (Var : String; Value : String; Override : Boolean := False) is
      N : Node_Ptr := Find_Tag (Current_Window_Settings.Child, Var);
   begin
      if N = null then
         N := new Node;
         N.Tag := new String'(Var);
         N.Value := new String'(Value);
         Add_Child (Current_Window_Settings, N);

      elsif Override then
         Glib.Xml_Int.Free (N.Value);
         N.Value := new String'(Value);
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Gint; Value : Gint; Override : Boolean := False) is
   begin
      Set (String (Var), Trim (Gint'Image (Value), Left), Override);
   end Set;

   ------------------------------
   -- Get_Process_Tab_Geometry --
   ------------------------------

   function Get_Process_Tab_Geometry
     (Num : Integer) return Process_Tab_Geometry
   is
      Result : Process_Tab_Geometry;
      Image  : constant String := Trim (Integer'Image (Num), Left);

   begin
      Result.Data_Height := Get_Setting (String_Gint ("Data_Height" & Image));
      Result.Data_Width := Get_Setting (String_Gint ("Data_Width" & Image));
      Result.Command_Height := Get_Setting
        (String_Gint ("Command_Height" & Image));
      Result.Editor_Height := Get_Setting
        (String_Gint ("Editor_Height" & Image));
      Result.Stack_Width := Get_Setting (String_Gint ("Stack_Width" & Image));
      Result.Stack_Num_Width :=
        Get_Setting (String_Gint ("Stack_Num_Width" & Image));
      Result.Stack_PC_Width :=
        Get_Setting (String_Gint ("Stack_PC_Width" & Image));
      Result.Stack_Subprogram_Width :=
        Get_Setting (String_Gint ("Stack_Subprogram_Width" & Image));
      Result.Stack_Parameters_Width :=
        Get_Setting (String_Gint ("Stack_Parameters_Width" & Image));
      Result.Stack_Location_Width :=
        Get_Setting (String_Gint ("Stack_Location_Width" & Image));
      Result.Stack_Mask :=
        Get_Setting (String_Gint ("Stack_Mask" & Image));
      Result.Explorer_Width := Get_Setting
        (String_Gint ("Explorer_Width" & Image));

      --  Use default values if needed.

      if Result.Data_Height = -1 then
         Result.Data_Height := 200;
      end if;

      if Result.Data_Width = -1 then
         Result.Data_Width := 300;
      end if;

      if Result.Editor_Height = -1 then
         Result.Editor_Height := 200;
      end if;

      if Result.Stack_Width = -1 then
         Result.Stack_Width := 200;
      end if;

      if Result.Explorer_Width = -1 then
         Result.Explorer_Width := 200;
      end if;

      if Result.Stack_Num_Width = -1 then
         Result.Stack_Num_Width := 30;
      end if;

      if Result.Stack_PC_Width = -1 then
         Result.Stack_PC_Width := 30;
      end if;

      if Result.Stack_Subprogram_Width = -1 then
         Result.Stack_Subprogram_Width := 80;
      end if;

      if Result.Stack_Parameters_Width = -1 then
         Result.Stack_Parameters_Width := 80;
      end if;

      if Result.Stack_Location_Width = -1 then
         Result.Stack_Location_Width := 80;
      end if;

      if Result.Stack_Mask = -1 then
         Result.Stack_Mask := 6;
      end if;

      return Result;
   end Get_Process_Tab_Geometry;

end GVD.Window_Settings;
