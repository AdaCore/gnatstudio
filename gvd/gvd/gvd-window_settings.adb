-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Gint_Xml;            use Gint_Xml;

with Gtk.Clist;           use Gtk.Clist;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Item_Factory;    use Gtk.Item_Factory;

with Gtkada.Canvas;       use Gtkada.Canvas;

with GVD.Main_Window;     use GVD.Main_Window;
with GVD.Memory_View;     use GVD.Memory_View;
with GVD.Dialogs;         use GVD.Dialogs;
with GVD.Process;         use GVD.Process;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Code_Editors;    use GVD.Code_Editors;
with GVD.Trace;           use GVD.Trace;
with Odd_Intl;            use Odd_Intl;

with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;

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
   Memory_View_Height       : constant String_Gint := "Memory_View_Height";
   Memory_View_Width        : constant String_Gint := "Memory_View_Width";
   History_Dialog_Height    : constant String_Gint := "History_Dialog_Height";
   History_Dialog_Width     : constant String_Gint := "History_Dialog_Width";
   Task_Dialog_Height       : constant String_Gint := "Task_Dialog_Height";
   Task_Dialog_Width        : constant String_Gint := "Task_Dialog_Width";

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
     (File_Name         : String;
      Main_Debug_Window : Gtk_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Main_Debug_Window);
   begin
      if Current_Window_Settings /= null then
         Free (Current_Window_Settings);
      end if;

      Current_Window_Settings := Parse (File_Name);

      Set_Default_Size (Top,
                        Get_Setting (Main_Debug_Window_Width),
                        Get_Setting (Main_Debug_Window_Height));

      Set_Default_Size (Top.Memory_View,
                        Get_Setting (Memory_View_Width),
                        Get_Setting (Memory_View_Height));

      Set_Default_Size (Top.History_Dialog,
                        Get_Setting (History_Dialog_Width),
                        Get_Setting (History_Dialog_Height));

      Set_Default_Size (Top.Task_Dialog,
                        Get_Setting (Task_Dialog_Width),
                        Get_Setting (Task_Dialog_Height));

   --  If we couldn't parse the file (maybe it was empty).
   --  However, we don't delete the file, so that manual recovery can still be
   --  done if needed.
   exception
      when others =>
         Output_Line (Top, "Couldn't load window_settings file");
         Current_Window_Settings := null;
   end Load_Window_Settings;

   --------------------------
   -- Save_Window_Settings --
   --------------------------

   procedure Save_Window_Settings
     (File_Name         : String;
      Main_Debug_Window : Gtk_Widget)
   is
      Top       : constant GVD_Main_Window :=
        GVD_Main_Window (Main_Debug_Window);
      Process   : Debugger_Process_Tab;
      Page      : Gtk_Widget;
      Num_Pages : constant Gint :=
        Gint (Page_List.Length (Get_Children (Top.Process_Notebook)));
      Widget    : Gtk_Widget;
      A         : Guint;

   begin
      if Current_Window_Settings = null then
         Current_Window_Settings := new Node;
         Current_Window_Settings.Tag := new String' ("GVD_Window_Settings");
      end if;

      Set (Main_Debug_Window_Width, Gint (Get_Allocation_Width (Top)), True);
      Set (Main_Debug_Window_Height, Gint (Get_Allocation_Height (Top)), True);

      if Get_Allocation_Width (Top.Memory_View) /= 1
        and then Get_Allocation_Height (Top.Memory_View) /= 1
      then
         Set (Memory_View_Width,
              Gint (Get_Allocation_Width (Top.Memory_View)),
              True);
         Set (Memory_View_Height,
              Gint (Get_Allocation_Height (Top.Memory_View)),
              True);
      end if;

      if Get_Allocation_Width (Top.History_Dialog) /= 1
        and then Get_Allocation_Height (Top.History_Dialog) /= 1
      then
         Set (History_Dialog_Width,
              Gint (Get_Allocation_Width (Top.History_Dialog)),
              True);
         Set (History_Dialog_Height,
              Gint (Get_Allocation_Height (Top.History_Dialog)),
              True);
      end if;

      if Get_Allocation_Width (Top.Task_Dialog) /= 1
        and then Get_Allocation_Height (Top.Task_Dialog) /= 1
      then
         Set (Task_Dialog_Width,
              Gint (Get_Allocation_Width (Top.Task_Dialog)),
              True);
         Set (Task_Dialog_Height,
              Gint (Get_Allocation_Height (Top.Task_Dialog)),
              True);
      end if;

      for Page_Num in 0 .. Num_Pages - 1 loop
         Page := Get_Nth_Page (Top.Process_Notebook, Page_Num);

         if Page /= null then
            Process := Process_User_Data.Get (Page);

            declare
               Image : constant String := Trim (Gint'Image (Page_Num), Left);
            begin
               Set (String_Gint ("Data_Height" & Image),
                    Gint (Get_Allocation_Height (Process.Data_Canvas)),
                    True);
               Set (String_Gint ("Data_Width" & Image),
                    Gint (Get_Allocation_Width (Process.Data_Canvas)),
                    True);

               if not Top.TTY_Mode then
                  Set (String_Gint ("Command_Height" & Image),
                       Gint (Get_Allocation_Height
                             (Process.Command_Scrolledwindow)),
                       True);
                  Set (String_Gint ("Editor_Height" & Image),
                       Gint (Get_Allocation_Height (Process.Editor_Text)),
                       True);
               end if;

               Widget := Get_Widget (Top.Factory, -"/Data/Call Stack");

               if Widget = null then
                  --  This means that GVD is part of Glide
                  Widget :=
                    Get_Widget (Top.Factory, -"/Debug/Data/Call Stack");
               end if;

               if Get_Active (Gtk_Check_Menu_Item (Widget)) then
                  Set (String_Gint ("Stack_Width" & Image),
                       Gint (Get_Allocation_Width
                             (Process.Stack_Scrolledwindow)),
                       True);

                  if Process.Backtrace_Mask / (2 ** 1) = 0 then
                     A := 2;
                  else
                     A := 1;
                  end if;

                  Set (String_Gint
                       ("Stack_Num_Width"
                        & Image (Image'First + 1 .. Image'Last)),
                       Gint (Get_Allocation_Width
                             (Get_Column_Widget
                              (Process.Stack_List, 0)) - A),
                       True);

                  if Process.Backtrace_Mask / (2 ** 2) = 0 then
                     A := 2;
                  else
                     A := 1;
                  end if;

                  Set (String_Gint
                       ("Stack_PC_Width"
                        & Image (Image'First + 1 .. Image'Last)),
                       Gint (Get_Allocation_Width
                             (Get_Column_Widget
                              (Process.Stack_List, 1)) - A),
                       True);

                  if Process.Backtrace_Mask / (2 ** 3) = 0 then
                     A := 2;
                  else
                     A := 1;
                  end if;

                  Set (String_Gint
                       ("Stack_Subprogram_Width"
                        & Image (Image'First + 1 .. Image'Last)),
                       Gint (Get_Allocation_Width
                             (Get_Column_Widget
                              (Process.Stack_List, 2)) - A),
                       True);

                  if Process.Backtrace_Mask / (2 ** 4) = 0 then
                     A := 2;
                  else
                     A := 1;
                  end if;

                  Set (String_Gint
                       ("Stack_Parameters_Width"
                        & Image (Image'First + 1 .. Image'Last)),
                       Gint (Get_Allocation_Width
                             (Get_Column_Widget
                              (Process.Stack_List, 3)) - A),
                       True);

                  if Process.Backtrace_Mask / (2 ** 5) = 0 then
                     A := 2;
                  else
                     A := 1;
                  end if;

                  Set (String_Gint
                       ("Stack_Location_Width"
                        & Image (Image'First + 1 .. Image'Last)),
                       Gint (Get_Allocation_Width
                             (Get_Column_Widget
                              (Process.Stack_List, 4)) - A),
                       True);

                  Set (String_Gint
                       ("Stack_Mask" &
                        Image (Image'First + 1 .. Image'Last)),
                       Gint (Process.Backtrace_Mask),
                       True);
               end if;

               if Top.Standalone and then Get_Pref (Display_Explorer) then
                  Set (String_Gint ("Explorer_Width" & Image),
                       Gint (Get_Allocation_Width
                             (Get_Explorer_Scroll (Process.Editor_Text))),
                       True);
               end if;
            end;
         end if;
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
         N.Tag := new String' (Var);
         N.Value := new String' (Value);
         Add_Child (Current_Window_Settings, N);

      elsif Override then
         Gint_Xml.Free (N.Value);
         N.Value := new String' (Value);
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
     (Page : Gint) return Process_Tab_Geometry
   is
      Result : Process_Tab_Geometry;
      Image  : constant String := Trim (Gint'Image (Page), Left);

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
