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

with Gtk.Widget;              use Gtk.Widget;
with Glib;                    use Glib;
with Glib.XML;

with Main_Debug_Window_Pkg;   use Main_Debug_Window_Pkg;
with GVD.Memory_View;         use GVD.Memory_View;
with GVD.Dialogs;             use GVD.Dialogs;

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

   --------------------
   -- Local packages --
   --------------------

   package Gint_Xml is new Glib.XML (Gint);
   use Gint_Xml;

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
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Main_Debug_Window);

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
   end Load_Window_Settings;

   --------------------------
   -- Save_Window_Settings --
   --------------------------

   procedure Save_Window_Settings
     (File_Name         : String;
      Main_Debug_Window : Gtk_Widget)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Main_Debug_Window);
   begin
      if Current_Window_Settings = null then
         Current_Window_Settings := new Node;
         Current_Window_Settings.Tag := new String' ("GVD_Window_Settings");
      end if;

      Set (Main_Debug_Window_Width, Gint (Get_Allocation_Width (Top)), True);
      Set (Main_Debug_Window_Height, Gint (Get_Allocation_Height (Top)), True);

      Set (Memory_View_Width,
           Gint (Get_Allocation_Width (Top.Memory_View)),
           True);
      Set (Memory_View_Height,
           Gint (Get_Allocation_Height (Top.Memory_View)),
           True);

      Set (History_Dialog_Width,
           Gint (Get_Allocation_Width (Top.History_Dialog)),
           True);
      Set (History_Dialog_Height,
           Gint (Get_Allocation_Height (Top.History_Dialog)),
           True);

      Set (Task_Dialog_Width,
           Gint (Get_Allocation_Width (Top.Task_Dialog)),
           True);
      Set (Task_Dialog_Height,
           Gint (Get_Allocation_Height (Top.Task_Dialog)),
           True);
      Print (Current_Window_Settings, File_Name => File_Name);
   end Save_Window_Settings;

   -----------------
   -- Get_Setting --
   -----------------

   function Get_Setting (Name : String_Gint) return Gint is
      Node : constant Node_Ptr :=
        Find_Tag (Current_Window_Settings.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Gint'Value (Node.Value.all);
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
      Set (String (Var), Gint'Image (Value), Override);
   end Set;

end GVD.Window_Settings;
