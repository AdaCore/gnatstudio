-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

with Search_Callback;       use Search_Callback;
with Find_Utils;            use Find_Utils;

with GNAT.Regexp;           use GNAT.Regexp;
with GNAT.OS_Lib;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;

with Gtkada.File_Selection; use Gtkada.File_Selection;

with Gtk.Editable;

with Gtk.Main;

package body Hyper_Grep_Base_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------
   -- Local sub-programs --
   ------------------------

   procedure Close_Window
     (Hyper_Grep_Window : access Hyper_Grep_Base_Record'Class);
   --  Close the window and quit the main loop.

   function Get_Project_Files return Project_Files_Access;
   --  Return the project file list.

   ------------------
   -- Close_Window --
   ------------------

   procedure Close_Window
     (Hyper_Grep_Window : access Hyper_Grep_Base_Record'Class)
   is
   begin
      Destroy (Hyper_Grep_Window);

      Gtk.Main.Main_Quit;
   end Close_Window;

   -----------------------
   -- Get_Project_Files --
   -----------------------

   function Get_Project_Files return Project_Files_Access is
   begin
      return null;
   end Get_Project_Files;

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Hyper_Grep_Window : constant Hyper_Grep_Base_Access :=
        Hyper_Grep_Base_Access (Get_Toplevel (Object));

      S : constant String :=
        File_Selection_Dialog
         ("Select a directory",
          "." & GNAT.OS_Lib.Directory_Separator,
          Dir_Only   => True,
          Must_Exist => True);
   begin
      if S /= "" then
         Set_Text (Hyper_Grep_Window.Directory_Entry, S);
      end if;
   end On_Browse_Button_Clicked;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Close_Window (Hyper_Grep_Base_Access (Get_Toplevel (Object)));
   end On_Close_Button_Clicked;

   --------------------------------
   -- On_Hyper_Grep_Delete_Event --
   --------------------------------

   function On_Hyper_Grep_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Close_Window (Hyper_Grep_Base_Access (Object));
      return False;
   end On_Hyper_Grep_Delete_Event;

   -----------------------------------
   -- On_Only_Project_Check_Toggled --
   -----------------------------------

   procedure On_Only_Project_Check_Toggled
     (Object : access Gtk_Check_Button_Record'Class)
   is
      Hyper_Grep_Window : constant Hyper_Grep_Base_Access :=
        Hyper_Grep_Base_Access (Get_Toplevel (Object));

      --  NOTE: We assume the sensitivity of these widgets is in phase with
      --        the checkbox status since initialization (this must be done
      --        with glade).

      Active : Boolean := not Get_Active (Object);
   begin
      Set_Sensitive (Hyper_Grep_Window.Directory_Combo, Active);
      Set_Sensitive (Hyper_Grep_Window.Browse_Button,   Active);
      Set_Sensitive (Hyper_Grep_Window.Subdirs_Check,   Active);
      Set_Sensitive (Hyper_Grep_Window.Files_Combo,     Active);
   end On_Only_Project_Check_Toggled;

   -----------------------------
   -- On_Start_Button_Clicked --
   -----------------------------

   procedure On_Start_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      S  : Code_Search;
      RE : Regexp;

      Hyper_Grep_Window : constant Hyper_Grep_Base_Access :=
        Hyper_Grep_Base_Access (Get_Toplevel (Object));

      Scope : Search_Scope;

      use Gtk.Editable;
   begin
      if Get_Active (Hyper_Grep_Window.Whole_Rbutton) then
         Scope := Whole;
      elsif Get_Active (Hyper_Grep_Window.Comm_Only_Rbutton) then
         Scope := Comm_Only;
      elsif Get_Active (Hyper_Grep_Window.Strings_Rbutton) then
         Scope := Str_Only;
      elsif Get_Active (Hyper_Grep_Window.Comm_Str_Rbutton) then
         Scope := Comm_Str;
      else
         Scope := All_But_Comm;
      end if;

      if Get_Active (Hyper_Grep_Window.Only_Project_Check) then
         Init_Search
           (S,
            Get_Chars (Hyper_Grep_Window.Pattern_Entry),
            Get_Project_Files,
            Get_Active (Hyper_Grep_Window.Case_Check),
            Get_Active (Hyper_Grep_Window.Whole_Word_Check),
            Get_Active (Hyper_Grep_Window.Regexp_Check),
            Scope);
      else
         RE := Compile
           (Get_Chars (Hyper_Grep_Window.Files_Entry), Glob => True);

         Init_Search
           (S,
            Get_Chars (Hyper_Grep_Window.Pattern_Entry),
            RE,
            Get_Chars (Hyper_Grep_Window.Directory_Entry),
            Get_Active (Hyper_Grep_Window.Subdirs_Check),
            Get_Active (Hyper_Grep_Window.Case_Check),
            Get_Active (Hyper_Grep_Window.Whole_Word_Check),
            Get_Active (Hyper_Grep_Window.Regexp_Check),
            Scope);
      end if;

      Reset_Search;
      Do_Search (S, Callback'Access);
      Put_Line ("====== End of search");
      Free (S);

   exception
      when Error_In_Regexp =>
         Put_Line ("--- Bad globbing pattern: '"
                   & Get_Chars (Hyper_Grep_Window.Files_Entry)
                   & "'");

      when E : others =>
         Put_Line ("--- Exception >>>");
         Put_Line (Exception_Information (E));
         Put_Line ("--- <<< Exception");
   end On_Start_Button_Clicked;

   ----------------------------
   -- On_Stop_Button_Clicked --
   ----------------------------

   procedure On_Stop_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Abort_Search;
   end On_Stop_Button_Clicked;

end Hyper_Grep_Base_Pkg.Callbacks;
