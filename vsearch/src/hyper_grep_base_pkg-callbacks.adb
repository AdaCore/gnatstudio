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

with Gtk.Widget; use Gtk.Widget;

with Find_Utils;            use Find_Utils;
with Hyper_Grep;            use Hyper_Grep;
with Glide_Pkg;             use Glide_Pkg;
with Search_Callback;

with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.Regexp;           use GNAT.Regexp;
with GNAT.OS_Lib;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;

with Gtkada.File_Selection; use Gtkada.File_Selection;
with Gdk.Color;             use Gdk.Color;
with Gtk.Text;              use Gtk.Text;
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
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
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
      Set_Sensitive (Hyper_Grep_Window.Files_Label,     Active);
      Set_Sensitive (Hyper_Grep_Window.Directory_Label, Active);
   end On_Only_Project_Check_Toggled;

   -----------------------------
   -- On_Start_Button_Clicked --
   -----------------------------

   procedure On_Start_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Highlight_File : constant String := "#FF0000000000";

      S         : Code_Search;
      RE        : Regexp;
      Continue  : Boolean := True;
      Highlight : Gdk_Color;


      Hyper_Grep_Window : constant Hyper_Grep_Access :=
        Hyper_Grep_Access (Get_Toplevel (Object));

      Scope : Search_Scope;

      procedure Reset_Search;
      --  Call it before every search using this callback.

      procedure Abort_Search;
      --  Call it to abort the search. The next time the callback is called, it
      --  will abort the search.

      function Callback
        (Match_Found : Boolean;
         File        : String;
         Line_Nr     : Positive    := 1;
         Line_Text   : String      := "";
         Sub_Matches : Match_Array := (0 => No_Match)) return Boolean;
      --  Print every match 'file:line:text'; ignore file calls.
      --  Handle Gtk pending events.

      ------------------
      -- Abort_Search --
      ------------------

      procedure Abort_Search is
      begin
         Continue := False;
      end Abort_Search;

      --------------
      -- Callback --
      --------------

      function Callback
        (Match_Found : Boolean;
         File        : String;
         Line_Nr     : Positive    := 1;
         Line_Text   : String      := "";
         Sub_Matches : Match_Array := (0 => No_Match)) return Boolean
      is
         Dummy       : Boolean;
         Parentheses : String (Line_Text'Range);

         use Ada.Strings;

         Location    : constant String :=
           File & ':' & Fixed.Trim (Positive'Image (Line_Nr), Left);
         Blanks      : constant String (Location'Range) := (others => ' ');

      begin
         if Match_Found then
            Insert (Glide_Access (Hyper_Grep_Window.Glide).Console,
                    Fore => Highlight,
                    Chars => Location);
            Insert (Glide_Access (Hyper_Grep_Window.Glide).Console,
                    Chars => ':' & Line_Text & ASCII.LF);

            if Sub_Matches (0) /= No_Match then
               for K in Sub_Matches'Range loop
                  Insert (Glide_Access (Hyper_Grep_Window.Glide).Console,
                          Chars => Natural'Image (K));
                  --  Natural_IO.Put (K, Width => Location'Length);

                  Parentheses := (others => ' ');

                  if Sub_Matches (K) /= No_Match then
                     Parentheses
                       (Sub_Matches (K).First .. Sub_Matches (K).Last) :=
                         (others => '#');
                  end if;

                  Insert (Glide_Access (Hyper_Grep_Window.Glide).Console,
                          Chars => '>' & Parentheses & '<');
               end loop;
            end if;
         end if;

         while Gtk.Main.Events_Pending loop
            Dummy := Gtk.Main.Main_Iteration;
         end loop;

         if Continue then
            return True;
         else
            return False;
         end if;
      end Callback;

      ------------------
      -- Reset_Search --
      ------------------

      procedure Reset_Search is
      begin
         Continue := True;
      end Reset_Search;

   begin
      Highlight := Parse (Highlight_File);
      Alloc (Get_Default_Colormap, Highlight);

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

      Set_Sensitive (Hyper_Grep_Window.Stop_Button,  True);
      Set_Sensitive (Hyper_Grep_Window.Start_Button, False);

      if Hyper_Grep_Window.Glide = null then
         Search_Callback.Reset_Search;
         Do_Search (S, Search_Callback.Callback'Access);
      else
         Reset_Search;
         Do_Search (S, Callback'Unrestricted_Access);
      end if;

      Set_Sensitive (Hyper_Grep_Window.Stop_Button,  False);
      Set_Sensitive (Hyper_Grep_Window.Start_Button, True);
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
      --  ??? Abort_Search;
      null;
   end On_Stop_Button_Clicked;

end Hyper_Grep_Base_Pkg.Callbacks;
