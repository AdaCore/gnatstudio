------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      Make_Suite_Window_Pkg.Callbacks                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2001 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;
with Glib; use Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Clist; use Gtk.Clist;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Gtkada.Dialogs; use Gtkada.Dialogs;
with String_Utils; use String_Utils;

with Pixmaps_IDE; use Pixmaps_IDE;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Color;  use Gdk.Color;

with Aunit_Filters; use Aunit_Filters;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtkada.Types; use Gtkada.Types;

with Row_Data; use Row_Data;

package body Make_Suite_Window_Pkg.Callbacks is
   use Gtk.Arguments;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   ---------------------------------------
   -- On_Make_Suite_Window_Delete_Event --
   ---------------------------------------

   function On_Make_Suite_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
   begin
      Hide (Get_Toplevel (Object));
      Main_Quit;
      return True;
   end On_Make_Suite_Window_Delete_Event;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Suite_Window : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));

      S            : String := Get_Selection (Suite_Window.Explorer);

      File         : File_Type;
      Index        : Integer;
      Index_End    : Integer;
      Line         : String (1 .. 256);
      Line_Last    : Integer;
      Current_Name : String_Access;
      Row_Num      : Gint;
      Found        : Boolean := False;
   begin
      if S = "" then
         return;
      end if;

      Ada.Text_IO.Open (File, In_File, S);

      while not Found loop
         Get_Line (File, Line, Line_Last);
         Index_End := 1;
         Skip_To_String (Line, Index_End, " is");
         if Index_End < Line_Last - 1 then
            Index := 1;
            while Line (Index) = ' ' loop
               Index := Index + 1;
            end loop;
            Skip_To_String (Line, Index, " ");
            Index_End := Index + 1;
            while Line (Index_End) = ' ' loop
               Index_End := Index_End + 1;
            end loop;
            Skip_To_String (Line, Index_End, " ");
            Current_Name :=
              new String' (Line (Index + 1 .. Index_End - 1));
            Found := True;
         end if;
      end loop;

      Reset (File);
      Found := False;

      if S (S'Last - 3 .. S'Last) = ".ads" then
         loop
            Get_Line (File, Line, Line_Last);
            Index := 1;
            Skip_To_String (To_Lower (Line), Index, "type");
            if Index < Line_Last - 4 then
               Index_End := Index;
               Skip_To_String
                 (To_Lower (Line), Index_End, "test_case");
               if Index_End < Line_Last - 9 then
                  Index := 1;
                  Skip_To_String (To_Lower (Line), Index, "type ");
                  Index_End := Index + 5;
                  Skip_To_String
                    (To_Lower (Line), Index_End, " is ");
                  Row_Num :=
                    Append
                    (Suite_Window.Test_List,
                     Null_Array
                     + S
                     + ("(test) "
                        & Line (Index + 5 .. Index_End - 1)));
                  Set (Suite_Window.Test_List,
                       Row_Num,
                       Current_Name.all);
                  Found := True;
               end if;
            end if;
         end loop;
      else
         loop
            Get_Line (File, Line, Line_Last);
            Index := 1;
            Skip_To_String (To_Lower (Line), Index, "function");
            if Index < Line_Last - 8 then
               Index_End := Index;
               Skip_To_String
                 (To_Lower (Line), Index_End, "access_test_suite");
               if Index_End < Line_Last - 15 then
                  Index := 1;
                  Skip_To_String
                    (To_Lower (Line), Index, "function ");
                  Index_End := Index + 9;
                  Skip_To_String
                    (To_Lower (Line), Index_End, " return ");
                  Row_Num :=
                    Append
                    (Suite_Window.Test_List,
                     Null_Array
                     + S
                     + ("(suite) "
                        & Line (Index + 9 .. Index_End - 1)));
                  Set (Suite_Window.Test_List,
                       Row_Num,
                       Current_Name.all);
                  Found := True;
               end if;
            end if;
         end loop;
      end if;

      Free (Current_Name);
      Hide (Suite_Window.Explorer);
      Close (File);

   exception
      when End_Error =>
         Close (File);
         if not Found then
            Row_Num := Append
              (Suite_Window.Test_List,
               Null_Array + S + "(no test found)");
            Set (Suite_Window.Test_List,
                 Row_Num,
                 "");
         end if;
         Hide (Suite_Window.Explorer);
   end On_Ok_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Suite_Window : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));
   begin
      Hide (Suite_Window.Explorer);
   end On_Cancel_Button_Clicked;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Open explorer window to select components of the suite

      Suite_Window : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));

      Filter_A : Filter_Show_All_Access := new Filter_Show_All;
      Filter_B : Filter_Show_Ada_Access := new Filter_Show_Ada;
   begin
      if Suite_Window.Explorer = null then
         Gtk_New (Suite_Window.Explorer, "");
         Create_From_Xpm_D
           (Filter_B.Spec_Pixmap,
            Window => null,
            Colormap => Get_System,
            Mask => Filter_B.Spec_Bitmap,
            Transparent => Null_Color,
            Data => box_xpm);

         Create_From_Xpm_D
           (Filter_B.Body_Pixmap,
            Window => null,
            Colormap => Get_System,
            Mask => Filter_B.Body_Bitmap,
            Transparent => Null_Color,
            Data => package_xpm);

         Register_Filter (Suite_Window.Explorer, Filter_B);
         Register_Filter (Suite_Window.Explorer, Filter_A);

         Widget_Callback.Object_Connect
           (Get_Ok_Button (Suite_Window.Explorer),
            "clicked",
            Widget_Callback.To_Marshaller (On_Ok_Button_Clicked'Access),
            Gtk_Widget (Suite_Window));
         Widget_Callback.Object_Connect
           (Get_Cancel_Button (Suite_Window.Explorer),
            "clicked",
            Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access),
            Gtk_Widget (Suite_Window));
      end if;

      Show_All (Suite_Window.Explorer);
   end On_Add_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Remove selected files from the suite component list
      Suite_Window : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));

      use Gtk.Enums.Gint_List;

      List : Gtk.Enums.Gint_List.Glist :=
        Get_Selection (Suite_Window.Test_List);

   begin
      while List /= Null_List loop
         Remove (Suite_Window.Test_List, Get_Data (List));
         List := Next (List);
      end loop;
   end On_Remove_Clicked;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Generate suite source file.  Exit program if successful

      Win  : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));
      File : File_Type;
      Name : String := Get_Text (Win.Name_Entry);


      use Row_List;
      List : Row_List.Glist := Get_Row_List (Win.Test_List);
   begin
      if Name /= "" then
         if To_Lower (Name) = "test_suite" then
            if Message_Dialog
              ("The name of the suite cannot be ""Test_Suite""."
               & ASCII.LF & "Write the code anyways ?",
               Warning,
               Button_Yes or Button_No,
               Button_No,
               "",
               "Warning !") = Button_No
            then
               return;
            end if;
         end if;

         Mixed_Case (Name);

         if Is_Regular_File (To_File_Name (Name) & ".adb") then
            if Message_Dialog
              ("File " & To_File_Name (Name) & ".adb" & " exists. Overwrite?",
               Warning,
               Button_Yes or Button_No,
               Button_No,
               "",
               "Warning !") = Button_No
            then
               return;
            end if;
         end if;

         Ada.Text_IO.Create (File, Out_File, To_File_Name (Name) & ".adb");
         Put_Line
           (File, "with AUnit.Test_Suites; use AUnit.Test_Suites;");

         while List /= Null_List loop
            declare
               Package_Name : String := Get (Win.Test_List, Get_Data (List));
            begin
               if Package_Name /= "" then
                  Mixed_Case (Package_Name);
                  Put_Line (File, "with " & Package_Name & ";");
               end if;
            end;
            List := Next (List);
         end loop;

         New_Line (File);

         Put_Line
           (File,
            "function "
            & Name & " return Access_Test_Suite is"
            & ASCII.LF
            & "   Result : Access_Test_Suite := new Test_Suite;"
            & ASCII.LF
            & "begin");

         List := Get_Row_List (Win.Test_List);

         while List /= Null_List loop
            declare
               S : String := Get_Text (Win.Test_List, Get_Data (List), 1);
               Package_Name : String := Get (Win.Test_List, Get_Data (List));

            begin
               if Package_Name /= "" then
                  Mixed_Case (S);
                  Mixed_Case (Package_Name);
                  if S (S'First .. S'First + 2) = "(te" then
                     Put_Line
                       (File,
                        "   Add_Test (Result, new "
                        & Package_Name
                        & "." & S (S'First + 7 .. S'Last)
                        & ");");
                  end if;

                  if S (S'First .. S'First + 2) = "(su" then
                     Put_Line
                       (File, "   Add_Test (Result, " & Package_Name &");");
                  end if;
               end if;
            end;
            List := Next (List);
         end loop;

         Put_Line
           (File, "   return Result;" & ASCII.LF & "end " & Name & ';');
         Close (File);
         Win.Name := new String' (To_Lower (Name));
      end if;

      Hide (Win);
      Main_Quit;
   end On_Ok_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Get_Toplevel (Object));
      Main_Quit;
   end On_Cancel_Clicked;

   ---------------------
   -- On_Help_Clicked --
   ---------------------

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Clicked;

end Make_Suite_Window_Pkg.Callbacks;
