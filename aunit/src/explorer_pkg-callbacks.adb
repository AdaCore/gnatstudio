------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          Explorer_Pkg.Callbacks                          --
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

with Glib; use Glib;
with Gtk.Widget; use Gtk.Widget;

with Gtkada.Types; use Gtkada.Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Make_Suite_Window_Pkg; use Make_Suite_Window_Pkg;
with Row_Data; use Row_Data;

package body Explorer_Pkg.Callbacks is
   --  Handle callbacks from explorer window for composing a test_suite

   use Gtk.Arguments;

   -------------------------
   -- On_Clist_Select_Row --
   -------------------------

   procedure On_Clist_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Handle selection of a file (test_case or test_suite)

      function Dir_Name (Old_Dir : String;
                         New_Dir : String) return String;
      --  Construct directory path string

      Explorer : constant Explorer_Access :=
        Explorer_Access (Get_Toplevel (Object));
      Suite_Window : Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Explorer.Suite_Window);
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);

      function Dir_Name (Old_Dir : String;
                         New_Dir : String) return String
      is
         --  Construct directory path string
      begin
         if New_Dir = "." then
            return Old_Dir;
         end if;
         if New_Dir = ".." then
            declare
               Index : Integer := Old_Dir'Last;
            begin
               while Index > Old_Dir'First loop
                  if Old_Dir (Index) = Directory_Separator then
                     exit;
                  end if;
                  Index := Index - 1;
               end loop;
               if Index > Old_Dir'First then
                  if Index = Old_Dir'Last - 2
                    and then Old_Dir (Index .. Old_Dir'Last)
                             = Directory_Separator & ".."
                  then
                     return Old_Dir & Directory_Separator & "..";
                  else
                     return Old_Dir (Old_Dir'First .. Index - 1);
                  end if;
               end if;
            end;
         end if;
         return Old_Dir & Directory_Separator & New_Dir;
      end Dir_Name;

   begin
      if Arg2 = 0 or else Arg2 = 1 then
         declare
            Name       : String := Get_Text (Object, Arg1, 0);
            Old_Dir    : String := Explorer.Directory.all;
            Dummy_Gint : Gint;

         begin
            if Is_Directory (Dir_Name (Old_Dir, Name)) then
               Free (Explorer.Directory);
               Explorer.Directory := new String'
                 (Dir_Name (Old_Dir, Name));
               Fill (Explorer);

            else
               Dummy_Gint :=
                 Append (Suite_Window.Test_List,
                         Null_Array
                         + (Explorer.Directory.all
                            & Directory_Separator
                            & Get_Text (Object, Arg1, 0))
                         + Get_Text (Object, Arg1, 1) + "");
               Set (Suite_Window.Test_List,
                    Dummy_Gint,
                    Get (Object, Arg1));
               Dummy_Gint :=
                 Columns_Autosize (Suite_Window.Test_List);
            end if;
         end;
      end if;
   end On_Clist_Select_Row;

   ----------------------
   -- On_Close_Clicked --
   ----------------------

   procedure On_Close_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Explorer : constant Explorer_Access :=
        Explorer_Access (Get_Toplevel (Object));

   begin
      Hide_All (Explorer);
   end On_Close_Clicked;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Explorer : constant Explorer_Access :=
        Explorer_Access (Get_Toplevel (Object));

   begin
      Hide_All (Explorer);
   end On_Ok_Clicked;

end Explorer_Pkg.Callbacks;
