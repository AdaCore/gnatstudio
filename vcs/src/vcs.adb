-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gtk.Widget; use Gtk.Widget;

package body VCS is

   -----------------------------
   -- Register_Error_Function --
   -----------------------------

   procedure Register_Error_Function
     (Rep  : access VCS_Record;
      Func : Error_Function;
      Data : Gtk.Widget.Gtk_Widget) is
   begin
      Rep.User_Data := Data;
      Rep.Local_Error_Function := Func;
   end Register_Error_Function;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Rep     : access VCS_Record;
      Message : String) is
   begin
      if Rep.Local_Error_Function /= null then
         Rep.Local_Error_Function (Message, Rep.User_Data);
      end if;
   end Set_Error;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access VCS_Record)
   is
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (S : in out String) is
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (F : in out File_Status_Record) is
      use String_List;
      use File_Status_List;
   begin
      Free (F.File_Name);
      Free (F.Working_Revision);
      Free (F.Repository_Revision);
      Free (F.Tags);
      Free (F.Users);
   end Free;

end VCS;
