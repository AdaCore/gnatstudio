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

with Glib.Object; use Glib.Object;
with Generic_List;

package body VCS is

   procedure Free (Identifier : in out VCS_Id_Identifier);
   --  Dummy function used to instanciate Identifiers list.

   package Identifiers is new Generic_List (VCS_Id_Identifier);

   Identifiers_List : Identifiers.List;

   ----------
   -- Free --
   ----------

   procedure Free (Identifier : in out VCS_Id_Identifier) is
   begin
      null;
   end Free;

   -----------------------------
   -- Register_VCS_Identifier --
   -----------------------------

   procedure Register_VCS_Identifier (Identifier : VCS_Id_Identifier) is
   begin
      Identifiers.Append (Identifiers_List, Identifier);
   end Register_VCS_Identifier;

   ---------------------
   -- Get_VCS_From_Id --
   ---------------------

   function Get_VCS_From_Id (Id : String) return VCS_Access
   is
      Result : VCS_Access := null;
      Temp   : Identifiers.List := Identifiers_List;
   begin
      while not Identifiers.Is_Empty (Temp) loop
         Result := Identifiers.Head (Temp) (Id);

         if Result /= null then
            return Result;
         end if;

         Temp := Identifiers.Next (Temp);
      end loop;

      return Result;
   end Get_VCS_From_Id;

   -----------------------------
   -- Register_Error_Function --
   -----------------------------

   procedure Register_Error_Function
     (Func : Error_Function;
      Data : Glib.Object.GObject) is
   begin
      Caller_Widget := Data;
      The_Error_Function := Func;
   end Register_Error_Function;

   ----------------------------
   -- Register_Idle_Function --
   ----------------------------

   procedure Register_Idle_Function
     (Func    : Idle_Function;
      Timeout : Integer := 200) is
   begin
      The_Idle_Function := Func;
      VCS.Timeout := Timeout;
   end Register_Idle_Function;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Rep     : access VCS_Record;
      Message : String) is
   begin
      if The_Error_Function /= null then
         The_Error_Function (Message, Caller_Widget);
      end if;
   end Set_Error;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access VCS_Record) is
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
