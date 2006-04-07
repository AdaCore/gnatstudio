-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Glib;      use Glib;
with GUI_Utils; use GUI_Utils;

package body Password_Manager is

   type Password_Record;
   type Password_Access is access all Password_Record;

   type Passphrase_Record;
   type Passphrase_Access is access all Passphrase_Record;

   type Password_Record is record
      Machine   : String_Ptr;
      User_Name : String_Ptr;
      Password  : String_Ptr;
      Next      : Password_Access;
   end record;

   type Passphrase_Record is record
      Key_Id     : String_Ptr;
      Passphrase : String_Ptr;
      Next       : Passphrase_Access;
   end record;

   Password_List : Password_Access := null;
   Passphrase_List : Passphrase_Access := null;

   Password_Regexp : constant Pattern_Matcher :=
                       Compile ("^[^\n]*[Pp]assword: *$",
                                Multiple_Lines or Single_Line);
   Passphrase_Regexp : constant Pattern_Matcher :=
                         Compile ("^[^\n]*[Pp]assphrase for key '([^']*)': *$",
                                  Multiple_Lines or Single_Line);

   ---------------------------------
   -- Get_Default_Password_Regexp --
   ---------------------------------

   function Get_Default_Password_Regexp return Pattern_Matcher is
   begin
      return Password_Regexp;
   end Get_Default_Password_Regexp;

   -----------------------------------
   -- Get_Default_Passphrase_Regexp --
   -----------------------------------

   function Get_Default_Passphrase_Regexp return Pattern_Matcher is
   begin
      return Passphrase_Regexp;
   end Get_Default_Passphrase_Regexp;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password
     (Parent       : Gtk.Window.Gtk_Window;
      Network_Name : String;
      User_Name    : String := "";
      Force_Asking : Boolean := False) return String
   is
      Pwd : Password_Access := Password_List;
      function Full_Machine_Name return String;
      --  Return user@machine if user is set, or machine.

      -----------------------
      -- Full_Machine_Name --
      -----------------------

      function Full_Machine_Name return String is
      begin
         if Pwd.User_Name.all = "" then
            return Pwd.Machine.all;
         else
            return Pwd.User_Name.all & "@" & Pwd.Machine.all;
         end if;
      end Full_Machine_Name;

   begin
      while Pwd /= null loop
         exit when Pwd.Machine.all = Network_Name
           and then Pwd.User_Name.all = User_Name;
         Pwd := Pwd.Next;
      end loop;

      if Pwd = null then
         Pwd := new Password_Record'
           (Machine   => new String'(Network_Name),
            User_Name => new String'(User_Name),
            Password  => null,
            Next      => Password_List);
         Password_List := Pwd;
      end if;

      if Pwd.Password = null or else Force_Asking then
         Free (Pwd.Password);

         declare
            Str : constant String :=
                    Query_User
                      (Parent,
                       "Please enter " & Full_Machine_Name & "'s password:",
                       Password_Mode => True);
         begin
            if Str = "" then
               return Str;
            end if;

            Pwd.Password := new String'(Str);
         end;
      end if;

      return Pwd.Password.all;
   end Get_Password;

   --------------------
   -- Get_Passphrase --
   --------------------

   function Get_Passphrase
     (Parent       : Gtk.Window.Gtk_Window;
      Key_Id       : String;
      Force_Asking : Boolean := False) return String
   is
      Psp : Passphrase_Access := Passphrase_List;
   begin
      while Psp /= null loop
         exit when Psp.Key_Id.all = Key_Id;
         Psp := Psp.Next;
      end loop;

      if Psp = null then
         Psp := new Passphrase_Record'
           (Key_Id     => new String'(Key_Id),
            Passphrase => null,
            Next       => Passphrase_List);
         Passphrase_List := Psp;
      end if;

      if Psp.Passphrase = null or else Force_Asking then
         Free (Psp.Passphrase);

         declare
            Str : constant String :=
                    Query_User
                      (Parent,
                       "Please enter passphrase for key " & Key_Id & ":",
                       Password_Mode => True);
         begin
            if Str = "" then
               return "";
            end if;

            Psp.Passphrase := new String'(Str);
         end;
      end if;

      return Psp.Passphrase.all;
   end Get_Passphrase;

end Password_Manager;
