-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Language;                use Language;
with Basic_Types;             use Basic_Types;
with GNAT.Regpat;             use GNAT.Regpat;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

package body Language_Handlers.GVD is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Info_Array, Language_Info_Access);

   function Get_Info_From_Name
     (Handler : access GVD_Language_Handler_Record'Class;
      Language_Name : String) return Natural;
   --  Return the index in Handler.Languages for the language Language_Name.
   --  0 is returned if the language wasn't found.
   --  Language_Name is assumed to be already lower-case.

   function To_Regexp (Extension : String) return String;
   --  Transform Extensions into a regexp, by applying the following
   --  transformations:
   --    - "." is transformed into "\."
   --    - a "$" is appended to S.

   ------------------------
   -- Get_Info_From_Name --
   ------------------------

   function Get_Info_From_Name
     (Handler : access GVD_Language_Handler_Record'Class;
      Language_Name : String) return Natural is
   begin
      if Handler.Languages /= null then
         for Index in Handler.Languages'Range loop
            if Handler.Languages (Index).Language_Name.all = Language_Name then
               return Index;
            end if;
         end loop;
      end if;

      return 0;
   end Get_Info_From_Name;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Handler : out GVD_Language_Handler) is
   begin
      Handler := new GVD_Language_Handler_Record;
   end Gtk_New;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler : access GVD_Language_Handler_Record;
      Source_Filename : String) return Language.Language_Access is
   begin
      if Handler.Languages /= null then
         for Index in Handler.Languages'Range loop
            if Handler.Languages (Index).Pattern /= null
              and then Match
              (Handler.Languages (Index).Pattern.all, Source_Filename)
            then
               return Handler.Languages (Index).Lang;
            end if;
         end loop;
      end if;

      return null;
   end Get_Language_From_File;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler : access GVD_Language_Handler_Record;
      Source_Filename : String) return String is
   begin
      if Handler.Languages /= null then
         for Index in Handler.Languages'Range loop
            if Handler.Languages (Index).Pattern /= null
              and then Match
              (Handler.Languages (Index).Pattern.all, Source_Filename)
            then
               return Handler.Languages (Index).Language_Name.all;
            end if;
         end loop;
      end if;

      return "";
   end Get_Language_From_File;

   ---------------------
   -- Known_Languages --
   ---------------------

   function Known_Languages
     (Handler : access GVD_Language_Handler_Record)
      return Argument_List is
   begin
      if Handler.Languages /= null then
         declare
            Result : Argument_List (Handler.Languages'Range);
         begin
            for Index in Handler.Languages'Range loop
               Result (Index) := new String'
                 (Handler.Languages (Index).Language_Name.all);
            end loop;
            return Result;
         end;
      else
         declare
            Result : Argument_List (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end Known_Languages;

   -----------------------
   -- Register_Language --
   -----------------------

   procedure Register_Language
     (Handler : access GVD_Language_Handler_Record;
      Name    : String;
      Lang    : Language.Language_Access)
   is
      N : constant String := To_Lower (Name);
      Tmp : Language_Info_Access;
      Index : Natural;
   begin
      if Handler.Languages /= null then
         Index := Get_Info_From_Name (Handler, N);
         if Index /= 0 then
            Handler.Languages (Index).Lang := Lang;
            return;
         end if;

         Tmp := new Language_Info_Array
           (Handler.Languages'First .. Handler.Languages'Last + 1);
         Tmp (Handler.Languages'First .. Handler.Languages'Last) :=
           Handler.Languages.all;
         Unchecked_Free (Handler.Languages);
         Handler.Languages := Tmp;
      else
         Handler.Languages := new Language_Info_Array (1 .. 1);
      end if;

      Handler.Languages (Handler.Languages'Last) :=
        (Language_Name => new String'(Name),
         Pattern       => null,
         Lang          => Lang);
   end Register_Language;

   ------------------------
   -- Add_File_Extension --
   ------------------------

   procedure Add_File_Extension
     (Handler       : access GVD_Language_Handler_Record;
      Language_Name : String;
      Pattern       : String)
   is
      N : constant String := To_Lower (Language_Name);
      Index : Natural;
      Tmp : Basic_Types.String_Access;
   begin
      Index := Get_Info_From_Name (Handler, N);
      if Index /= 0 then
         Tmp := Handler.Languages (Index).Pattern;

         if Tmp = null then
            Handler.Languages (Index).Pattern := new String'
              ('(' & Pattern & ')');
         else
            Handler.Languages (Index).Pattern :=
              new String'(Tmp.all & "|(" & Pattern & ')');
            Free (Tmp);
         end if;
      end if;
   end Add_File_Extension;

   ---------------
   -- To_Regexp --
   ---------------

   function To_Regexp (Extension : String) return String is
      Res : String (1 .. Extension'Length * 2 + 1);
      Ind : Positive := 1;
   begin
      for J in Extension'Range loop
         if Extension (J) /= '.' then
            Res (Ind) := Extension (J);
         else
            Res (Ind) := '\';
            Ind := Ind + 1;
            Res (Ind) := '.';
         end if;

         Ind := Ind + 1;
      end loop;

      Res (Ind) := '$';
      return Res (1 .. Ind);
   end To_Regexp;

   -------------------------
   -- Add_File_Extensions --
   -------------------------

   procedure Add_File_Extensions
     (Handler       : access GVD_Language_Handler_Record;
      Language_Name : String;
      Extensions    : String)
   is
      First, Ind : Natural;
   begin
      First := Extensions'First;

      loop
         Ind := Ada.Strings.Fixed.Index
           (Extensions (First .. Extensions'Last), ";");

         if Ind = 0 then
            Ind := Extensions'Last + 1;
         end if;

         Add_File_Extension
           (Handler, Language_Name,
            To_Regexp (Extensions (First .. Ind - 1)));

         exit when Ind > Extensions'Last;
         First := Ind + 1;
      end loop;
   end Add_File_Extensions;

   ---------------------------
   -- Reset_File_Extensions --
   ---------------------------

   procedure Reset_File_Extensions
     (Handler : access GVD_Language_Handler_Record) is
   begin
      if Handler.Languages /= null then
         for J in Handler.Languages'Range loop
            Free (Handler.Languages (J).Pattern);
         end loop;
      end if;
   end Reset_File_Extensions;

end Language_Handlers.GVD;
