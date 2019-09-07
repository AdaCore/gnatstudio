------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;

package body GNATdoc.Utils is

   ------------
   -- Filter --
   ------------

   function Filter (S : String) return String is
      F : Natural := S'First;
      L : Natural := S'Last;
   begin
      while F <= S'Last and then S (F) = ASCII.LF loop
         F := F + 1;
      end loop;

      while L >= F and then S (L) = ASCII.LF loop
         L := L - 1;
      end loop;

      return S (F .. L);
   end Filter;

   --------------------
   -- Get_Short_Name --
   --------------------

   function Get_Short_Name (Ada_Expanded_Name : String) return String is
      Index : Integer := Ada_Expanded_Name'Last;
   begin
      while Index > Ada_Expanded_Name'First
        and then Ada_Expanded_Name (Index) /= '.'
      loop
         Index := Index - 1;
      end loop;

      if Ada_Expanded_Name'Length > 0
        and then Ada_Expanded_Name (Index) = '.'
      then
         return Ada_Expanded_Name (Index + 1 .. Ada_Expanded_Name'Last);
      else
         return Ada_Expanded_Name;
      end if;
   end Get_Short_Name;

   -----------
   -- Image --
   -----------

   function Image
     (Entity : Root_Entity'Class) return String
   is
      Decl : constant General_Entity_Declaration :=
               Get_Declaration (Entity);
   begin
      return Image (Decl.Loc);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Loc           : General_Location;
      With_Filename : Boolean := True) return String
   is
   begin
      if Loc = No_Location
        or else Loc.File = No_File
      then
         return "";
      end if;

      if not With_Filename then
         return
           To_String (Loc.Line) & ":" &
           To_String (Integer (Loc.Column));
      else
         declare
            F_Name : constant String := +Loc.File.Base_Name;
         begin
            if F_Name = "<case_insensitive_predefined>"
              or else F_Name = "<case_sensitive_predefined>"
            then
               return "standard";
            end if;

            return F_Name & ":" &
              To_String (Loc.Line) & ":" &
              To_String (Integer (Loc.Column));
         end;
      end if;
   end Image;

   ----------------------
   -- Is_Expanded_Name --
   ----------------------

   function Is_Expanded_Name (Name : String) return Boolean is
      Index : Integer := Name'Last;
   begin
      while Index > Name'First
        and then Name (Index) /= '.'
      loop
         Index := Index - 1;
      end loop;

      return Name'Length > 0
        and then Name (Index) = '.';
   end Is_Expanded_Name;

   -------------------------
   -- Is_GNAT_Binder_File --
   -------------------------

   function Is_GNAT_Binder_File
     (File : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      Name          : constant String := +File.Base_Name;
      Binder_Prefix : constant String := "b_";
   begin
      return Name'Length > Binder_Prefix'Length
        and then Name (Name'First .. Name'First + Binder_Prefix'Length - 1)
                   = Binder_Prefix;
   end Is_GNAT_Binder_File;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Kernel.Registry.Tree.Info (File).Unit_Part = Unit_Spec;
   end Is_Spec_File;

   --------
   -- No --
   --------

   function No (E : Root_Entity'Class) return Boolean is
   begin
      return E = No_Root_Entity;
   end No;

   function No (L : General_Location) return Boolean is
   begin
      return L = No_Location;
   end No;

   function No (Text : Unbounded_String) return Boolean is
   begin
      return Text = Null_Unbounded_String;
   end No;

   function No (Text : Unbounded_String_Vectors.Vector) return Boolean is
   begin
      return Text.Is_Empty;
   end No;

   -------------
   -- Present --
   -------------

   function Present (E : Root_Entity'Class) return Boolean is
   begin
      return E /= No_Root_Entity;
   end Present;

   function Present (L : General_Location) return Boolean is
   begin
      return L /= No_Location;
   end Present;

   function Present (Text : Unbounded_String) return Boolean is
   begin
      return Text /= Null_Unbounded_String;
   end Present;

   function Present (Text : Unbounded_String_Vectors.Vector) return Boolean is
   begin
      return not Text.Is_Empty;
   end Present;

   -----------------
   -- Spaces_Only --
   -----------------

   function Spaces_Only (Text : String) return Boolean is
   begin
      if Text'Length = 0 then
         return False;

      else
         for J in Text'Range loop
            if Text (J) /= ' ' then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Spaces_Only;

   -----------------
   -- Split_Lines --
   -----------------

   function Split_Lines
     (Text : String) return Unbounded_String_Vectors.Vector
   is
      use Ada.Characters.Latin_1;

      First   : Positive := Text'First;
      Current : Positive := Text'First;
      Result  : Unbounded_String_Vectors.Vector;

   begin
      while Current <= Text'Last loop
         if Text (Current) = CR or Text (Current) = LF then
            Result.Append (To_Unbounded_String (Text (First .. Current - 1)));

            --  CR & LF combination is handled as single line separator

            if Text (Current) = CR
              and then Current < Text'Last
              and then Text (Current + 1) = LF
            then
               Current := Current + 2;

            else
               Current := Current + 1;
            end if;

            First := Current;

         else
            Current := Current + 1;
         end if;
      end loop;

      if First /= Current then
         --  Append content of last non terminated line

         Result.Append (To_Unbounded_String (Text (First .. Text'Last)));
      end if;

      return Result;
   end Split_Lines;

   ---------------
   -- To_String --
   ---------------

   function To_String (N : Integer) return String is
      Str : constant String := Integer'Image (N);
   begin
      if Str (Str'First) = ' ' then
         return Str (Str'First + 1 .. Str'Last);
      else
         return Str;
      end if;
   end To_String;

   function To_String (Text : Unbounded_String_Vectors.Vector) return String is
      Aux : Unbounded_String;

   begin
      for S of Text loop
         Append (Aux, S);
         Append (Aux, Ada.Characters.Latin_1.LF);
      end loop;

      return To_String (Aux);
   end To_String;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Text : Unbounded_String_Vectors.Vector) return Unbounded_String is
      Aux : Unbounded_String;

   begin
      for S of Text loop
         Append (Aux, S);
         Append (Aux, Ada.Characters.Latin_1.LF);
      end loop;

      return Aux;
   end To_Unbounded_String;

end GNATdoc.Utils;
