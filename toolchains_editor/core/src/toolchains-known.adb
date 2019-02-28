------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Characters.Handling;               use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;
with Ada.Unchecked_Deallocation;

with XML_Utils;   use XML_Utils;
with XML_Parsers; use XML_Parsers;

package body Toolchains.Known is

   package Name_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=");
   package Toolchain_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Name_Map.Map,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=",
      "="             => Name_Map."=");

   Default_Naming : Name_Map.Map;
   The_Map        : Toolchain_Map.Map;

   ------------------------
   -- Read_From_XML_File --
   ------------------------

   procedure Read_From_XML_File (File : GNATCOLL.VFS.Virtual_File) is
      Root : Node_Ptr;
      Node : Node_Ptr;
      Buff : String_Access;

   begin
      XML_Parsers.Parse (File, Root, Buff);

      if Buff /= null then
         declare
            Cnt : constant String := Buff.all;
         begin
            Free (Buff);
            raise Invalid_File with Cnt;
         end;
      end if;

      Node := Root.Child;
      while Node /= null loop
         Read_From_XML (Node);
         Node := Node.Next;
      end loop;

      Free (Buff);
   end Read_From_XML_File;

   -------------------
   -- Read_From_XML --
   -------------------

   procedure Read_From_XML (Node : XML_Utils.Node_Ptr) is
      Child : Node_Ptr;
   begin
      if Node.Tag.all = "toolchain_default" then
         Child := Node.Child;
         while Child /= null loop
            if Child.Tag.all = "compiler" then
               Default_Naming.Include
                 ("compiler_" & Get_Attribute (Child, "lang"),
                  Child.Value.all);
            else
               Default_Naming.Include (Child.Tag.all, Child.Value.all);
            end if;

            Child := Child.Next;
         end loop;

      elsif Node.Tag.all = "toolchain" then
         declare
            Name : constant String := Get_Attribute (Node, "name");
            Map  : Name_Map.Map := Name_Map.Empty_Map;
         begin
            Child := Node.Child;
            while Child /= null loop
               if Child.Tag.all = "compiler" then
                  Map.Insert
                    ("compiler_" & Get_Attribute (Child, "lang"),
                     Child.Value.all);
               else
                  Map.Insert (Child.Tag.all, Child.Value.all);
               end if;

               Child := Child.Next;
            end loop;

            The_Map.Insert (Name, Map);
         end;
      end if;
   end Read_From_XML;

   -----------------------------
   -- Is_Known_Toolchain_Name --
   -----------------------------

   function Is_Known_Toolchain_Name (Name    : String) return Boolean is
   begin
      return The_Map.Contains (Name);
   end Is_Known_Toolchain_Name;

   -------------------------------
   -- Get_Known_Toolchain_Names --
   -------------------------------

   function Get_Known_Toolchain_Names return String_List_Access is
      Ret  : constant String_List_Access :=
               new String_List (1 .. Natural (The_Map.Length));
      Iter : Toolchain_Map.Cursor := The_Map.First;
      Idx  : Natural := 0;

   begin
      while Toolchain_Map.Has_Element (Iter) loop
         declare
            Tc   : String renames Toolchain_Map.Key (Iter);
            Done : Boolean := False;
         begin
            --  alphabetically ordered list
            for J in 1 .. Idx loop
               if Ret (J).all > Tc then
                  Ret (1 .. Idx + 1) :=
                    Ret (1 .. J - 1) & new String'(Tc) & Ret (J .. Idx);
                  Idx  := Idx + 1;
                  Done := True;

                  exit;
               end if;
            end loop;

            if not Done then
               Ret (Idx + 1) := new String'(Tc);
               Idx := Idx + 1;
            end if;
         end;

         Toolchain_Map.Next (Iter);
      end loop;

      return Ret;
   end Get_Known_Toolchain_Names;

   --------------------------
   -- Has_Naming_Exception --
   --------------------------

   function Has_Naming_Exception (Name : String) return Boolean is
   begin
      if not The_Map.Contains (Name) then
         return False;
      end if;

      return not The_Map.Element (Name).Is_Empty;
   end Has_Naming_Exception;

   ------------------
   -- Tool_Command --
   ------------------

   function Tool_Command (Tc : String; Name : Valid_Tools) return String is
      Tool_Name : constant String := To_Lower (Name'Img);
      Names     : Name_Map.Map;
   begin
      if not The_Map.Contains (Tc) then
         return "";
      end if;

      Names := The_Map.Element (Tc);

      if Names.Contains (Tool_Name) then
         return Names.Element (Tool_Name);
      else
         return Tc & "-" & Default_Naming.Element (Tool_Name);
      end if;
   end Tool_Command;

   -------------------------
   -- Is_Compiler_Defined --
   -------------------------

   function Is_Compiler_Defined (Tc : String; Lang : String) return Boolean is
      Lang_Name : constant String := "compiler_" & To_Lower (Lang);
      Names     : Name_Map.Map;
   begin
      if not The_Map.Contains (Tc) then
         return False;
      end if;

      Names := The_Map.Element (Tc);

      if Names.Contains (Lang_Name) then
         return Names.Element (Lang_Name) /= "";
      else
         return Default_Naming.Contains (Lang_Name);
      end if;
   end Is_Compiler_Defined;

   ----------------------
   -- Compiler_Command --
   ----------------------

   function Compiler_Command (Tc : String; Lang : String) return String is
      Lang_Name : constant String := "compiler_" & To_Lower (Lang);
      Names     : Name_Map.Map;
   begin
      if not The_Map.Contains (Tc) then
         return "";
      end if;

      Names := The_Map.Element (Tc);

      if Names.Contains (Lang_Name) then
         return Names.Element (Lang_Name);
      elsif Default_Naming.Contains (Lang_Name) then
         return Tc & "-" & Default_Naming.Element (Lang_Name);
      else
         return "";
      end if;
   end Compiler_Command;

   -----------
   -- Langs --
   -----------

   function Langs (Tc : String) return String_List_Access is
      Names       : Name_Map.Map;
      Iter        : Name_Map.Cursor;
      N_Languages : Natural := 0;
      Ret         : String_List_Access := new String_List (1 .. 10);

      procedure Simple_Free is new Ada.Unchecked_Deallocation
        (String_List, String_List_Access);

      procedure Append_Lang (Lang : String);

      -----------------
      -- Append_Lang --
      -----------------

      procedure Append_Lang (Lang : String) is
      begin
         N_Languages := N_Languages + 1;

         if N_Languages > Ret'Last then
            declare
               Tmp : constant String_List_Access :=
                       new String_List (1 .. Ret'Length * 2);
            begin
               Tmp (1 .. Ret'Length) := Ret.all;
               Simple_Free (Ret);
               Ret := Tmp;
            end;
         end if;

         Ret (N_Languages) := new String'(Lang);
      end Append_Lang;

   begin
      if not The_Map.Contains (Tc) then
         return null;
      end if;

      Names := Default_Naming;
      Iter := Names.First;

      while Name_Map.Has_Element (Iter) loop
         declare
            Key : constant String := Name_Map.Key (Iter);
         begin
            if Key'Length > 9
              and then Key (Key'First .. Key'First + 8) = "compiler_"
            then
               Append_Lang (Key (Key'First + 9 .. Key'Last));
            end if;
         end;

         Name_Map.Next (Iter);
      end loop;

      Names := The_Map.Element (Tc);
      Iter := Names.First;

      while Name_Map.Has_Element (Iter) loop
         declare
            Key : constant String := Name_Map.Key (Iter);
            Val : constant String := Name_Map.Element (Iter);
         begin
            if Key'Length > 9
              and then Key (Key'First .. Key'First + 8) = "compiler_"
            then
               declare
                  Lang : constant String := Key (Key'First + 9 .. Key'Last);
                  Found : Boolean;
               begin
                  if Val = "" then
                     --  Need to remove Lang for the return values
                     for J in 1 .. N_Languages loop
                        if Ret (J).all = Lang then
                           Free (Ret (J));
                           Ret (1 .. N_Languages - 1) :=
                             Ret (1 .. J - 1) & Ret (J + 1 .. N_Languages);
                           N_Languages := N_Languages - 1;

                           exit;
                        end if;
                     end loop;

                  else
                     --  If compiler is not defined in the base compilers list
                     --  we need to add it to the list
                     Found := False;

                     for J in 1 .. N_Languages loop
                        if Ret (J).all = Lang then
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Append_Lang (Lang);
                     end if;
                  end if;
               end;
            end if;
         end;

         Name_Map.Next (Iter);
      end loop;

      declare
         Tmp : constant String_List_Access :=
                 new String_List'(Ret (1 .. N_Languages));
      begin
         Simple_Free (Ret);

         return Tmp;
      end;
   end Langs;

end Toolchains.Known;
