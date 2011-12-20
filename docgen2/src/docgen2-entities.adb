------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNAT.HTable;
with GNAT.Strings;                use GNAT.Strings;

with Basic_Types;
with Entities.Queries;            use Entities.Queries;
with Find_Utils;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with String_Utils;                use String_Utils;

package body Docgen2.Entities is

   procedure Xref_Free (Xref : in out Cross_Ref);
   --  Free memory used by Xref

   procedure EInfo_List_Free
     (EInfo : in out Entity_Info);
   procedure EInfo_Free
     (Loc   : File_Location;
      EInfo : in out Entity_Info);
   --  Free memory used by Entity Info

   procedure Ensure_Loc_Index
     (File_Buffer : GNAT.Strings.String_Access;
      Loc         : in out Source_Location);
   --  Ensures that loc.Index is properly set.

   ----------
   -- Free --
   ----------

   procedure Xref_Free (Xref : in out Cross_Ref) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Cross_Ref_Record, Cross_Ref);
   begin
      if Xref /= null then
         Free (Xref.Name);
         Internal (Xref);
      end if;
   end Xref_Free;

   ----------------
   -- EInfo_Free --
   ----------------

   procedure EInfo_Free
     (Loc   : File_Location;
      EInfo : in out Entity_Info)
   is
      pragma Unreferenced (Loc);
   begin
      EInfo_List_Free (EInfo);
   end EInfo_Free;

   ---------------------
   -- EInfo_List_Free --
   ---------------------

   procedure EInfo_List_Free
     (EInfo : in out Entity_Info)
   is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Entity_Info_Record, Entity_Info);
   begin
      if EInfo = null then
         return;
      end if;

      Free (EInfo.Printout);
      EInfo.Generic_Params.Clear;
      EInfo.Children.Clear;
      EInfo.References.Clear;
      EInfo.Calls.Clear;
      EInfo.Called.Clear;

      case EInfo.Category is
         when Cat_Class =>
            EInfo.Parents.Clear;
            EInfo.Class_Children.Clear;
            EInfo.Primitive_Ops.Clear;

         when others =>
            null;
      end case;

      Internal (EInfo);
   end EInfo_List_Free;

   ----------------------
   -- Ensure_Loc_Index --
   ----------------------

   procedure Ensure_Loc_Index
     (File_Buffer : GNAT.Strings.String_Access;
      Loc         : in out Source_Location)
   is
      Col, Line : Natural;
   begin
      if File_Buffer = null then
         return;
      end if;

      if Loc.Index >= File_Buffer'First then
         --  Nothing to do: location index is initialized
         return;
      end if;

      --  Case where index is not initialized in the construct
      --  This happens with the C++ parser, for example.
      if Loc.Column /= 0 then
         Col := 1;
         Line := 1;

         for J in File_Buffer'Range loop

            if File_Buffer (J) = ASCII.LF then
               Line := Line + 1;
               Col := 0;
            elsif File_Buffer (J) /= ASCII.CR then
               --  ??? what about utf-8 characters ?
               Col := Col + 1;
            end if;

            if Line = Loc.Line and then Col = Loc.Column then
               Loc.Index := J;
               exit;
            end if;

         end loop;
      end if;
   end Ensure_Loc_Index;

   -----------
   -- Image --
   -----------

   function Image (Cat : Entity_Info_Category) return String is
   begin
      case Cat is
         when Cat_File =>
            return "file";
         when Cat_Package =>
            return "package";
         when Cat_Class =>
            return "class";
         when Cat_Task =>
            return "task";
         when Cat_Protected =>
            return "protected";
         when Cat_Type =>
            return "type";
         when Cat_Variable =>
            return "constant or variable";
         when Cat_Parameter =>
            return "parameter";
         when Cat_Subprogram =>
            return "subprogram";
         when Cat_Entry =>
            return "entry";
         when Cat_Unknown =>
            return "";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Cat : Language_Category) return String is
   begin
      case Cat is
         when Cat_Function =>
            return "function";
         when Cat_Procedure =>
            return "procedure";
         when others =>
            return Category_Name (Cat);
      end case;
   end Image;

   -----------------
   -- To_Category --
   -----------------

   function To_Category
     (Category : Language_Category) return Entity_Info_Category is
   begin
      case Category is
         when Cat_Package =>
            return Cat_Package;
         when Cat_Class =>
            return Cat_Class;
         when Cat_Task =>
            return Cat_Task;
         when Cat_Protected =>
            return Cat_Protected;
         when Cat_Entry =>
            return Cat_Entry;
         when Cat_Structure | Cat_Union | Cat_Type | Cat_Subtype =>
            return Cat_Type;
         when Cat_Variable =>
            return Cat_Variable;
         when Cat_Parameter =>
            return Cat_Parameter;
         when Cat_Procedure | Cat_Function | Cat_Method =>
            return Cat_Subprogram;
         when others =>
            return Cat_Unknown;
      end case;
   end To_Category;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Cross_Ref_List.Vector) is
   begin
      for J in List.First_Index .. List.Last_Index loop
         List.Update_Element (J, Xref_Free'Access);
      end loop;

      List.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Entity_Info_List.Vector) is
   begin
      for J in List.First_Index .. List.Last_Index loop
         List.Update_Element (J, EInfo_List_Free'Access);
      end loop;

      List.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Entity_Info_Map.Map) is
      Cursor : Entity_Info_Map.Cursor := List.First;
   begin
      while Entity_Info_Map.Has_Element (Cursor) loop
         List.Update_Element (Cursor, EInfo_Free'Access);
         Entity_Info_Map.Next (Cursor);
      end loop;

      List.Clear;
   end Free;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Cross_Ref) return Boolean is
   begin
      return To_Lower (Left.Name.all) < To_Lower (Right.Name.all);
   end Less_Than;

   -------------------------
   -- Less_Than_Full_Name --
   -------------------------

   function Less_Than_Full_Name (Left, Right : Entity_Info) return Boolean is
   begin
      return To_Lower (Get (Left.Name).all) < To_Lower (Get (Right.Name).all);
   end Less_Than_Full_Name;

   --------------------------
   -- Less_Than_Short_Name --
   --------------------------

   function Less_Than_Short_Name (Left, Right : Entity_Info) return Boolean is
   begin
      return To_Lower (Get (Left.Short_Name).all) <
        To_Lower (Get (Right.Short_Name).all);
   end Less_Than_Short_Name;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Virtual_File) return Boolean is
   begin
      return To_Lower (+Base_Name (Left)) < To_Lower (+Base_Name (Right));
   end Less_Than;

   ----------
   -- Hash --
   ----------

   function Hash (Key : File_Location) return Ada.Containers.Hash_Type is
      type Internal_Hash_Type is range 0 .. 2 ** 31 - 1;
      function Internal is new GNAT.HTable.Hash
        (Header_Num => Internal_Hash_Type);
   begin
      return Ada.Containers.Hash_Type
        (Internal
           (+GNATCOLL.VFS.Full_Name (Get_Filename (Key.File)) &
            Natural'Image (Key.Line) &
            Basic_Types.Visible_Column_Type'Image (Key.Column)));
   end Hash;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Left, Right : File_Location)
                             return Boolean is
      use Basic_Types;
   begin
      return Left.File = Right.File
        and then Left.Line = Right.Line
        and then Left.Column = Right.Column;
   end Equivalent_Keys;

   ------------------
   -- Set_Printout --
   ------------------

   procedure Set_Printout
     (Construct   : Simple_Construct_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info)
   is
      Sloc_Start, Sloc_End : Source_Location;
   begin
      Sloc_Start := Construct.Sloc_Start;
      Sloc_End   := Construct.Sloc_End;

      Ensure_Loc_Index (File_Buffer, Sloc_Start);
      Ensure_Loc_Index (File_Buffer, Sloc_End);

      E_Info.Printout_Loc := Construct.Sloc_Start;
      E_Info.Printout := new String'
        (File_Buffer
           (Sloc_Start.Index .. Sloc_End.Index));
   end Set_Printout;

   ----------------------
   -- Set_Pkg_Printout --
   ----------------------

   procedure Set_Pkg_Printout
     (Construct   : Simple_Construct_Information;
      Entity      : Entity_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info)
   is
      Start_Index : Natural;
      End_Index   : Natural;
      Pkg_Found   : Boolean;

      function Is_Token
        (Token : String; Start_Index : Natural) return Boolean;
      --  Test if Token is found at index Start_Index

      --------------
      -- Is_Token --
      --------------

      function Is_Token
        (Token : String; Start_Index : Natural) return Boolean is
      begin
         return Start_Index >= File_Buffer'First
           and then Start_Index + Token'Length <= File_Buffer'Last
           and then Is_Blank (File_Buffer (Start_Index + Token'Length))
           and then (Start_Index = File_Buffer'First
                     or else Is_Blank (File_Buffer (Start_Index - 1)))
           and then Equal
             (File_Buffer (Start_Index .. Start_Index + Token'Length - 1),
              Token,
              Case_Sensitive => False);
      end Is_Token;

   begin
      --  We assume Index is initialized, as the Ada parser does so.
      Start_Index := Construct.Sloc_Start.Index;
      End_Index := Construct.Sloc_Start.Index - 1;

      --  If we have an instantiation or a renaming, then output the full
      --  printout
      if Is_Instantiation_Of (Entity) /= null
        or else Renaming_Of (Entity) /= null
      then
         End_Index := Construct.Sloc_End.Index;

      else
         --  We will stop after 'package XXX is'
         Pkg_Found := False;

         for J in Construct.Sloc_Start.Index .. Construct.Sloc_End.Index loop
            if not Pkg_Found and then Is_Token ("package", J) then
               Pkg_Found := True;
            end if;

            if Pkg_Found then
               --  After package keywork, expect a ' is '
               --  or a '; '
               if File_Buffer (J) = ';' then
                  End_Index := J;
                  exit;

               elsif Is_Token ("is", J) then
                  End_Index := J + 1;
                  exit;

               end if;
            end if;
         end loop;
      end if;

      E_Info.Printout := new String'(File_Buffer (Start_Index .. End_Index));

      if Start_Index /= Construct.Sloc_Start.Index then
         declare
            Line    : Natural := 1;
            Col     : Basic_Types.Character_Offset_Type := 1;
            V_Col   : Basic_Types.Visible_Column_Type := 1;
            L_Start : Natural := 1;
         begin
            Find_Utils.To_Line_Column
              (Buffer         => File_Buffer.all,
               Pos            => Start_Index,
               Line           => Line,
               Column         => Col,
               Visible_Column => V_Col,
               Line_Start     => L_Start);
            E_Info.Printout_Loc :=
              (Line   => Line,
               Column => Natural (Col),
               Index  => Start_Index);
         end;
      else
         E_Info.Printout_Loc := Construct.Sloc_Start;
      end if;
   end Set_Pkg_Printout;

end Docgen2.Entities;
