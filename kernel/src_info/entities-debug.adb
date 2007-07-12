-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003-2007, AdaCore           --
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

with Ada.Calendar; use Ada.Calendar;
with GNAT.Calendar; use GNAT.Calendar;
with String_Utils; use String_Utils;
with VFS;          use VFS;
with GNAT.Strings; use GNAT.Strings;
with GNAT.Heap_Sort; use GNAT.Heap_Sort;

with Basic_Types; use Basic_Types;

package body Entities.Debug is

   Dump_Full_File_Names : constant Boolean := False;
   Show_Timestamps      : Boolean := True;

   use Entities_Hash;
   use Files_HTable;
   use LI_HTable;
   use Source_File_Arrays;
   use Entity_Reference_Arrays;
   use Entity_Information_Arrays;
   use Dependency_Arrays;
   use Instantiation_Arrays;

   type Source_File_Array is array (Natural range <>) of Source_File;

   function Get_Sorted_List_Of_Files
     (Files : access Files_HTable.HTable) return Source_File_Array;
   --  Sort the Files and return a sorted array

   procedure Dump
     (Sorted_Files  : Source_File_Array;
      Show_Entities : Boolean;
      Full          : Boolean);
   procedure Dump (LIs       : in out LI_HTable.HTable);
   procedure Dump
     (Entities : Entities_Hash.HTable;
      Full     : Boolean;
      Name     : String);
   procedure Dump (LI        : LI_File);
   procedure Dump (Timestamp : Ada.Calendar.Time);
   procedure Dump (Files : Source_File_List; Name : String);
   procedure Dump (Files     : Dependency_List; Name : String);
   procedure Dump (Dep       : File_Dependency);
   procedure Dump
     (Entities  : Entity_Information_List;
      Full      : Boolean;
      Name      : String);
   procedure Dump
     (Locs : Entity_Reference_List; Name : String; Full : Boolean);
   procedure Dump (Kind : E_Kind);
   procedure Dump (Ref  : E_Reference; Full : Boolean);
   procedure Dump (File : Virtual_File; Full : Boolean := False);
   procedure Dump_Entities_From_Files
     (Files         : Source_File_Array;
      Entities_Only : Boolean := False);
   procedure Dump (Instantiation : Entity_Instantiation);
   --  Dump various parts of the system

   function Image (Col : Visible_Column_Type) return String;
   --  Image of the column number

   Reference_Kind_To_Char : constant array (Reference_Kind) of Character :=
     (Reference                                => 'r',
      Instantiation_Reference                  => ' ',
      Modification                             => 'm',
      Declaration                              => 'D',
      Body_Entity                              => 'b',
      Completion_Of_Private_Or_Incomplete_Type => 'c',
      Type_Extension                           => 'x',
      Implicit                                 => 'i',
      Discriminant                             => 'd',
      Label                                    => 'l',
      Primitive_Operation                      => 'p',
      Overriding_Primitive_Operation           => 'p',
      With_Line                                => 'w',
      Subprogram_In_Parameter                  => '>',
      Subprogram_In_Out_Parameter              => '=',
      Subprogram_Out_Parameter                 => '<',
      Subprogram_Access_Parameter              => '^',
      Formal_Generic_Parameter                 => 'z',
      Parent_Package                           => 'k',
      End_Of_Spec                              => 'e',
      End_Of_Body                              => 't');

   -----------
   -- Image --
   -----------

   function Image (Col : Visible_Column_Type) return String is
      C : constant String := Visible_Column_Type'Image (Col);
   begin
      return C (C'First + 1 .. C'Last);
   end Image;

   ------------------------
   -- Set_Show_Timestamp --
   ------------------------

   procedure Set_Show_Timestamp (Show : Boolean := True) is
   begin
      Show_Timestamps := Show;
   end Set_Show_Timestamp;

   ----------
   -- Dump --
   ----------

   procedure Dump (Instantiation : Entity_Instantiation) is
      Ins : Entity_Instantiation := Instantiation;
   begin
      while Ins /= null loop
         Dump (Get_Entity (Ins), Full => False, Name => "");
         Output (" ");
         Ins := Generic_Parent (Ins);
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (File : Virtual_File; Full : Boolean := False) is
   begin
      if Dump_Full_File_Names or else Full then
         Output (Full_Name (File).all);
      else
         Output (Base_Name (File));
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Dep : File_Dependency) is
   begin
      Dump (Get_Filename (Dep.File));
      if Dep.Explicit then
         Output (":W");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Kind : E_Kind) is
   begin
      if Kind = Unresolved_Entity_Kind then
         Output ("(Unresolved)");
      else
         Output ('(' & Kind.Kind'Img & " type=" & Boolean'Image (Kind.Is_Type)
              & " generic=" & Boolean'Image (Kind.Is_Generic)
              & " abstract=" & Boolean'Image (Kind.Is_Abstract) & ')');
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Entities  : Entity_Information_List;
      Full      : Boolean;
      Name      : String) is
   begin
      if Length (Entities) /= 0 then
         if not Full and then Name /= "" then
            Output ("   " & Name & "= ");
         end if;

         for E in Entity_Information_Arrays.First .. Last (Entities) loop
            Dump (Entities.Table (E), Full => Full, Name => "");

            if not Full and then Entities.Table (E) /= null then
               Output (" ");
            end if;
         end loop;

         if not Full and then Name /= "" then
            Output_Line ("");
         end if;
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Ref : E_Reference; Full : Boolean) is
   begin
      Dump (Ref.Location); Output (':' & Reference_Kind_To_Char (Ref.Kind));
      if Full and then Ref.From_Instantiation_At /= null then
         Output ("[");
         Dump (Ref.From_Instantiation_At);
         Output ("]");
      end if;

      if Full and then Ref.Caller /= null then
         Output ("@"); Dump (Ref.Caller, Full => False, Name => "");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Locs : Entity_Reference_List; Name : String; Full : Boolean) is
   begin
      if Length (Locs) /= 0 then
         Output ("   " & Name & "= ");
         for L in Entity_Reference_Arrays.First .. Last (Locs) loop
            Dump (Locs.Table (L), Full); Output (" ");
         end loop;
         Output_Line ("");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Loc : File_Location) is
   begin
      if Loc = No_File_Location then
         Output ("<no_location>");
      else
         Dump (Get_Filename (Loc.File));
         Output (':' & Image (Loc.Line) & ':' & Image (Loc.Column));
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Timestamp : Ada.Calendar.Time) is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Hour    : Hour_Number;
      Minutes : Minute_Number;
      Seconds : Second_Number;
      Sub     : Second_Duration;
   begin
      if Timestamp = No_Time then
         Output ("@<no_time>");
      elsif not Show_Timestamps then
         Output ("@<hidden time>");
      else
         Split (Timestamp, Year, Month, Day, Hour, Minutes, Seconds, Sub);
         Output ("@" & Image (Integer (Year)) & ':'
              & Image (Integer (Month)) & ':'
              & Image (Integer (Day)) & '-'
              & Image (Integer (Hour)) & ':'
              & Image (Integer (Minutes)) & ':'
              & Image (Integer (Seconds)));
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Files : Dependency_List; Name : String) is
   begin
      if Length (Files) /= 0 then
         Output ("   " & Name & "= ");
         for L in Dependency_Arrays.First .. Last (Files) loop
            Dump (Files.Table (L));
            Output (" ");
         end loop;
         Output_Line ("");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Files : Source_File_List; Name : String) is
   begin
      if Length (Files) /= 0 then
         Output ("   " & Name & "= ");
         for L in Source_File_Arrays.First .. Last (Files) loop
            Dump (Get_Filename (Files.Table (L)));
            Output (" ");
         end loop;
         Output_Line ("");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (LI : LI_File) is
   begin
      Dump (Get_LI_Filename (LI));
      Output (" ");
      Dump (LI.Timestamp);
      Output_Line (" ref_count=" & Image (LI.Ref_Count));

      Dump (LI.Files, "sources");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (LIs : in out LI_HTable.HTable) is
      Iter : LI_HTable.Iterator;
      LI   : LI_File_Item;
   begin
      Output_Line ("====== LI files =====");
      Get_First (LIs, Iter);
      loop
         LI := Get_Element (Iter);
         exit when LI = null;
         Dump (LI.File);
         Get_Next (LIs, Iter);
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (File : Source_File; Show_Entities : Boolean; Full : Boolean) is
   begin
      Dump (Get_Filename (File), Full);
      Output (" ");
      Dump (File.Timestamp);
      Output_Line
        (" ref_count=" & Image (File.Ref_Count)
         & " has_scope_tree=" & Boolean'Image (File.Scope_Tree_Computed));

      if File.LI /= null then
         Output ("   li=");
         Dump (Get_LI_Filename (File.LI));
         Output_Line ("");
      end if;

      Dump (File.Depends_On,  "depends_on");
      Dump (File.Depended_On, "depended_on");

      if Show_Entities then
         Dump (File.Entities, Full => False, Name => "entities");
      end if;

      Dump (File.All_Entities, Full => False, Name => "all_entities");

      if Length (File.Instantiations) /= 0 then
         Output ("instantiations=");
         for J in Instantiation_Arrays.First .. Last (File.Instantiations) loop
            Output ("[");
            Dump (File.Instantiations.Table (J));
            Output ("] ");
         end loop;
         Output_Line ("");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Entity : Entity_Information; Full : Boolean; Name : String) is
   begin
      if Entity /= null then
         if not Full and then Name /= "" then
            Output ("   " & Name & "= ");
         end if;

         if Get_Name (Entity) = null then
            Output ("<no_name>:");
         else
            Output (Get_Name (Entity).all & ':');
         end if;
         Dump (Entity.Declaration);

         if Full then
            if not Entity.Is_Valid then
               Output (" is_valid=FALSE");
            end if;

            Output_Line (" ref_count=" & Image (Entity.Ref_Count));
            if Entity.Kind /= Unresolved_Entity_Kind then
               Output ("   kind="); Dump (Entity.Kind); Output_Line ("");
            end if;
            if Entity.End_Of_Scope.Location /= No_File_Location then
               Output ("   end_of_scope=");
               Dump (Entity.End_Of_Scope, Full => True);
               Output_Line ("");
            end if;

            Dump (Entity.Instantiation_Of, False, "instantiates");
            Dump (Entity.Caller_At_Declaration, False, "caller_at_decl");
            Dump (Entity.Rename, False, "renames");
            Dump (Entity.Parent_Types,    False, "parents");
            Dump (Entity.Pointed_Type,    False, "points_to");
            Dump (Entity.Returned_Type,   False, "returns");
            Dump (Entity.Primitive_Op_Of, False, "primitive_of");
            Dump (Entity.Primitive_Subprograms, False, "primitives");
            Dump (Entity.Child_Types, False, "child_types");
            Dump (Entity.Called_Entities, False, "calls");
            Dump (Entity.References, "references", Full => True);
         elsif Name /= "" then
            Output_Line ("");
         end if;
      end if;
   end Dump;

   ------------------------------
   -- Get_Sorted_List_Of_Files --
   ------------------------------

   function Get_Sorted_List_Of_Files
     (Files : access Files_HTable.HTable) return Source_File_Array
   is
      Iter  : Files_HTable.Iterator;
      Count : Natural := 0;
      File  : Source_File_Item;
   begin
      Get_First (Files.all, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;
         Count := Count + 1;
         Get_Next (Files.all, Iter);
      end loop;

      declare
         Sorted : Source_File_Array (1 .. Count);

         procedure Xchg (Op1, Op2 : Natural);
         function Lt (Op1, Op2 : Natural) return Boolean;

         procedure Xchg (Op1, Op2 : Natural) is
            T : constant Source_File := Sorted (Op1);
         begin
            Sorted (Op1) := Sorted (Op2);
            Sorted (Op2) := T;
         end Xchg;

         function Lt (Op1, Op2 : Natural) return Boolean is
            OpF1 : constant Virtual_File := Get_Filename (Sorted (Op1));
            OpF2 : constant Virtual_File := Get_Filename (Sorted (Op2));
         begin
            if OpF1 = VFS.No_File then
               return OpF2 /= VFS.No_File;
            elsif OpF2 = VFS.No_File then
               return False;
            elsif Dump_Full_File_Names then
               --  We want <=, but it is more efficient to compute it this way
               return OpF2 < OpF1;
            else
               return Base_Name (OpF1) <= Base_Name (OpF2);
            end if;
         end Lt;

      begin
         Get_First (Files.all, Iter);
         for F in Sorted'Range loop
            Sorted (F) := Get_Element (Iter).File;
            Get_Next (Files.all, Iter);
         end loop;

         Sort (Sorted'Last, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);

         return Sorted;
      end;
   end Get_Sorted_List_Of_Files;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Sorted_Files  : Source_File_Array;
      Show_Entities : Boolean;
      Full          : Boolean) is
   begin
      Output_Line ("====== Source files =====");
      for F in Sorted_Files'Range loop
         Dump (Sorted_Files (F), Show_Entities, Full);
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Entities : Entities_Hash.HTable;
      Full     : Boolean;
      Name     : String)
   is
      Iter : Entities_Hash.Iterator;
      Is_Empty : Boolean;
      Count : Natural := 0;
   begin
      Get_First (Entities, Iter);
      Is_Empty := Get_Element (Iter) = null;

      while Get_Element (Iter) /= null loop
         Count := Count + 1;
         Get_Next (Entities, Iter);
      end loop;

      if Full then
         Output_Line ("====== Entities =====");
      elsif not Is_Empty then
         Output ("   " & Name & "= ");
      end if;

      declare
         Sorted : array (1 .. Count) of Entity_Informations;
         procedure Xchg (Op1, Op2 : Natural);
         function Lt    (Op1, Op2 : Natural) return Boolean;

         procedure Xchg (Op1, Op2 : Natural) is
            T : constant Entity_Informations := Sorted (Op1);
         begin
            Sorted (Op1) := Sorted (Op2);
            Sorted (Op2) := T;
         end Xchg;

         function Lt (Op1, Op2 : Natural) return Boolean is
         begin
            if Sorted (Op1) = null then
               return True;
            elsif Sorted (Op2) = null then
               return False;
            else
               return Get_Name (Sorted (Op1)).Str.all <
                 Get_Name (Sorted (Op2)).Str.all;
            end if;
         end Lt;
      begin
         Get_First (Entities, Iter);
         Count := Sorted'First;
         while Get_Element (Iter) /= null loop
            Sorted (Count) := Get_Element (Iter);
            Count := Count + 1;
            Get_Next (Entities, Iter);
         end loop;

         Sort (Sorted'Last, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);
         for S in Sorted'Range loop
            Dump (Sorted (S).List.all, Full => Full, Name => "");
         end loop;
      end;

      if not Full and then not Is_Empty then
         Output_Line ("");
      end if;
   end Dump;

   ------------------------------
   -- Dump_Entities_From_Files --
   ------------------------------

   procedure Dump_Entities_From_Files
     (Files         : Source_File_Array;
      Entities_Only : Boolean := False) is
   begin
      if not Entities_Only then
         Output_Line ("====== Entities from files =====");
         for F in Files'Range loop
            Dump (Files (F).Entities, Full => True, Name => "");
         end loop;
      else
         for F in Files'Range loop
            Dump (Files (F).Entities, Full => False,
                  Name => Base_Name (Get_Filename (Files (F))));
         end loop;
      end if;
   end Dump_Entities_From_Files;

   ----------
   -- Dump --
   ----------

   procedure Dump (Db            : Entities_Database;
                   Full          : Boolean := False;
                   Entities_Only : Boolean := False) is
   begin
      if Db /= null then
         declare
            Files : constant Source_File_Array := Get_Sorted_List_Of_Files
              (Db.Files'Unrestricted_Access);
         begin
            if not Entities_Only then
               Dump (Db.LIs);
               Dump (Files, Show_Entities => True, Full => Full);
            end if;
            Dump_Entities_From_Files (Files, Entities_Only);
         end;
      end if;
   end Dump;

   ------------------------
   -- Set_Default_Output --
   ------------------------

   procedure Set_Default_Output is
   begin
      Output      := GNAT.IO.Put'Access;
      Output_Line := GNAT.IO.Put_Line'Access;
   end Set_Default_Output;

end Entities.Debug;
