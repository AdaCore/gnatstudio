-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003-2004                    --
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

with Ada.Calendar; use Ada.Calendar;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.IO;      use GNAT.IO;
with String_Utils; use String_Utils;
with VFS;          use VFS;
with GNAT.Strings; use GNAT.Strings;
with GNAT.Bubble_Sort; use GNAT.Bubble_Sort;

package body Entities.Debug is

   Dump_Full_File_Names : constant Boolean := False;
   Show_Timestamps      : Boolean := True;

   use Entities_Tries;
   use Files_HTable;
   use LI_HTable;
   use Source_File_Arrays;
   use Entity_Reference_Arrays;
   use Entity_Information_Arrays;
   use Dependency_Arrays;

   type Source_File_Array is array (Natural range <>) of Source_File;

   function Get_Sorted_List_Of_Files
     (Files : access Files_HTable.HTable) return Source_File_Array;
   --  Sort the Files and return a sorted array

   procedure Dump (Sorted_Files : Source_File_Array);
   procedure Dump (LIs       : in out LI_HTable.HTable);
   procedure Dump
     (Entities : Entities_Tries.Trie_Tree; Full : Boolean; Name : String);
   procedure Dump (LI        : LI_File);
   procedure Dump (Timestamp : Ada.Calendar.Time);
   procedure Dump (Files     : Source_File_List; Name : String);
   procedure Dump (Files     : Dependency_List; Name : String);
   procedure Dump (E         : Entity_Information_List_Access);
   procedure Dump (Dep       : File_Dependency);
   procedure Dump
     (Entities  : Entity_Information_List;
      Full      : Boolean;
      Name      : String);
   procedure Dump (Locs : Entity_Reference_List; Name : String);
   procedure Dump (Kind : E_Kind);
   procedure Dump (Ref  : Entity_Reference);
   procedure Dump (File : Virtual_File);
   procedure Dump_Entities_From_Files (Files : Source_File_Array);
   --  Dump various parts of the system

   procedure Low_Level_Dump is new Entities_Tries.Dump (GNAT.IO.Put, Dump);
   pragma Unreferenced (Low_Level_Dump);

   Reference_Kind_To_Char : constant array (Reference_Kind) of Character :=
     (Reference                                => 'r',
      Instantiation_Reference                  => ' ',
      Modification                             => 'm',
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

   procedure Dump (File : Virtual_File) is
   begin
      if Dump_Full_File_Names then
         Put (Full_Name (File).all);
      else
         Put (Base_Name (File));
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Dep : File_Dependency) is
   begin
      Dump (Get_Filename (Dep.File));
      if Dep.Explicit then
         Put (":W");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Kind : E_Kind) is
   begin
      if Kind = Unresolved_Entity_Kind then
         Put ("(Unresolved)");
      else
         Put ('(' & Kind.Kind'Img & " type=" & Boolean'Image (Kind.Is_Type)
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
            Put ("   " & Name & "= ");
         end if;

         for E in Entity_Information_Arrays.First .. Last (Entities) loop
            Dump (Entities.Table (E), Full => Full, Name => "");

            if not Full then
               Put (' ');
            end if;
         end loop;

         if not Full and then Name /= "" then
            New_Line;
         end if;
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (E : Entity_Information_List_Access) is
   begin
      if E = null then
         Put ("<empty_list>");
      else
         Dump (E.all, Full => False, Name => "");
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Ref : Entity_Reference) is
   begin
      Dump (Ref.Location); Put (':' & Reference_Kind_To_Char (Ref.Kind));
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Locs : Entity_Reference_List; Name : String) is
   begin
      if Length (Locs) /= 0 then
         Put ("   " & Name & "= ");
         for L in Entity_Reference_Arrays.First .. Last (Locs) loop
            Dump (Locs.Table (L)); Put (' ');
         end loop;
         New_Line;
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Loc : File_Location) is
   begin
      if Loc = No_File_Location then
         Put ("<no_location>");
      else
         Dump (Get_Filename (Loc.File));
         Put (':' & Image (Loc.Line) & ':' & Image (Loc.Column));
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
         Put ("@<no_time>");
      elsif not Show_Timestamps then
         Put ("@<hidden time>");
      else
         Split (Timestamp, Year, Month, Day, Hour, Minutes, Seconds, Sub);
         Put ("@" & Image (Integer (Year)) & ':'
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
         Put ("   " & Name & "= ");
         for L in Dependency_Arrays.First .. Last (Files) loop
            Dump (Files.Table (L));
            Put (' ');
         end loop;
         New_Line;
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Files : Source_File_List; Name : String) is
   begin
      if Length (Files) /= 0 then
         Put ("   " & Name & "= ");
         for L in Source_File_Arrays.First .. Last (Files) loop
            Dump (Get_Filename (Files.Table (L)));
            Put (' ');
         end loop;
         New_Line;
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (LI : LI_File) is
   begin
      Dump (Get_LI_Filename (LI));
      Put (' ');
      Dump (LI.Timestamp);
      Put_Line (" ref_count=" & Image (LI.Ref_Count));

      Dump (LI.Files, "sources");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (LIs : in out LI_HTable.HTable) is
      Iter : LI_HTable.Iterator;
      LI   : LI_File;
   begin
      Put_Line ("====== LI files =====");
      Get_First (LIs, Iter);
      loop
         LI := Get_Element (Iter);
         exit when LI = null;
         Dump (LI);
         Get_Next (LIs, Iter);
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (File : Source_File) is
   begin
      Dump (Get_Filename (File));
      Put (' ');
      Dump (File.Timestamp);
      Put_Line (" ref_count=" & Image (File.Ref_Count)
                & " is_valid=" & Boolean'Image (File.Is_Valid)
                & " has_scope_tree=" & Boolean'Image (File.Scope /= null));

      if File.LI /= null then
         Put ("   li=");
         Dump (Get_LI_Filename (File.LI));
         New_Line;
      end if;

      Dump (File.Depends_On,  "depends_on");
      Dump (File.Depended_On, "depended_on");
      Dump (File.Entities, Full => False, Name => "entities");
      Dump (File.All_Entities, Full => False, Name => "all_entities");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Entity : Entity_Information; Full : Boolean; Name : String) is
   begin
      if Entity /= null then
         if not Full and then Name /= "" then
            Put ("   " & Name & "= ");
         end if;

         if Get_Name (Entity) = null then
            Put ("<no_name>:");
         elsif Is_Partial_Entity (Entity) then
            Put ("<unresolved yet>:");
         else
            Put (Get_Name (Entity).all & ':');
         end if;
         Dump (Entity.Declaration);

         if Full then
            Put_Line (" ref_count=" & Image (Entity.Ref_Count));
            if Entity.Kind /= Unresolved_Entity_Kind then
               Put ("   kind="); Dump (Entity.Kind); New_Line;
            end if;
            if Entity.End_Of_Scope.Location /= No_File_Location then
               Put ("   end_of_scope="); Dump (Entity.End_Of_Scope); New_Line;
            end if;
            Dump (Entity.Rename, False, "renames");
            Dump (Entity.Parent_Types,    False, "parents");
            Dump (Entity.Pointed_Type,    False, "points_to");
            Dump (Entity.Returned_Type,   False, "returns");
            Dump (Entity.Primitive_Op_Of, False, "primitive_of");
            Dump (Entity.Primitive_Subprograms, False, "primitives");
            Dump (Entity.Child_Types, False, "child_types");
            Dump (Entity.References, "references");
         elsif Name /= "" then
            New_Line;
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
      File  : Source_File;
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
            T : Source_File := Sorted (Op1);
         begin
            Sorted (Op1) := Sorted (Op2);
            Sorted (Op2) := T;
         end Xchg;

         function Lt (Op1, Op2 : Natural) return Boolean is
         begin
            return Get_Filename (Sorted (Op1)) < Get_Filename (Sorted (Op2));
         end Lt;

      begin
         Get_First (Files.all, Iter);
         for F in Sorted'Range loop
            Sorted (F) := Get_Element (Iter);
            Get_Next (Files.all, Iter);
         end loop;

         Sort (Sorted'Last, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);

         return Sorted;
      end;
   end Get_Sorted_List_Of_Files;

   ----------
   -- Dump --
   ----------

   procedure Dump (Sorted_Files : Source_File_Array) is
   begin
      Put_Line ("====== Source files =====");
      for F in Sorted_Files'Range loop
         Dump (Sorted_Files (F));
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Entities : Entities_Tries.Trie_Tree; Full : Boolean; Name : String)
   is
      Iter : Entities_Tries.Iterator := Start (Entities, "");
      E    : Entity_Information_List_Access;
      Is_Empty : constant Boolean := Get (Iter) = null;
   begin
      if Full then
         Put_Line ("====== Entities =====");
      elsif not Is_Empty then
         Put ("   " & Name & "= ");
      end if;

      loop
         E := Get (Iter);
         exit when E = null;
         Dump (E.all, Full => Full, Name => "");
         Next (Iter);
      end loop;
      Free (Iter);

      if not Full and then not Is_Empty then
         New_Line;
      end if;
   end Dump;

   ------------------------------
   -- Dump_Entities_From_Files --
   ------------------------------

   procedure Dump_Entities_From_Files (Files : Source_File_Array) is
   begin
      Put_Line ("====== Entities from files =====");
      for F in Files'Range loop
         Dump (Files (F).Entities, Full => True, Name => "");
      end loop;
   end Dump_Entities_From_Files;

   ----------
   -- Dump --
   ----------

   procedure Dump (Db : Entities_Database) is
   begin
      if Db /= null then
         declare
            Files : constant Source_File_Array := Get_Sorted_List_Of_Files
              (Db.Files'Unrestricted_Access);
         begin
            Dump (Db.LIs);
            Dump (Files);

            if Db.Entities = Entities_Tries.Empty_Trie_Tree then
               Dump_Entities_From_Files (Files);
            else
               Dump (Db.Entities, Full => True, Name => "");
            end if;

            --  Low_Level_Dump (Db.Entities);
         end;
      end if;
   end Dump;
end Entities.Debug;
