with Ada.Calendar; use Ada.Calendar;
with GNAT.IO;      use GNAT.IO;
with String_Utils; use String_Utils;
with VFS;          use VFS;
with GNAT.Strings; use GNAT.Strings;

package body Entities.Debug is

   use Entities_Tries;
   use Files_HTable;
   use LI_HTable;
   use Source_File_Arrays;
   use File_Location_Arrays;
   use Entity_Information_Arrays;

   procedure Dump (LIs       : in out LI_HTable.HTable);
   procedure Dump (Files     : in out Files_HTable.HTable);
   procedure Dump
     (Entities : Entities_Tries.Trie_Tree; Full : Boolean; Name : String);
   procedure Dump (LI        : LI_File);
   procedure Dump (Timestamp : Ada.Calendar.Time);
   procedure Dump (Loc       : File_Location);
   procedure Dump (Files     : Source_File_List; Name : String);
   procedure Dump (E         : Entity_Information_List_Access);
   procedure Dump
     (Entities  : Entity_Information_List;
      Full      : Boolean;
      Name      : String);
   procedure Dump (Locs : File_Location_List; Name : String);
   procedure Dump (Kind : E_Kind);
   --  Dump various parts of the system

   procedure Low_Level_Dump is new Entities_Tries.Dump (GNAT.IO.Put, Dump);
   pragma Unreferenced (Low_Level_Dump);

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

   procedure Dump (Locs : File_Location_List; Name : String) is
   begin
      if Length (Locs) /= 0 then
         Put ("   " & Name & "= ");
         for L in File_Location_Arrays.First .. Last (Locs) loop
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
         Put (Full_Name (Get_Filename (Loc.File)).all & ':' & Image (Loc.Line)
              & ':' & Image (Loc.Column));
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Timestamp : Ada.Calendar.Time) is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
      Hour    : Integer;
      Minutes : Integer;
   begin
      if Timestamp = No_Time then
         Put ("@<no_time>");
      else
         Split (Timestamp, Year, Month, Day, Seconds);
         Hour    := Integer (Seconds / 3600.0);
         Minutes := Integer ((Seconds - Day_Duration (Hour) * 3600.0) / 60.0);
         Seconds := Seconds - Day_Duration (Hour) * 3600.0
         - Day_Duration (Minutes) * 60.0;

         Put ("@" & Image (Integer (Year)) & ':'
              & Image (Integer (Month)) & ':'
              & Image (Integer (Day)) & '-'
              & Image (Hour) & ':'
              & Image (Minutes) & ':'
              & Image (Integer (Seconds)));
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
            Put (Full_Name (Get_Filename (Files.Table (L))).all & ' ');
         end loop;
         New_Line;
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (LI : LI_File) is
   begin
      Put (Full_Name (Get_LI_Filename (LI)).all & ' ');
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
      Put (Full_Name (Get_Filename (File)).all & ' ');
      Dump (File.Timestamp);
      Put_Line (" ref_count=" & Image (File.Ref_Count)
                & " is_valid=" & Boolean'Image (File.Is_Valid)
                & " has_scope_tree=" & Boolean'Image (File.Scope /= null));

      if File.LI /= null then
         Put_Line ("   li=" & Full_Name (Get_LI_Filename (File.LI)).all);
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
         else
            Put (Get_Name (Entity).all & ':');
         end if;
         Dump (Entity.Declaration);

         if Full then
            Put_Line (" ref_count=" & Image (Entity.Ref_Count));
            if Entity.Kind /= Unresolved_Entity_Kind then
               Put ("   kind="); Dump (Entity.Kind); New_Line;
            end if;
            if Entity.End_Of_Scope /= No_File_Location then
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

   ----------
   -- Dump --
   ----------

   procedure Dump (Files : in out Files_HTable.HTable) is
      Iter : Files_HTable.Iterator;
      File : Source_File;
   begin
      Put_Line ("====== Source files =====");
      Get_First (Files, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;
         Dump (File);
         Get_Next (Files, Iter);
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

   ----------
   -- Dump --
   ----------

   procedure Dump (Db : Entities_Database) is
   begin
      if Db /= null then
         Dump (Db.LIs);
         Dump (Db.Files);
         Dump (Db.Entities, Full => True, Name => "");

         --  Low_Level_Dump (Db.Entities);
      end if;
   end Dump;
end Entities.Debug;
