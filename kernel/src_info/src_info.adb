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

with Ada.Exceptions;            use Ada.Exceptions;
with Src_Info.Prj_Utils;        use Src_Info.Prj_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Unchecked_Deallocation;
with Types;                     use Types;
with Snames;                    use Snames;
with Prj;                       use Prj;
with Prj.Util;                  use Prj.Util;
with Prj.Env;                   use Prj.Env;
with Namet;                     use Namet;
with Stringt;                   use Stringt;

--  The following dependencies should be removed when we move this to the GNAT
--  sources (or the GNAT tools)
with Traces;                    use Traces;
with String_Utils;              use String_Utils;

package body Src_Info is

   Me : Debug_Handle := Create ("Src_Info");

   type LI_Handler_List_Record;
   type LI_Handler_List is access LI_Handler_List_Record;
   type LI_Handler_List_Record is record
      Handler  : LI_Handler;
      Language : Name_Id;
      Next     : LI_Handler_List;
   end record;

   LI_Handlers : LI_Handler_List;
   --  Global variable to store all the registered handlers. This has to be a
   --  global variable, since we want to be able to eventually move this
   --  package to the GNAT sources (thus we can't associated this with the
   --  kernel).

   Base_Year         : constant := 1990;
   --  Year used as year 0 when computing timestamps. This avoids range
   --  checking issues when converting timestamps to Src_Info.Time_Stamp;

   Seconds_In_Minute : constant := 60;
   Seconds_In_Hour   : constant := 60 * Seconds_In_Minute;
   Seconds_In_Day    : constant := 24 * Seconds_In_Hour;
   Seconds_In_Month  : constant := 31 * Seconds_In_Day;
   Seconds_In_Year   : constant := 12 * Seconds_In_Month;

   procedure Free is new Unchecked_Deallocation (File_Info, File_Info_Ptr);
   procedure Free is new Unchecked_Deallocation
     (LI_File_Constrained, LI_File_Ptr);
   procedure Free is new
     Unchecked_Deallocation (LI_File_Node, LI_File_Node_Ptr);
   procedure Free is new
     Unchecked_Deallocation (E_Reference_Node, E_Reference_List);
   procedure Free is new
     Unchecked_Deallocation (E_Declaration_Info_Node, E_Declaration_Info_List);
   procedure Free is new
     Unchecked_Deallocation (File_Info_Ptr_Node, File_Info_Ptr_List);
   procedure Free is new
     Unchecked_Deallocation
       (Dependency_File_Info_Node, Dependency_File_Info_List);
   --  Memory deallocation routines.

   function Hash is new HTables.Hash (LI_File_HTable_Index);
   --  Hash function for strings.

   function Get_Separate_File_Info
     (LIF : LI_File_Ptr; Unit_Name : String_Access) return File_Info_Ptr;
   --  Return a pointer to the file info which Unit_Name matches the given
   --  Unit_Name. Return null if such unit could not be found.

   function Find_Source_File
     (Source_Info_List : LI_File_List; File : Internal_File)
      return Source_File;
   --  Create a new Source_File structure matching File.

   function Check_Language_Of
     (Project : Project_Id; Source_Filename : String; Language : Name_Id)
      return Boolean;
   --  Return True if Source_Filename belongs to Language, as defined in
   --  Project.
   --  ??? This should probably be moved to Prj.Nmsc.

   ----------------------------
   -- Get_Separate_File_Info --
   ----------------------------

   function Get_Separate_File_Info
     (LIF : LI_File_Ptr; Unit_Name : String_Access) return File_Info_Ptr
   is
      Current_Node : File_Info_Ptr_List := LIF.LI.Separate_Info;
   begin
      while Current_Node /= null loop
         if Current_Node.Value.Unit_Name.all = Unit_Name.all then
            return Current_Node.Value;
         end if;
      end loop;
      --  If we reach this point, this means that the File_Info was not found.
      return null;
   end Get_Separate_File_Info;

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Source_Info : LI_File_Ptr) return Boolean is
   begin
      return not Source_Info.LI.Parsed;
   end Is_Incomplete;

   -----------
   -- Reset --
   -----------

   procedure Reset (LIFL : in out LI_File_List) is
   begin
      Reset (LIFL.Table);
   end Reset;

   ------------
   -- Locate --
   ------------

   function Locate
     (List : LI_File_List;
      LI_Filename : String)
      return LI_File_Ptr is
   begin
      return Get (List.Table, LI_Filename);
   end Locate;

   ------------------------
   -- Locate_From_Source --
   ------------------------

   function Locate_From_Source
     (List            : LI_File_List;
      Source_Filename : String)
      return LI_File_Ptr
   is
      Short_Filename : constant String := Base_Name (Source_Filename);
      Current_LI     : LI_File_Ptr;
      Current_Sep    : File_Info_Ptr_List;
      Table : LI_File_HTable.HTable := List.Table;
      --  ??? Make a copy of the table since Get_First and Get_Next need
      --  ??? a Read/Writable HTable. This is temporary since we should stop
      --  ??? using Get_First/Next soon. See ??? comment below.
   begin
      --  ??? The best way of doing this is to convert the filename into the
      --  ??? Library Info filename, and then use the Htable to retrieve the
      --  ??? LI_File. This poses a few problems because the conversion is
      --  ??? language dependent. We might want to play with dispatching
      --  ??? using Language.* later on. For the moment, we do a brutal
      --  ??? search; that'll do for now, and it works fast enough on small
      --  ??? projects.
      Get_First (Table, Current_LI);
      LI_File_Loop :
      while Current_LI /= null loop
         --  See if the filename matches the spec filename
         if Current_LI.LI.Spec_Info /= null
           and then Current_LI.LI.Spec_Info.Source_Filename.all =
              Short_Filename
         then
            return Current_LI;
         end if;

         --  Check if the filename matches the body filename
         if Current_LI.LI.Body_Info /= null
           and then Current_LI.LI.Body_Info.Source_Filename.all =
              Short_Filename
         then
            return Current_LI;
         end if;

         --  Finally, check the filenames of the separates
         Current_Sep := Current_LI.LI.Separate_Info;
         Separate_Loop :
         while Current_Sep /= null loop
            if Current_Sep.Value.Source_Filename.all = Short_Filename then
               return Current_LI;
            end if;
            Current_Sep := Current_Sep.Next;
         end loop Separate_Loop;

         --  This LI_File does not match, try the next one in the table...
         Get_Next (Table, Current_LI);
      end loop LI_File_Loop;

      --  If we reach this point, then there is no matching LI_File
      return null;
   end Locate_From_Source;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Source_File) return Boolean is
   begin
      if Left.LI = Right.LI and then Left.Part = Right.Part then
         if Left.Unit_Name = null then
            return Right.Unit_Name = null;
         else
            return Right.Unit_Name /= null
              and then Left.Unit_Name.all = Right.Unit_Name.all;
         end if;
      else
         return False;
      end if;
   end "=";

   function "=" (Left, Right : File_Location) return Boolean is
   begin
      return Left.Line = Right.Line
        and then Left.Column = Right.Column
        and then Left.File = Right.File;
   end "=";

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E : LI_File_Node_Ptr; Next : LI_File_Node_Ptr) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : LI_File_Node_Ptr) return LI_File_Node_Ptr is
   begin
      return E.Next;
   end Next;

   ---------------------
   -- Get_LI_Filename --
   ---------------------

   function Get_LI_Filename (E : LI_File_Node_Ptr) return String_Access is
   begin
      return E.Value.LI.LI_Filename;
   end Get_LI_Filename;

   ----------
   -- Hash --
   ----------

   function Hash (F : String_Access) return LI_File_HTable_Index is
   begin
      return Hash (F.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ---------
   -- Add --
   ---------

   procedure Add
     (HT      : in out LI_File_HTable.HTable;
      LIFP    : LI_File_Ptr;
      Success : out Boolean)
   is
      Tmp : constant LI_File_Node_Ptr :=
        LI_File_HTable.Get (HT, LIFP.LI.LI_Filename);
   begin
      --  Make sure no LI_File with the same unit name already exists before
      --  inserting in the table.
      if Tmp /= null then
         Success := False;
         return;
      end if;
      LI_File_HTable.Set (HT, new LI_File_Node'(Value => LIFP, Next => null));
      Success := True;
   end Add;

   -----------
   -- Reset --
   -----------

   procedure Reset (HT : in out LI_File_HTable.HTable) is
      Current_Unit : LI_File_Node_Ptr;
      Next_Unit    : LI_File_Node_Ptr;
   begin
      --  Destroy all elements pointed by the hash-table...
      LI_File_HTable.Get_First (HT, Current_Unit);
      while Current_Unit /= null loop
         LI_File_HTable.Get_Next (HT, Next_Unit);
         Destroy (Current_Unit);
         Current_Unit := Next_Unit;
      end loop;
      --  And finally, reset the hash-table itself...
      LI_File_HTable.Reset (HT);
   end Reset;

   ---------
   -- Get --
   ---------

   function Get
     (HT : LI_File_HTable.HTable; LI_Filename : String) return LI_File_Ptr
   is
      Name : aliased String := Base_Name (LI_Filename);
      Node : constant LI_File_Node_Ptr :=
        LI_File_HTable.Get (HT, Name'Unchecked_Access);
   begin
      if Node = null then
         return No_LI_File;
      else
         return Node.Value;
      end if;
   end Get;

   ---------------
   -- Get_First --
   ---------------

   procedure Get_First
     (HT : in out LI_File_HTable.HTable; Result : out LI_File_Ptr)
   is
      Node : LI_File_Node_Ptr;
   begin
      LI_File_HTable.Get_First (HT, Node);
      if Node = null then
         Result := No_LI_File;
      else
         Result := Node.Value;
      end if;
   end Get_First;

   --------------
   -- Get_Next --
   --------------

   procedure Get_Next
     (HT : in out LI_File_HTable.HTable; Result : out LI_File_Ptr)
   is
      Node : LI_File_Node_Ptr;
   begin
      LI_File_HTable.Get_Next (HT, Node);
      if Node = null then
         Result := No_LI_File;
      else
         Result := Node.Value;
      end if;
   end Get_Next;

   ----------------------
   -- Is_File_Location --
   ----------------------

   function Is_File_Location (Location : in File_Location) return Boolean is
   begin
      return Location.File.LI /= null;
   end Is_File_Location;

   -------------------
   -- Get_File_Info --
   -------------------

   function Get_File_Info (SF : Source_File) return File_Info_Ptr is
   begin
      case SF.Part is
         when Unit_Spec =>
            return SF.LI.LI.Spec_Info;
         when Unit_Body =>
            return SF.LI.LI.Body_Info;
         when Unit_Separate =>
            return Get_Separate_File_Info (SF.LI, SF.Unit_Name);
      end case;
   end Get_File_Info;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (LIF : in out LI_File) is
   begin
      Free (LIF.LI_Filename);
      Destroy (LIF.Spec_Info);
      Destroy (LIF.Body_Info);
      Destroy (LIF.Separate_Info);
      if LIF.Parsed then
         Destroy (LIF.Dependencies_Info);
      end if;
   end Destroy;

   procedure Destroy (LIFP : in out LI_File_Ptr) is
   begin
      if LIFP /= null then
         Destroy (LIFP.LI);
         Free (LIFP);
      end if;
   end Destroy;

   procedure Destroy (SF : in out Source_File) is
   begin
      --  Do not deallocate SF.LI, we are just pointing to it, we did
      --  not allocate it for this object.
      Free (SF.Unit_Name);
   end Destroy;

   procedure Destroy (FL : in out File_Location) is
   begin
      Destroy (FL.File);
   end Destroy;

   procedure Destroy (ER : in out E_Reference) is
   begin
      Destroy (ER.Location);
   end Destroy;

   procedure Destroy (ERL : in out E_Reference_List)
   is
      Current_Node : E_Reference_List renames ERL;
      Next_Node    : E_Reference_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (ED : in out E_Declaration) is
   begin
      Free (ED.Name);
      Destroy (ED.Location);
      Destroy (ED.Parent_Location);
      Destroy (ED.End_Of_Scope);
   end Destroy;

   procedure Destroy (EDI : in out E_Declaration_Info) is
   begin
      Destroy (EDI.Declaration);
      Destroy (EDI.References);
   end Destroy;

   procedure Destroy (EDIL : in out E_Declaration_Info_List) is
      Current_Node : E_Declaration_Info_List renames EDIL;
      Next_Node    : E_Declaration_Info_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (FI : in out File_Info) is
   begin
      Free (FI.Unit_Name);
      Free (FI.Source_Filename);
      Free (FI.Original_Filename);
      Destroy (FI.Declarations);
   end Destroy;

   procedure Destroy (FIP : in out File_Info_Ptr) is
   begin
      if FIP /= null then
         Destroy (FIP.all);
         Free (FIP);
      end if;
   end Destroy;

   procedure Destroy (FIPL : in out File_Info_Ptr_List) is
      Current_Node : File_Info_Ptr_List renames FIPL;
      Next_Node    : File_Info_Ptr_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (DFI : in out Dependency_File_Info) is
   begin
      Destroy (DFI.File);
      Destroy (DFI.Declarations);
   end Destroy;

   procedure Destroy (DFIL : in out Dependency_File_Info_List)
   is
      Current_Node : Dependency_File_Info_List renames DFIL;
      Next_Node    : Dependency_File_Info_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (LIFNP : in out LI_File_Node_Ptr)
   is
      Current_Node : LI_File_Node_Ptr renames LIFNP;
      Next_Node    : LI_File_Node_Ptr;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   ----------
   -- Copy --
   ----------

   function Copy (SF : Source_File) return Source_File is
      Result : Source_File := SF;
   begin
      if SF.Unit_Name /= null then
         Result.Unit_Name := new String'(SF.Unit_Name.all);
      end if;
      return Result;
   end Copy;

   ---------------------------
   -- Get_Depends_From_Spec --
   ---------------------------

   function Get_Depends_From_Spec (Dep : Dependency_Info) return Boolean is
   begin
      return Dep.Depends_From_Spec;
   end Get_Depends_From_Spec;

   ---------------------------
   -- Get_Depends_From_Body --
   ---------------------------

   function Get_Depends_From_Body (Dep : Dependency_Info) return Boolean is
   begin
      return Dep.Depends_From_Body;
   end Get_Depends_From_Body;

   -------------------
   -- Get_Unit_Name --
   -------------------

   procedure Get_Unit_Name
     (Source                 : in out Source_File;
      Source_Info_List       : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String;
      Unit_Name              : out String_Access)
   is
      Source_Filename : constant String := Get_Source_Filename (Source);
   begin
      Create_Or_Complete_LI
        (Handler                =>
           Handler_From_Filename (Project, Source_Filename),
         File                   => Source.LI,
         Source_Filename        => Source_Filename,
         List                   => Source_Info_List,
         Project                => Project,
         Predefined_Source_Path => Predefined_Source_Path,
         Predefined_Object_Path => Predefined_Object_Path);
      Unit_Name := Get_File_Info (Source).Unit_Name;
   end Get_Unit_Name;

   ----------------------
   -- Make_Source_File --
   ----------------------

   function Make_Source_File
     (Source_Filename        : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String) return Internal_File
   is
      ALI : constant String := LI_Filename_From_Source
        (Handler                =>
           Handler_From_Filename (Project, Source_Filename),
         Source_Filename        => Source_Filename,
         Project                => Project,
         Predefined_Source_Path => Predefined_Source_Path);
   begin
      return (File_Name => new String' (Source_Filename),
              Unit_Name => null,
              LI_Name   => new String' (ALI));
   end Make_Source_File;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (File : in out Internal_File) is
   begin
      Free (File.File_Name);
      Free (File.Unit_Name);
      Free (File.LI_Name);
   end Destroy;

   ----------
   -- Copy --
   ----------

   function Copy (File : Internal_File) return Internal_File is
      Result : Internal_File;
   begin
      Result := (File_Name => new String' (File.File_Name.all),
                 Unit_Name => null,
                 LI_Name   => new String' (File.LI_Name.all));
      if File.Unit_Name /= null then
         Result.Unit_Name := new String' (File.Unit_Name.all);
      end if;
      return Result;
   end Copy;

   -------------------------
   -- Get_Source_Filename --
   -------------------------

   function Get_Source_Filename (File : Internal_File) return String is
   begin
      return File.File_Name.all;
   end Get_Source_Filename;

   ------------------------------
   -- Get_Full_Source_Filename --
   ------------------------------

   function Get_Full_Source_Filename
     (File                   : Internal_File;
      Source_Info_List       : Src_Info.LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String) return String
   is
      SF : Source_File := Find_Source_File (Source_Info_List, File);
      Ptr : File_Info_Ptr := Get_File_Info (SF);
      Dir : constant String := Get_Directory_Name
        (Ptr, Project, Predefined_Source_Path);
   begin
      return Dir & Ptr.Source_Filename.all;
   end Get_Full_Source_Filename;

   -------------------------
   -- Get_Source_Filename --
   -------------------------

   function Get_Source_Filename (File : Source_File) return String is
      FI : constant File_Info_Ptr := Get_File_Info (File);
   begin
      return FI.Source_Filename.all;
   end Get_Source_Filename;

   -------------------
   -- Get_Unit_Name --
   -------------------

   procedure Get_Unit_Name
     (File              : in out Internal_File;
      Source_Info_List  : in out Src_Info.LI_File_List;
      Project           : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String;
      Unit_Name         : out String_Access)
   is
      Source : Source_File;
   begin
      Source := Find_Source_File (Source_Info_List, File);
      pragma Assert (Source /= No_Source_File);

      Get_Unit_Name
        (Source                 => Source,
         Source_Info_List       => Source_Info_List,
         Project                => Project,
         Predefined_Source_Path => Predefined_Source_Path,
         Predefined_Object_Path => Predefined_Object_Path,
         Unit_Name              => Unit_Name);
   end Get_Unit_Name;

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (Source_Info_List : LI_File_List; File : Internal_File)
      return Source_File
   is
      Source : constant String := Get_Source_Filename (File);
      LI : LI_File_Ptr := Get (Source_Info_List.Table, File.LI_Name.all);
      Current_Node : File_Info_Ptr_List;
   begin
      --  We must have found an LI file. However, it might still be
      --  incomplete. But since we have the filename, it has to be associated
      --  with the LI file already.
      pragma Assert (LI /= No_LI_File);

      if LI.LI.Spec_Info /= null
        and then LI.LI.Spec_Info.Source_Filename.all = Source
      then
         return (LI        => LI,
                 Unit_Name => null,
                 Part      => Unit_Spec);

      elsif LI.LI.Body_Info /= null
        and then LI.LI.Body_Info.Source_Filename.all = Source
      then
         return (LI        => LI,
                 Unit_Name => null,
                 Part      => Unit_Body);

      else
         Current_Node := LI.LI.Separate_Info;
         while Current_Node /= null loop
            if Current_Node.Value.Source_Filename.all = Source then
               return (LI        => LI,
                       Unit_Name => Current_Node.Value.Unit_Name,
                       Part      => Unit_Separate);
            end if;
         end loop;
      end if;

      return No_Source_File;
   end Find_Source_File;

   -------------------
   -- Get_Unit_Part --
   -------------------

   function Get_Unit_Part
     (Source_Info_List : LI_File_List; File : String) return Unit_Part
   is
      LI : LI_File_Ptr := Locate_From_Source (Source_Info_List, File);
   begin
      if LI.LI.Spec_Info /= null
        and then LI.LI.Spec_Info.Source_Filename.all = File
      then
         return Unit_Spec;

      elsif LI.LI.Body_Info /= null
        and then LI.LI.Body_Info.Source_Filename.all = File
      then
         return Unit_Body;
      end if;

      return Unit_Separate;
   end Get_Unit_Part;

   ---------------------
   -- Get_LI_Filename --
   ---------------------

   function Get_LI_Filename (LI : LI_File_Ptr) return String is
   begin
      return LI.LI.LI_Filename.all;
   end Get_LI_Filename;

   ------------------------
   -- Get_Directory_Name --
   ------------------------

   function Get_Directory_Name
     (File                   : File_Info_Ptr;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String) return String
   is
      Ts : Timestamp := 0;
   begin
      --  If the timestamps mismatch, then we'll simply recompute the location
      --  of the file. Generally, it will be because the file has been edited
      --  since then (and it would be fine to use the same path), but it might
      --  also be because the project has changed and we are pointing to some
      --  other files.
      if File.Directory_Name /= null then
         Ts := To_Timestamp (File_Time_Stamp
            (File.Directory_Name.all & File.Source_Filename.all));

         if File.File_Timestamp /= Ts then
            Trace (Me, "Get_Directory_Name: timestamps mismatch for file "
                   & File.Source_Filename.all
                   & Ts'Img
                   & File.File_Timestamp'Img);
            Free (File.Directory_Name);
         end if;
      end if;

      if File.Directory_Name = null then
         Trace (Me, "Computing directory name for "
                & File.Source_Filename.all);
         File.Directory_Name := new String'
           (Dir_Name
            (Find_File (File.Source_Filename.all,
                        Ada_Include_Path (Project).all,
                        Predefined_Source_Path)));
      end if;

      --  Memorize the timestamp if necessary. This case is when we
      --  have created a dummy entry for the file, because another
      --  file depended on it. However, since we didn't actually parse
      --  its LI file, we don't have any timestamp
      --  information. Memorizing it here will allow the cache for the
      --  directory name to work properly.
      if File.File_Timestamp = 0 then
         if Ts = 0 then
            Ts := To_Timestamp (File_Time_Stamp
              (File.Directory_Name.all & File.Source_Filename.all));
         end if;

         File.File_Timestamp := Ts;
      end if;

      return File.Directory_Name.all;
   end Get_Directory_Name;

   -----------------------
   -- Check_Language_Of --
   -----------------------

   function Check_Language_Of
     (Project : Project_Id; Source_Filename : String; Language : Name_Id)
     return Boolean
   is
      function Check_Exception (List : Array_Element_Id) return Boolean;
      --  Return True if SourcE_Filename is part of the exceptions List.

      ---------------------
      -- Check_Exception --
      ---------------------

      function Check_Exception (List : Array_Element_Id) return Boolean is
         Elem : Array_Element_Id := List;
      begin
         while Elem /= No_Array_Element loop
            String_To_Name_Buffer (Array_Elements.Table (Elem).Value.Value);

            if Name_Buffer (1 .. Name_Len) = Source_Filename then
               return True;
            end if;

            Elem := Array_Elements.Table (Elem).Next;
         end loop;
         return False;
      end Check_Exception;

      Naming : Naming_Data := Projects.Table (Project).Naming;
      Spec, Bodies : Name_Id;
   begin
      Spec   := Value_Of (Language, Naming.Specification_Suffix);
      Bodies := Value_Of (Language, Naming.Implementation_Suffix);

      --  First, check the general extensions

      if Spec /= No_Name
        and then Suffix_Matches (Source_Filename, Get_Name_String (Spec))
      then
         return True;

      elsif Bodies /= No_Name
        and then Suffix_Matches (Source_Filename, Get_Name_String (Bodies))
      then
         return True;
      end if;

      --  otherwise, check the naming exceptions
      --  Special case for Ada
      if Language = Name_Ada then
         if Naming.Separate_Suffix /= No_Name
           and then Suffix_Matches (Source_Filename,
                                    Get_Name_String (Naming.Separate_Suffix))
         then
            return True;
         end if;

         return Check_Exception (Naming.Specifications)
           or else Check_Exception (Naming.Bodies);

      else
         return Check_Exception (Naming.Specification_Exceptions)
           or else Check_Exception (Naming.Implementation_Exceptions);
      end if;
   end Check_Language_Of;

   ---------------------------
   -- Handler_From_Filename --
   ---------------------------

   function Handler_From_Filename
     (Project : Project_Id; Source_Filename : String) return LI_Handler
   is
      Tmp : LI_Handler_List := LI_Handlers;
   begin
      while Tmp /= null loop
         if Check_Language_Of
           (Project, Base_Name (Source_Filename), Tmp.Language)
         then
            return Tmp.Handler;
         end if;
         Tmp := Tmp.Next;
      end loop;

      Trace (Me, "Unsupported language for " & Source_Filename);
      Raise_Exception (Unsupported_Language'Identity,
                       "Unsupported language for " & Source_Filename);
      return null;
   end Handler_From_Filename;

   -------------------------
   -- Register_LI_Handler --
   -------------------------

   procedure Register_LI_Handler (Handler : LI_Handler; Language : String) is
   begin
      Name_Len := Language'Length;
      Name_Buffer (1 .. Name_Len) := Language;
      LI_Handlers := new LI_Handler_List_Record'
        (Handler => Handler, Language => Name_Find, Next => LI_Handlers);
   end Register_LI_Handler;

   ------------------
   -- To_Timestamp --
   ------------------

   function To_Timestamp (Str : Types.Time_Stamp_Type) return Timestamp is
      Year, Month, Day, Hour, Minutes, Seconds : Nat;
   begin
      Split_Time_Stamp (Str, Year, Month, Day, Hour, Minutes, Seconds);

      --  Save some space on the year
      return Timestamp (Year - Base_Year) * Seconds_In_Year
        + Timestamp (Month) * Seconds_In_Month
        * Timestamp (Day) * Seconds_In_Day
        + Timestamp (Hour) * Seconds_In_Hour
        * Timestamp (Minutes) * Seconds_In_Minute
        * Timestamp (Seconds);
   end To_Timestamp;

   ------------------
   -- To_Timestamp --
   ------------------

   function To_Timestamp (Time : GNAT.OS_Lib.OS_Time) return Timestamp is
      Year    : Year_Type;
      Month   : Month_Type;
      Day     : Day_Type;
      Hour    : Hour_Type;
      Minutes : Minute_Type;
      Second  : Second_Type;

   begin
      GM_Split (Time, Year, Month, Day, Hour, Minutes, Second);
      return Timestamp (Year - Base_Year) * Seconds_In_Year
        + Timestamp (Month) * Seconds_In_Month
        * Timestamp (Day) * Seconds_In_Day
        + Timestamp (Hour) * Seconds_In_Hour
        * Timestamp (Minutes) * Seconds_In_Minute
        * Timestamp (Second);
   end To_Timestamp;

end Src_Info;
