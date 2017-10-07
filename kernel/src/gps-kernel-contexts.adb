------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with System.Address_To_Access_Conversions;

with GNATCOLL.Projects;    use GNATCOLL.Projects;
with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;
with GNATCOLL.VFS;         use GNATCOLL.VFS;

with Basic_Types;          use Basic_Types;
with GPS.Kernel.Project;   use GPS.Kernel.Project;

with Projects;             use Projects;
with Xref;                 use Xref;

package body GPS.Kernel.Contexts is

   type Filter_File is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_File; Context : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Directory is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Directory; Context : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Entity is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Entity; Context : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Project_Only is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Project_Only; Context : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Editable_Project is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Editable_Project; Context : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Project_File is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_Project_File; Context : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_In_Project is new GPS.Kernel.Action_Filter_Record
   with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_In_Project;
      Context : Selection_Context) return Boolean;
   --  True if the current file belongs to an opened project

   function Has_Directory_Information (File : Virtual_File) return Boolean;
   --  Returns true if file has directory information

   function Has_File_Information (File : Virtual_File) return Boolean;
   --  Idem for file information

   procedure Fetch_Entity_Locations
     (Context       : Selection_Context;
      Spec_Location : out Xref.General_Location;
      Body_Location : out Xref.General_Location);
   --  Retrive locations of the Data's Entity

   package Message_Conversions is
     new System.Address_To_Access_Conversions
       (GPS.Kernel.Messages.Abstract_Message'Class);

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Area_Context;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Area_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_Project_Only;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Context)
        and then not Has_Directory_Information (Context)
        and then not Has_File_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Editable_Project; Context : Selection_Context)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Context)
        and then Is_Editable (Project_Information (Context));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_Project_File;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Context)
        and then Has_File_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_In_Project;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_File_Information (Context)
        and then Project_Information (Context) /= No_Project;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_Entity;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Entity_Name_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_File;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_File_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_Directory;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Directory_Information (Context);
   end Filter_Matches_Primitive;

   --------------------------
   -- Set_File_Information --
   --------------------------

   procedure Set_File_Information
     (Context           : in out Selection_Context;
      Files             : GNATCOLL.VFS.File_Array :=
        GNATCOLL.VFS.Empty_File_Array;
      Project           : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Importing_Project : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Publish_Project   : Boolean := True;
      Line              : Integer := 0;
      Column            : Basic_Types.Visible_Column_Type := 0;
      Revision          : String  := "";
      Other_Revision    : String  := "";
      Tag               : String  := "";
      File_Line         : Natural := 0)
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      GNATCOLL.VFS.Unchecked_Free (Data.Files);

      if Files'Length > 0 then
         Data.Files := new GNATCOLL.VFS.File_Array'(Files);
      end if;

      Data.File_Checked             := False;
      Data.Line                     := Line;
      Data.Column                   := Column;
      Data.Creator_Provided_Project :=
        Project /= No_Project and then Publish_Project;
      Data.Project                  := Project;
      Data.Importing_Project        := Importing_Project;

      Data.Revision       := To_Unbounded_String (Revision);
      Data.Other_Revision := To_Unbounded_String (Other_Revision);
      Data.Tag            := To_Unbounded_String (Tag);
      Data.File_Line      := File_Line;
   end Set_File_Information;

   -----------------------------
   -- Has_Project_Information --
   -----------------------------

   function Has_Project_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Creator_Provided_Project;
   end Has_Project_Information;

   -------------------------
   -- Project_Information --
   -------------------------

   function Project_Information (Context : Selection_Context)
      return Project_Type is
   begin
      if Context.Ref.Is_Null then
         return No_Project;
      elsif Context.Ref.Get.Project = No_Project
        and then Has_File_Information (Context)
      then
         --  Tries to guess which project is the correct one. Since we do not
         --  have any information, we just chose the first matching one.
         declare
            F_Info : constant File_Info'Class :=
              File_Info'Class
                (Get_Registry (Get_Kernel (Context)).Tree
                 .Info_Set (File_Information (Context)).First_Element);
         begin
            Context.Ref.Get.Project := F_Info.Project;
         end;
      end if;
      return Context.Ref.Get.Project;
   end Project_Information;

   generic
      with function Check (File : Virtual_File) return Boolean;
   function Check_All (Files : File_Array) return Boolean;

   ---------------
   -- Check_All --
   ---------------

   function Check_All (Files : File_Array) return Boolean is
   begin
      if Files'Length = 0 then
         return False;

      else
         for K in Files'Range loop
            if not Check (Files (K)) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Check_All;

   -------------------------------
   -- Has_Directory_Information --
   -------------------------------

   function Has_Directory_Information (File : Virtual_File) return Boolean is
   begin
      return Dir_Name (File)'Length > 0;
   end Has_Directory_Information;

   function Has_Directory_Information
     (Context : Selection_Context) return Boolean
   is
      function Check_All_Files is new Check_All (Has_Directory_Information);
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      return not Context.Ref.Is_Null
        and then Data.Files /= null
        and then Check_All_Files (Data.Files.all);
   end Has_Directory_Information;

   ---------------------------
   -- Directory_Information --
   ---------------------------

   function Directory_Information
     (Context : Selection_Context) return Virtual_File
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Files (Data.Files'First).Is_Directory then
         return Data.Files (Data.Files'First);
      else
         return Data.Files (Data.Files'First).Get_Parent;
      end if;
   end Directory_Information;

   --------------------------
   -- Has_File_Information --
   --------------------------

   function Has_File_Information (File : Virtual_File) return Boolean is
   begin
      return Base_Name (File)'Length > 0;
   end Has_File_Information;

   function Has_File_Information
     (Context : Selection_Context) return Boolean
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
      function Check_All_Files is new Check_All (Has_File_Information);
   begin
      return not Context.Ref.Is_Null
        and then Data.Files /= null
        and then Check_All_Files (Data.Files.all);
   end Has_File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information
     (Context : Selection_Context) return Virtual_File
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Files = null or else Data.Files'Length = 0 then
         return No_File;
      else
         if not Data.File_Checked then
            declare
               Files : constant File_Array := File_Information (Context);
            begin
               return Files (Files'First);
            end;
         end if;

         return Data.Files (Data.Files'First);
      end if;
   end File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information
     (Context : Selection_Context) return File_Array
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      --  Check for $log should probably be done here!
      if not Data.File_Checked then
         for K in Data.Files'Range loop
            declare
               Name : constant Filesystem_String := Base_Name (Data.Files (K));
            begin
               if Data.Kernel /= null
                 and then Name'Length > 4
                 and then Name (Name'First .. Name'First + 3) = "ref$"
               then
                  --  This is a reference file, we have no need of it in the
                  --  context. We record then the corresponding file.
                  Data.Files (K) :=
                    Get_Registry (Data.Kernel).Tree.Create
                    (Name (Name'First + 4 .. Name'Last),
                     Use_Object_Path => False);
               end if;
            end;
         end loop;
         Data.File_Checked := True;
      end if;

      return Data.Files.all;
   end File_Information;

   ------------------------------
   -- Has_Revision_Information --
   ------------------------------

   function Has_Revision_Information
     (Context : Selection_Context) return Boolean
   is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Revision /= Null_Unbounded_String;
   end Has_Revision_Information;

   --------------------------
   -- Revision_Information --
   --------------------------

   function Revision_Information
     (Context : Selection_Context) return String is
   begin
      return To_String (Context.Ref.Get.Revision);
   end Revision_Information;

   ------------------------------------
   -- Has_Other_Revision_Information --
   ------------------------------------

   function Has_Other_Revision_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Other_Revision /= Null_Unbounded_String;
   end Has_Other_Revision_Information;

   --------------------------------
   -- Other_Revision_Information --
   --------------------------------

   function Other_Revision_Information
     (Context : Selection_Context) return String is
   begin
      return To_String (Context.Ref.Get.Other_Revision);
   end Other_Revision_Information;

   -------------------------
   -- Has_Tag_Information --
   -------------------------

   function Has_Tag_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Tag /= Null_Unbounded_String;
   end Has_Tag_Information;

   ---------------------
   -- Tag_Information --
   ---------------------

   function Tag_Information
     (Context : Selection_Context) return String is
   begin
      return To_String (Context.Ref.Get.Tag);
   end Tag_Information;

   ---------------------------------------
   -- Has_Importing_Project_Information --
   ---------------------------------------

   function Has_Importing_Project_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Importing_Project /= No_Project;
   end Has_Importing_Project_Information;

   -----------------------------------
   -- Importing_Project_Information --
   -----------------------------------

   function Importing_Project_Information
     (Context : Selection_Context) return Project_Type is
   begin
      return Context.Ref.Get.Importing_Project;
   end Importing_Project_Information;

   ------------------------------
   -- Set_Messages_Information --
   ------------------------------

   procedure Set_Messages_Information
     (Context  : in out Selection_Context;
      Messages : GPS.Kernel.Messages.Message_Array)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Addresses_Array, Addresses_Array_Access);

      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
      Index : Positive := 1;
   begin
      if Data.Messages /= null
        and then Data.Messages'Length /= Messages'Length
      then
         Unchecked_Free (Data.Messages);
      end if;

      if Messages'Length = 0 then
         return;
      end if;

      if Data.Messages = null then
         Data.Messages := new Addresses_Array (1 .. Messages'Length);
      end if;

      for I in Messages'Range loop
         Data.Messages (Index) := Message_Conversions.To_Address
           (Message_Conversions.Object_Pointer (Messages (I)));
         Index := Index + 1;
      end loop;
   end Set_Messages_Information;

   --------------------------
   -- Has_Line_Information --
   --------------------------

   function Has_Line_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Line /= 0;
   end Has_Line_Information;

   ----------------------
   -- Line_Information --
   ----------------------

   function Line_Information (Context : Selection_Context) return Integer is
   begin
      return Context.Ref.Get.Line;
   end Line_Information;

   -------------------------------
   -- Has_File_Line_Information --
   -------------------------------

   function Has_File_Line_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.File_Line /= 0;
   end Has_File_Line_Information;

   ---------------------------
   -- File_Line_Information --
   ---------------------------

   function File_Line_Information
     (Context : Selection_Context) return Natural is
   begin
      return Context.Ref.Get.File_Line;
   end File_Line_Information;

   ----------------------------
   -- Has_Column_Information --
   ----------------------------

   function Has_Column_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Column /= 0;
   end Has_Column_Information;

   ------------------------
   -- Column_Information --
   ------------------------

   function Column_Information
     (Context : Selection_Context) return Basic_Types.Visible_Column_Type is
   begin
      return Context.Ref.Get.Column;
   end Column_Information;

   -----------------------------
   -- Has_Message_Information --
   -----------------------------

   function Has_Message_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and Context.Ref.Get.Messages /= null;
   end Has_Message_Information;

   --------------------------
   -- Messages_Information --
   --------------------------

   function Messages_Information
     (Context : Selection_Context) return GPS.Kernel.Messages.Message_Array
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Messages = null then
         return (1 .. 0 => <>);

      else
         return Result : GPS.Kernel.Messages.Message_Array
           (1 .. Data.Messages'Last)
         do
            for Index in Result'Range loop
               Result (Index) := GPS.Kernel.Messages.Message_Access
                 (Message_Conversions.To_Pointer (Data.Messages (Index)));
            end loop;
         end return;
      end if;
   end Messages_Information;

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context         : in out Selection_Context;
      Entity_Name     : String;
      Entity_Column   : Basic_Types.Visible_Column_Type := 0;
      From_Expression : String := "")
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      Data.Entity_Name   := To_Unbounded_String (Entity_Name);
      Data.Entity_Column := Entity_Column;
      Data.Expression    := To_Unbounded_String (From_Expression);
   end Set_Entity_Information;

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context         : in out Selection_Context;
      Entity          : Xref.Root_Entity'Class;
      From_Expression : String := "")
   is
      Decl   : constant General_Location := Entity.Get_Declaration.Loc;
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      Data.Entity_Name   := To_Unbounded_String (Entity.Get_Name);
      Data.Entity_Column := Decl.Column;
      Data.Xref_Entity.Replace_Element (Entity);
      Data.Expression := To_Unbounded_String (From_Expression);
   end Set_Entity_Information;

   --------------------------------
   -- Has_Expression_Information --
   --------------------------------

   function Has_Expression_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Expression /= Null_Unbounded_String;
   end Has_Expression_Information;

   ----------------------------
   -- Expression_Information --
   ----------------------------

   function Expression_Information
     (Context : Selection_Context) return String is
   begin
      return To_String (Context.Ref.Get.Expression);
   end Expression_Information;

   ---------------------------------
   -- Has_Entity_Name_Information --
   ---------------------------------

   function Has_Entity_Name_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Entity_Name /= Null_Unbounded_String;
   end Has_Entity_Name_Information;

   -----------------------------
   -- Entity_Name_Information --
   -----------------------------

   function Entity_Name_Information
     (Context : Selection_Context) return String is
   begin
      return To_String (Context.Ref.Get.Entity_Name);
   end Entity_Name_Information;

   -----------------------------------
   -- Has_Entity_Column_Information --
   -----------------------------------

   function Has_Entity_Column_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Entity_Column /= 0;
   end Has_Entity_Column_Information;

   -------------------------------
   -- Entity_Column_Information --
   -------------------------------

   function Entity_Column_Information
     (Context : Selection_Context) return Basic_Types.Visible_Column_Type is
   begin
      return Context.Ref.Get.Entity_Column;
   end Entity_Column_Information;

   --------------------------
   -- Set_Area_Information --
   --------------------------

   procedure Set_Area_Information
     (Context    : in out Selection_Context;
      Text       : String;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0)
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      Data.Text       := To_Unbounded_String (Text);
      Data.Start_Line := Start_Line;
      Data.End_Line   := End_Line;
   end Set_Area_Information;

   --------------------------
   -- Has_Area_Information --
   --------------------------

   function Has_Area_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then
          (Context.Ref.Get.Text /= Null_Unbounded_String
           or else
             not (Context.Ref.Get.Start_Line = 0
                  and then Context.Ref.Get.End_Line = 0));
   end Has_Area_Information;

   --------------
   -- Get_Area --
   --------------

   procedure Get_Area
     (Context    : Selection_Context;
      Start_Line : out Integer;
      End_Line   : out Integer) is
   begin
      Start_Line := Context.Ref.Get.Start_Line;
      End_Line   := Context.Ref.Get.End_Line;
   end Get_Area;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Context           : Selection_Context;
      Approximate_Search_Fallback : Boolean := True)
      return Xref.Root_Entity'Class
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
      Db   : constant General_Xref_Database := Data.Kernel.Databases;
   begin
      --  If we have never attempted to get the actual location of the entity,
      --  do so now.

      if not Data.Xref_Entity_Resolution_Attempted then
         --  We are attempting resolution now
         Data.Xref_Entity_Resolution_Attempted := True;

         if Has_File_Information (Context)
           and then Has_Line_Information (Context)
           and then Data.Entity_Name /= Null_Unbounded_String
         then
            Data.Xref_Entity.Replace_Element
              (Db.Get_Entity
                 (Loc  =>
                      (File    => Data.Files (Data.Files'First),
                       Project_Path => Data.Project.Project_Path,
                       Line    => Data.Line,
                       Column  => Data.Entity_Column),
                  Name => To_String (Data.Entity_Name),
                  Closest_Ref => Data.Xref_Closest_Ref,
                  Approximate_Search_Fallback => Approximate_Search_Fallback));
         end if;
      end if;

      if Data.Xref_Entity.Is_Empty then
         return No_Root_Entity;
      else
         return Data.Xref_Entity.Element;
      end if;
   end Get_Entity;

   ----------------------------
   -- Fetch_Entity_Locations --
   ----------------------------

   procedure Fetch_Entity_Locations
     (Context       : Selection_Context;
      Spec_Location : out Xref.General_Location;
      Body_Location : out Xref.General_Location)
   is
      Entity : constant Root_Entity'Class := Get_Entity (Context);
      Data   : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Entity /= No_Root_Entity then
         Spec_Location := Get_Declaration (Entity).Loc;
         Body_Location := Get_Body (Entity);
      else
         Spec_Location := No_Location;
         Body_Location := No_Location;
      end if;

      Data.Entity_Locations := (True, Spec_Location, Body_Location);
   end Fetch_Entity_Locations;

   --------------------------
   -- Get_Entity_Locations --
   --------------------------

   procedure Get_Entity_Locations
     (Context       : Selection_Context;
      Spec_Location : out Xref.General_Location;
      Body_Location : out Xref.General_Location)
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Entity_Locations.Is_Fetched then
         Spec_Location := Data.Entity_Locations.Spec_Location;
         Body_Location := Data.Entity_Locations.Body_Location;

      else
         Fetch_Entity_Locations (Context, Spec_Location, Body_Location);
      end if;
   end Get_Entity_Locations;

   -------------------------------
   -- Get_Entity_Spec_Locations --
   -------------------------------

   procedure Get_Entity_Spec_Locations
     (Context  : Selection_Context;
      Location : out Xref.General_Location)
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Entity_Locations.Is_Fetched then
         Location := Data.Entity_Locations.Spec_Location;
      else
         declare
            Body_Location : Xref.General_Location with Unreferenced;
         begin
            Fetch_Entity_Locations (Context, Location, Body_Location);
         end;
      end if;
   end Get_Entity_Spec_Locations;

   ------------------------
   -- Get_Entity_Type_Of --
   ------------------------

   function Get_Entity_Type_Of
     (Context           : Selection_Context)
      return Xref.Root_Entity'Class
   is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Xref_Entity_Type_Of.Is_Empty then
         if Get_Entity (Context) = No_Root_Entity then
            return No_Root_Entity;
         end if;

         Data.Xref_Entity_Type_Of.Replace_Element
           (Get_Type_Of (Get_Entity (Context)));
      end if;

      return Data.Xref_Entity_Type_Of.Element;
   end Get_Entity_Type_Of;

   ----------------------
   -- Has_Parent_Types --
   ----------------------

   function Has_Parent_Types
     (Context           : Selection_Context)
      return Boolean
   is
      use GNATCOLL.Tribooleans;
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.Xref_Entity_Has_Parent_Types = Indeterminate
        and then Get_Entity (Context) /= No_Root_Entity
      then
         declare
            Parents : constant Xref.Entity_Array :=
              Parent_Types (Get_Entity (Context), Recursive => False);
         begin
            Data.Xref_Entity_Has_Parent_Types :=
              To_TriBoolean (Parents'Length /= 0);
         end;
      end if;

      return To_Boolean (Data.Xref_Entity_Has_Parent_Types);
   end Has_Parent_Types;

   ---------------------
   -- Get_Closest_Ref --
   ---------------------

   function Get_Closest_Ref
     (Context : Selection_Context) return Root_Entity_Reference'Class
   is
   begin
      return Context.Ref.Get.Xref_Closest_Ref.Element;
   end Get_Closest_Ref;

   -----------------------
   -- Get_File_Language --
   -----------------------

   function Get_File_Language (Context : Selection_Context) return String is
      Data : constant Selection_Pointers.Reference_Type := Context.Ref.Get;
   begin
      if Data.File_Lang = Null_Unbounded_String
        and then Has_File_Information (Context)
      then
         declare
            F_Info : constant File_Info'Class :=
              File_Info'Class
                (Get_Registry (Get_Kernel (Context)).Tree
                 .Info_Set (File_Information (Context)).First_Element);
         begin
            Data.File_Lang := To_Unbounded_String (F_Info.Language);
         end;
      end if;

      return To_String (Data.File_Lang);
   end Get_File_Language;

   ------------------------------
   -- Set_Activity_Information --
   ------------------------------

   procedure Set_Activity_Information
     (Context : in out Selection_Context;
      Id      : String) is
   begin
      Context.Ref.Get.Activities.Clear;
      String_List_Utils.String_List.Append (Context.Ref.Get.Activities, Id);
   end Set_Activity_Information;

   procedure Set_Activity_Information
     (Context    : in out Selection_Context;
      Activities : String_List_Utils.String_List.Vector) is
   begin
      Context.Ref.Get.Activities := Activities;
   end Set_Activity_Information;

   --------------------------
   -- Activity_Information --
   --------------------------

   function Activity_Information
     (Context : Selection_Context)
      return String_List_Utils.String_List.Vector is
   begin
      return Context.Ref.Get.Activities;
   end Activity_Information;

   ------------------------------
   -- Has_Activity_Information --
   ------------------------------

   function Has_Activity_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not String_List_Utils.String_List.Is_Empty
        (Context.Ref.Get.Activities);
   end Has_Activity_Information;

   ----------------------
   -- Text_Information --
   ----------------------

   function Text_Information (Context : Selection_Context) return String is
   begin
      return To_String (Context.Ref.Get.Text);
   end Text_Information;

   -----------------------------
   -- Set_Is_Dispatching_Call --
   -----------------------------

   procedure Set_Is_Dispatching_Call
     (Context : Selection_Context; Is_Dispatching : Boolean) is
   begin
      Context.Ref.Get.Is_Dispatching_Call := To_TriBoolean (Is_Dispatching);
   end Set_Is_Dispatching_Call;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   function Is_Dispatching_Call
     (Context : Selection_Context) return GNATCOLL.Tribooleans.Triboolean is
   begin
      if Context.Ref.Is_Null then
         return Indeterminate;
      else
         return Context.Ref.Get.Is_Dispatching_Call;
      end if;
   end Is_Dispatching_Call;

   -----------------------------
   -- Set_Browser_Information --
   -----------------------------

   procedure Set_Browser_Information
     (Context : in out Selection_Context;
      Details : Gtkada.Canvas_View.Canvas_Event_Details)
   is
   begin
      Context.Ref.Get.Has_Browser_Details := True;
      Context.Ref.Get.Browser_Details := Details;
   end Set_Browser_Information;

   -----------------------------
   -- Has_Browser_Information --
   -----------------------------

   function Has_Browser_Information
     (Context : Selection_Context) return Boolean is
   begin
      return not Context.Ref.Is_Null
        and then Context.Ref.Get.Has_Browser_Details;
   end Has_Browser_Information;

   -------------------------
   -- Browser_Information --
   -------------------------

   function Browser_Information
     (Context : Selection_Context)
      return Gtkada.Canvas_View.Canvas_Event_Details is
   begin
      if Context.Ref.Is_Null then
         return Canvas_Event_Details'(others => <>);
      else
         return Context.Ref.Get.Browser_Details;
      end if;
   end Browser_Information;

   ------------------------------
   -- Register_Default_Filters --
   ------------------------------

   procedure Register_Default_Filters
     (Kernel : access Kernel_Handle_Record'Class)
   is
      File_Filter         : constant Action_Filter := new Filter_File;
      Directory_Filter    : constant Action_Filter := new Filter_Directory;
      Entity_Filter       : constant Action_Filter := new Filter_Entity;
      Project_File_Filter : constant Action_Filter := new Filter_Project_File;
      Project_Only_Filter : constant Action_Filter := new Filter_Project_Only;
      In_Project_Filter   : constant Action_Filter := new Filter_In_Project;
      Editable_Project    : constant Action_Filter :=
                              new Filter_Editable_Project;
   begin
      Register_Filter (Kernel, File_Filter, "File");
      Register_Filter (Kernel, Directory_Filter, "Directory");
      Register_Filter (Kernel, Entity_Filter, "Entity");
      Register_Filter (Kernel, Project_Only_Filter, "Project only");
      Register_Filter (Kernel, Editable_Project, "Editable Project");
      Register_Filter (Kernel, Project_File_Filter, "Project and file");
      Register_Filter (Kernel, In_Project_Filter, "In project");
   end Register_Default_Filters;

end GPS.Kernel.Contexts;
