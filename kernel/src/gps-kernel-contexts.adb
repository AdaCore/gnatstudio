-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007, AdaCore              --
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

with GNAT.OS_Lib;        use GNAT.OS_Lib;

with Basic_Types;        use Basic_Types;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with Entities;           use Entities;
with Entities.Queries;   use Entities.Queries;
with Language_Handlers;  use Language_Handlers;
with Projects;           use Projects;
with Projects.Registry;  use Projects.Registry;
with VFS;                use VFS;

package body GPS.Kernel.Contexts is

   type Filter_File is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_File; Ctxt : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Directory is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Directory; Ctxt : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Entity is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Entity; Ctxt : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Project_Only is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Project_Only; Ctxt : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Editable_Project is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Editable_Project; Ctxt : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_Project_File is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_Project_File; Ctxt : Selection_Context)
      return Boolean;
   --  See inherited documentation

   type Filter_In_Project is new GPS.Kernel.Action_Filter_Record
   with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Filter_In_Project;
      Context : Selection_Context) return Boolean;
   --  True if the current file belongs to an opened project

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
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

   function Filter_Matches_Primitive
     (Filter  : access Filter_Project_Only;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Ctxt)
        and then not Has_Directory_Information (Ctxt)
        and then not Has_File_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter : access Filter_Editable_Project; Ctxt : Selection_Context)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Ctxt)
        and then Is_Editable (Project_Information (Ctxt));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Project_File;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Ctxt)
        and then Has_File_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_In_Project;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      return Has_File_Information (Context)
        and then Get_Project_From_File
          (Get_Registry (Kernel).all, File_Information (Context), False) /=
          No_Project;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Entity;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Entity_Name_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_File;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_File_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Directory;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Directory_Information (Ctxt);
   end Filter_Matches_Primitive;

   --------------------------
   -- Set_File_Information --
   --------------------------

   procedure Set_File_Information
     (Context           : in out Selection_Context;
      File              : VFS.Virtual_File := VFS.No_File;
      Project           : Projects.Project_Type := Projects.No_Project;
      Importing_Project : Projects.Project_Type := Projects.No_Project;
      Line              : Integer := 0;
      Column            : Basic_Types.Visible_Column_Type := 0;
      Revision          : String := "";
      Tag               : String := "") is
   begin
      Context.Data.Data.File                     := File;
      Context.Data.Data.File_Checked             := False;
      Context.Data.Data.Line                     := Line;
      Context.Data.Data.Column                   := Column;
      Context.Data.Data.Creator_Provided_Project := Project /= No_Project;
      Context.Data.Data.Project                  := Project;
      Context.Data.Data.Importing_Project        := Importing_Project;

      if Revision /= "" then
         Context.Data.Data.Revision := new String'(Revision);
      end if;

      if Tag /= "" then
         Context.Data.Data.Tag := new String'(Tag);
      end if;
   end Set_File_Information;

   -----------------------------
   -- Has_Project_Information --
   -----------------------------

   function Has_Project_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Creator_Provided_Project;
   end Has_Project_Information;

   -------------------------
   -- Project_Information --
   -------------------------

   function Project_Information (Context : Selection_Context)
      return Projects.Project_Type is
   begin
      if Context.Data.Data.Project = No_Project
        and then Has_File_Information (Context)
      then
         Context.Data.Data.Project := Get_Project_From_File
           (Get_Registry (Get_Kernel (Context)).all,
            File_Information (Context));
      end if;
      return Context.Data.Data.Project;
   end Project_Information;

   -------------------------------
   -- Has_Directory_Information --
   -------------------------------

   function Has_Directory_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Dir_Name (Context.Data.Data.File).all /= "";
   end Has_Directory_Information;

   ---------------------------
   -- Directory_Information --
   ---------------------------

   function Directory_Information
     (Context : Selection_Context) return String is
   begin
      return Dir_Name (Context.Data.Data.File).all;
   end Directory_Information;

   --------------------------
   -- Has_File_Information --
   --------------------------

   function Has_File_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Base_Name (Context.Data.Data.File) /= "";
   end Has_File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information
     (Context : Selection_Context) return Virtual_File is
   begin
      if not Context.Data.Data.File_Checked then
         declare
            Name : constant String := Base_Name (Context.Data.Data.File);
         begin
            if Context.Data.Data.Kernel /= null
              and then Name'Length > 4
              and then Name (Name'First .. Name'First + 3) = "ref$"
            then
               --  This is a reference file, we have no need of it in the
               --  context. We record then the corresponding file.
               Context.Data.Data.File := Create
                 (Get_Full_Path_From_File
                    (Get_Registry (Context.Data.Data.Kernel).all,
                     Name (Name'First + 4 .. Name'Last),
                     Use_Source_Path => True,
                     Use_Object_Path => False));
            end if;
         end;

         Context.Data.Data.File_Checked := True;
      end if;

      return Context.Data.Data.File;
   end File_Information;

   ------------------------------
   -- Has_Revision_Information --
   ------------------------------

   function Has_Revision_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Revision /= null;
   end Has_Revision_Information;

   --------------------------
   -- Revision_Information --
   --------------------------

   function Revision_Information
     (Context : Selection_Context) return String is
   begin
      return Context.Data.Data.Revision.all;
   end Revision_Information;

   -------------------------
   -- Has_Tag_Information --
   -------------------------

   function Has_Tag_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Tag /= null;
   end Has_Tag_Information;

   ---------------------
   -- Tag_Information --
   ---------------------

   function Tag_Information
     (Context : Selection_Context) return String is
   begin
      return Context.Data.Data.Tag.all;
   end Tag_Information;

   ---------------------------------------
   -- Has_Importing_Project_Information --
   ---------------------------------------

   function Has_Importing_Project_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Importing_Project /= No_Project;
   end Has_Importing_Project_Information;

   -----------------------------------
   -- Importing_Project_Information --
   -----------------------------------

   function Importing_Project_Information
     (Context : Selection_Context) return Project_Type is
   begin
      return Context.Data.Data.Importing_Project;
   end Importing_Project_Information;

   -----------------------------
   -- Set_Message_Information --
   -----------------------------

   procedure Set_Message_Information
     (Context     : in out Selection_Context;
      Category    : String := "";
      Message     : String := "") is
   begin
      Free (Context.Data.Data.Category_Name);
      if Category /= "" then
         Context.Data.Data.Category_Name := new String'(Category);
      end if;

      Free (Context.Data.Data.Message);
      if Message /= "" then
         Context.Data.Data.Message := new String'(Message);
      end if;
   end Set_Message_Information;

   --------------------------
   -- Has_Line_Information --
   --------------------------

   function Has_Line_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Line /= 0;
   end Has_Line_Information;

   ----------------------
   -- Line_Information --
   ----------------------

   function Line_Information (Context : Selection_Context) return Integer is
   begin
      return Context.Data.Data.Line;
   end Line_Information;

   ----------------------------
   -- Has_Column_Information --
   ----------------------------

   function Has_Column_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Column /= 0;
   end Has_Column_Information;

   ------------------------
   -- Column_Information --
   ------------------------

   function Column_Information
     (Context : Selection_Context) return Basic_Types.Visible_Column_Type is
   begin
      return Context.Data.Data.Column;
   end Column_Information;

   ------------------------------
   -- Has_Category_Information --
   ------------------------------

   function Has_Category_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Category_Name /= null;
   end Has_Category_Information;

   --------------------------
   -- Category_Information --
   --------------------------

   function Category_Information
     (Context : Selection_Context) return String is
   begin
      return Context.Data.Data.Category_Name.all;
   end Category_Information;

   -----------------------------
   -- Has_Message_Information --
   -----------------------------

   function Has_Message_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Message /= null;
   end Has_Message_Information;

   -------------------------
   -- Message_Information --
   -------------------------

   function Message_Information
     (Context : Selection_Context) return String is
   begin
      return Context.Data.Data.Message.all;
   end Message_Information;

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context       : in out Selection_Context;
      Entity_Name   : String := "";
      Entity_Column : Basic_Types.Visible_Column_Type := 0) is
   begin
      Free (Context.Data.Data.Entity_Name);
      if Entity_Name /= "" then
         Context.Data.Data.Entity_Name := new String'(Entity_Name);
      end if;

      Context.Data.Data.Entity_Column := Entity_Column;
      Context.Data.Data.Entity_Resolved := Entity_Not_Found;
   end Set_Entity_Information;

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context       : in out Selection_Context;
      Entity        : access Entities.Entity_Information_Record'Class) is
   begin
      Ref (Entity_Information (Entity));
      Free (Context.Data.Data.Entity_Name);
      Context.Data.Data.Entity_Name     :=
        new String'(Get_Name (Entity_Information (Entity)).all);
      Context.Data.Data.Entity_Column   :=
        Get_Column (Get_Declaration_Of (Entity_Information (Entity)));
      Context.Data.Data.Entity          := Entity_Information (Entity);
      Context.Data.Data.Entity_Resolved := Success;
   end Set_Entity_Information;

   ---------------------------------
   -- Has_Entity_Name_Information --
   ---------------------------------

   function Has_Entity_Name_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Entity_Name /= null;
   end Has_Entity_Name_Information;

   -----------------------------
   -- Entity_Name_Information --
   -----------------------------

   function Entity_Name_Information
     (Context : Selection_Context) return String is
   begin
      if Context.Data.Data.Entity_Name = null then
         return "";
      else
         return Context.Data.Data.Entity_Name.all;
      end if;
   end Entity_Name_Information;

   -----------------------------------
   -- Has_Entity_Column_Information --
   -----------------------------------

   function Has_Entity_Column_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Entity_Column /= 0;
   end Has_Entity_Column_Information;

   -------------------------------
   -- Entity_Column_Information --
   -------------------------------

   function Entity_Column_Information
     (Context : Selection_Context) return Basic_Types.Visible_Column_Type is
   begin
      return Context.Data.Data.Entity_Column;
   end Entity_Column_Information;

   --------------------------
   -- Set_Area_Information --
   --------------------------

   procedure Set_Area_Information
     (Context    : in out Selection_Context;
      Text       : String;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0) is
   begin
      Free (Context.Data.Data.Text);
      Context.Data.Data.Text       := new String'(Text);
      Context.Data.Data.Start_Line := Start_Line;
      Context.Data.Data.End_Line   := End_Line;
   end Set_Area_Information;

   --------------------------
   -- Has_Area_Information --
   --------------------------

   function Has_Area_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Text /= null;
   end Has_Area_Information;

   --------------
   -- Get_Area --
   --------------

   procedure Get_Area
     (Context    : Selection_Context;
      Start_Line : out Integer;
      End_Line   : out Integer) is
   begin
      Start_Line := Context.Data.Data.Start_Line;
      End_Line   := Context.Data.Data.End_Line;
   end Get_Area;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Context           : Selection_Context;
      Ask_If_Overloaded : Boolean := False) return Entity_Information
   is
      File   : Source_File;
      Never_Examined : constant Boolean :=
        Context.Data.Data.Entity_Resolved = Entity_Not_Found
        or else (Context.Data.Data.Entity_Resolved = Overloaded_Entity_Found
                 and then Ask_If_Overloaded);

   begin
      if Context.Data.Data.Entity_Resolved = Overloaded_Entity_Found
        and then not Ask_If_Overloaded
      then
         return Context.Data.Data.Entity;

      elsif Never_Examined
        and then Has_Entity_Name_Information (Context)
        and then Has_Line_Information (Context)
        and then Has_File_Information (Context)
      then
         File := Get_Or_Create
           (Db   => Get_Database (Get_Kernel (Context)),
            File => File_Information (Context),
            Handler => Get_LI_Handler_From_File
              (Get_Language_Handler (Get_Kernel (Context)),
               File_Information (Context)));

         if File = null then
            Context.Data.Data.Entity_Resolved := Success;
            return null;
         end if;

         Find_Declaration_Or_Overloaded
           (Kernel            => Get_Kernel (Context),
            File              => File,
            Entity_Name       => Entity_Name_Information (Context),
            Line              => Line_Information (Context),
            Column            => Entity_Column_Information (Context),
            Ask_If_Overloaded => Ask_If_Overloaded,
            Entity            => Context.Data.Data.Entity,
            Closest_Ref       => Context.Data.Data.Closest_Ref,
            Status            => Context.Data.Data.Entity_Resolved);

         if Context.Data.Data.Entity_Resolved = Fuzzy_Match
           or else Context.Data.Data.Entity_Resolved = Overloaded_Entity_Found
         then
            if Ask_If_Overloaded then
               Context.Data.Data.Entity_Resolved := Success;
            end if;
         elsif Context.Data.Data.Entity_Resolved /= Success then
            Context.Data.Data.Entity := null;
            Context.Data.Data.Closest_Ref := No_Entity_Reference;
            Context.Data.Data.Entity_Resolved := Success;
         end if;

         Ref (Context.Data.Data.Entity);
      else
         Context.Data.Data.Entity_Resolved := Success;
      end if;

      return Context.Data.Data.Entity;
   end Get_Entity;

   ---------------------
   -- Get_Closest_Ref --
   ---------------------

   function Get_Closest_Ref
     (Context           : Selection_Context)
      return Entities.Entity_Reference
   is
      --  Make sure the info is computed
      Entity : constant Entity_Information :=
        Get_Entity (Context, Ask_If_Overloaded => False);
      pragma Unreferenced (Entity);
   begin
      if Context.Data.Data.Entity_Resolved = Success then
         return Context.Data.Data.Closest_Ref;
      else
         return No_Entity_Reference;
      end if;
   end Get_Closest_Ref;

   ------------------------------
   -- Set_Activity_Information --
   ------------------------------

   procedure Set_Activity_Information
     (Context : in out Selection_Context;
      Id      : String) is
   begin
      Free (Context.Data.Data.Activity_Id);
      Context.Data.Data.Activity_Id := new String'(Id);
   end Set_Activity_Information;

   --------------------------
   -- Activity_Information --
   --------------------------

   function Activity_Information
     (Context : Selection_Context) return String is
   begin
      return Context.Data.Data.Activity_Id.all;
   end Activity_Information;

   ------------------------------
   -- Has_Activity_Information --
   ------------------------------

   function Has_Activity_Information
     (Context : Selection_Context) return Boolean is
   begin
      return Context.Data.Data /= null
        and then Context.Data.Data.Activity_Id /= null;
   end Has_Activity_Information;

   ----------------------
   -- Text_Information --
   ----------------------

   function Text_Information
     (Context : Selection_Context) return String is
   begin
      if Context.Data.Data.Text /= null then
         return Context.Data.Data.Text.all;
      else
         return "";
      end if;
   end Text_Information;

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
      Editable_Project : constant Action_Filter := new Filter_Editable_Project;
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
