-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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

with Projects;           use Projects;
with Projects.Registry;  use Projects.Registry;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with VFS;                use VFS;
with Entities;           use Entities;
with Entities.Queries;   use Entities.Queries;

package body GPS.Kernel.Contexts is

   type Filter_File is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Filter_File;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean;
   --  See inherited documentation

   type Filter_Directory is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Filter_Directory;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean;
   --  See inherited documentation

   type Filter_Entity is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Filter_Entity;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean;
   --  See inherited documentation

   type Filter_Project_Only is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Filter_Project_Only;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean;
   --  See inherited documentation

   type Filter_Project_File is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Filter_Project_File;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean;
   --  See inherited documentation

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Is_Area_Context;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Context.all in File_Area_Context'Class;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Project_Only;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Ctxt.all in File_Selection_Context'Class
        and then Has_Project_Information (File_Selection_Context_Access (Ctxt))
        and then not Has_Directory_Information
          (File_Selection_Context_Access (Ctxt))
        and then not Has_File_Information
          (File_Selection_Context_Access (Ctxt));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Project_File;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Ctxt.all in File_Selection_Context'Class
        and then Has_Project_Information
          (File_Selection_Context_Access (Ctxt))
        and then Has_File_Information
          (File_Selection_Context_Access (Ctxt));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Entity;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Ctxt.all in Entity_Selection_Context'Class
        and then Has_Entity_Name_Information
          (Entity_Selection_Context_Access (Ctxt));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_File;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Ctxt.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Ctxt));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Filter_Directory;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Ctxt.all in File_Selection_Context'Class
        and then Has_Directory_Information
          (File_Selection_Context_Access (Ctxt));
   end Filter_Matches_Primitive;

   --------------------------
   -- Set_File_Information --
   --------------------------

   procedure Set_File_Information
     (Context           : access File_Selection_Context;
      File              : VFS.Virtual_File := VFS.No_File;
      Project           : Projects.Project_Type := Projects.No_Project;
      Importing_Project : Projects.Project_Type := Projects.No_Project;
      Line              : Integer := 0;
      Column            : Integer := 0) is
   begin
      Context.File                     := File;
      Context.Line                     := Line;
      Context.Column                   := Column;
      Context.Creator_Provided_Project := Project /= No_Project;
      Context.Project                  := Project;
      Context.Importing_Project        := Importing_Project;
   end Set_File_Information;

   -----------------------------
   -- Has_Project_Information --
   -----------------------------

   function Has_Project_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Creator_Provided_Project;
   end Has_Project_Information;

   -------------------------
   -- Project_Information --
   -------------------------

   function Project_Information (Context : access File_Selection_Context)
      return Projects.Project_Type is
   begin
      if Context.Project = No_Project
        and then Has_File_Information (Context)
      then
         Context.Project := Get_Project_From_File
           (Get_Registry (Get_Kernel (Context)).all,
            File_Information (Context));
      end if;
      return Context.Project;
   end Project_Information;

   -------------------------------
   -- Has_Directory_Information --
   -------------------------------

   function Has_Directory_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Dir_Name (Context.File).all /= "";
   end Has_Directory_Information;

   ---------------------------
   -- Directory_Information --
   ---------------------------

   function Directory_Information
     (Context : access File_Selection_Context) return String is
   begin
      return Dir_Name (Context.File).all;
   end Directory_Information;

   --------------------------
   -- Has_File_Information --
   --------------------------

   function Has_File_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Base_Name (Context.File) /= "";
   end Has_File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information
     (Context  : access File_Selection_Context) return Virtual_File is
   begin
      if Context.Checked then
         return Context.File;

      else
         declare
            Name : constant String := Base_Name (Context.File);
         begin
            if Context.Kernel /= null
              and then Name'Length > 4
              and then Name (Name'First .. Name'First + 3) = "ref$"
            then
               --  This is a reference file, we have no need of it in the
               --  context. We record then the corresponding file.
               Context.File := Create
                 (Get_Full_Path_From_File
                    (Get_Registry (Context.Kernel).all,
                     Name (Name'First + 4 .. Name'Last),
                     Use_Source_Path => True,
                     Use_Object_Path => False));
            end if;
         end;

         Context.Checked := True;
         return Context.File;
      end if;
   end File_Information;

   ---------------------------------------
   -- Has_Importing_Project_Information --
   ---------------------------------------

   function Has_Importing_Project_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Importing_Project /= No_Project;
   end Has_Importing_Project_Information;

   -----------------------------------
   -- Importing_Project_Information --
   -----------------------------------

   function Importing_Project_Information
     (Context : access File_Selection_Context) return Project_Type is
   begin
      return Context.Importing_Project;
   end Importing_Project_Information;

   -----------------------------
   -- Set_Message_Information --
   -----------------------------

   procedure Set_Message_Information
     (Context     : access Message_Context;
      Category    : String := "";
      Message     : String := "") is
   begin
      Free (Context.Category_Name);
      if Category /= "" then
         Context.Category_Name := new String'(Category);
      end if;

      Free (Context.Message);
      if Message /= "" then
         Context.Message := new String'(Message);
      end if;

   end Set_Message_Information;

   --------------------------
   -- Has_Line_Information --
   --------------------------

   function Has_Line_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Line /= 0;
   end Has_Line_Information;

   ----------------------
   -- Line_Information --
   ----------------------

   function Line_Information
     (Context : access File_Selection_Context) return Integer is
   begin
      return Context.Line;
   end Line_Information;

   ----------------------------
   -- Has_Column_Information --
   ----------------------------

   function Has_Column_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Column /= 0;
   end Has_Column_Information;

   ------------------------
   -- Column_Information --
   ------------------------

   function Column_Information
     (Context : access File_Selection_Context) return Integer is
   begin
      return Context.Column;
   end Column_Information;

   ------------------------------
   -- Has_Category_Information --
   ------------------------------

   function Has_Category_Information
     (Context : access Message_Context) return Boolean is
   begin
      return Context.Category_Name /= null;
   end Has_Category_Information;

   --------------------------
   -- Category_Information --
   --------------------------

   function Category_Information
     (Context : access Message_Context) return String is
   begin
      return Context.Category_Name.all;
   end Category_Information;

   -----------------------------
   -- Has_Message_Information --
   -----------------------------

   function Has_Message_Information
     (Context : access Message_Context) return Boolean is
   begin
      return Context.Message /= null;
   end Has_Message_Information;

   -------------------------
   -- Message_Information --
   -------------------------

   function Message_Information
     (Context : access Message_Context) return String is
   begin
      return Context.Message.all;
   end Message_Information;

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context       : access Entity_Selection_Context;
      Entity_Name   : String := "";
      Entity_Column : Integer := 0) is
   begin
      Free (Context.Entity_Name);
      if Entity_Name /= "" then
         Context.Entity_Name := new String'(Entity_Name);
      end if;

      Context.Entity_Column := Entity_Column;
   end Set_Entity_Information;

   ---------------------------------
   -- Has_Entity_Name_Information --
   ---------------------------------

   function Has_Entity_Name_Information
     (Context : access Entity_Selection_Context) return Boolean is
   begin
      return Context.Entity_Name /= null;
   end Has_Entity_Name_Information;

   -----------------------------
   -- Entity_Name_Information --
   -----------------------------

   function Entity_Name_Information
     (Context : access Entity_Selection_Context) return String is
   begin
      if Context.Entity_Name = null then
         return "";
      else
         return Context.Entity_Name.all;
      end if;
   end Entity_Name_Information;

   -----------------------------------
   -- Has_Entity_Column_Information --
   -----------------------------------

   function Has_Entity_Column_Information
     (Context : access Entity_Selection_Context) return Boolean is
   begin
      return Context.Entity_Column /= 0;
   end Has_Entity_Column_Information;

   -------------------------------
   -- Entity_Column_Information --
   -------------------------------

   function Entity_Column_Information
     (Context : access Entity_Selection_Context) return Integer is
   begin
      return Context.Entity_Column;
   end Entity_Column_Information;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out File_Selection_Context) is
   begin
      GPS.Kernel.Destroy (Selection_Context (Context));
   end Destroy;

   --------------------------
   -- Set_Area_Information --
   --------------------------

   procedure Set_Area_Information
     (Context    : access File_Area_Context;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0) is
   begin
      Context.Start_Line := Start_Line;
      Context.End_Line   := End_Line;
   end Set_Area_Information;

   --------------
   -- Get_Area --
   --------------

   procedure Get_Area
     (Context    : access File_Area_Context;
      Start_Line : out Integer;
      End_Line   : out Integer) is
   begin
      Start_Line := Context.Start_Line;
      End_Line   := Context.End_Line;
   end Get_Area;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Context : access Entity_Selection_Context)
      return Entities.Entity_Information
   is
      Status : Find_Decl_Or_Body_Query_Status;
      File   : Source_File;

   begin
      if Context.Entity = null
        and then Has_Entity_Name_Information (Context)
        and then Has_Line_Information (Context)
        and then Has_File_Information (Context)
      then
         File := Get_Or_Create
           (Db   => Get_Database (Get_Kernel (Context)),
            File => File_Information (Context),
            Handler => Get_LI_Handler
              (Get_Database (Get_Kernel (Context)),
               File_Information (Context)));

         if File = null then
            return null;
         end if;

         Find_Declaration_Or_Overloaded
           (Kernel      => Get_Kernel (Context),
            File        => File,
            Entity_Name => Entity_Name_Information (Context),
            Line        => Line_Information (Context),
            Column      => Entity_Column_Information (Context),
            Entity      => Context.Entity,
            Status      => Status);

         if Status /= Success and then Status /= Fuzzy_Match then
            Context.Entity := null;
         end if;

         Ref (Context.Entity);
      end if;

      return Context.Entity;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Entity_Selection_Context) is
   begin
      Unref (Context.Entity);
      Destroy (File_Selection_Context (Context));
      Free (Context.Entity_Name);
   end Destroy;

   ------------------------------
   -- Set_Activity_Information --
   ------------------------------

   procedure Set_Activity_Information
     (Context : access Activity_Context;
      Id      : String) is
   begin
      Context.Id := new String'(Id);
   end Set_Activity_Information;

   --------------------------
   -- Activity_Information --
   --------------------------

   function Activity_Information
     (Context : access Activity_Context) return String is
   begin
      return Context.Id.all;
   end Activity_Information;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Activity_Context) is
   begin
      Destroy (File_Selection_Context (Context));
      Free (Context.Id);
   end Destroy;

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
   begin
      Register_Filter (Kernel, File_Filter, "File");
      Register_Filter (Kernel, Directory_Filter, "Directory");
      Register_Filter (Kernel, Entity_Filter, "Entity");
      Register_Filter (Kernel, Project_Only_Filter, "Project only");
      Register_Filter (Kernel, Project_File_Filter, "Project and file");
   end Register_Default_Filters;

end GPS.Kernel.Contexts;
