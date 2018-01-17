------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

--  This module provides various types and subprograms to integrate various
--  external scripting languages in GPS.

with Basic_Types;
with GNATCOLL.Arg_Lists;     use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Projects;
with GPS.Debuggers;          use GPS.Debuggers;
with GPS.Scripts;            use GPS.Scripts;
with GPS.Scripts.Entities;
with GPS.Scripts.Files;
with GPS.Scripts.File_Locations;
with Language;               use Language;

package GPS.Kernel.Scripts is

   GUI_Class_Name : constant String := "GUI";
   --  Name for the GPS.GUI class

   GPS_Shell_Name      : constant String := "Shell";
   --  Name for the GPS.Shell class

   Console_Class_Name : constant String := "Console";
   --  Name for the GPS.Console class

   function Get_Kernel (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return GPS.Kernel.Kernel_Handle;
   function Get_Kernel
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class)
      return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated with Data

   procedure Register_Default_Script_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Add the standard script commands.
   --  This subprogram should be called only after all scripting languages
   --  have been registered.

   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      CL      : Arg_List);
   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      CL      : Arg_List) return String;
   --  Execute the command in the GPS shell.
   --  This is only intended as a simpler form of
   --     Execute_Command
   --       (Lookup_Scripting_Language (Kernel, GPS_Shell_Name), CL)

   procedure Register_Command
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command       : String;
      Minimum_Args  : Natural    := 0;
      Maximum_Args  : Natural    := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   function New_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Base   : Class_Type := No_Class) return Class_Type;
   function Get
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      List   : Callback_Data_List;
      Script : access Scripting_Language_Record'Class)
      return Callback_Data_Access;
   procedure Set
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      List   : in out Callback_Data_List;
      Script : access Scripting_Language_Record'Class;
      Data   : Callback_Data_Access);
   --  Just a rewrite for the same subprogram applied to Get_Scripts (Kernel)

   ------------------
   -- Entity_Class --
   ------------------

   --  The following services are provided for use in the context of GPS. They
   --  provide access to various predefined classes shared between multiple
   --  modules.

   function Get_Entity_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type renames GPS.Scripts.Entities.Get_Entity_Class;
   --  Return the class to use for entities. This encapsulates an
   --  Entity_Information.

   procedure Set_Data
     (Instance : Class_Instance; Entity : Xref.Root_Entity'Class)
      renames GPS.Scripts.Entities.Set_Data;
   function Get_Data
     (Data : Callback_Data'Class; N : Positive)
      return Xref.Root_Entity'Class renames GPS.Scripts.Entities.Get_Data;
   --  The Entity class stores some Entity_Information data in Instance
   --  You should destroy the entity passed to Set_Data, but not the value
   --  returned by Get_Data

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Xref.Root_Entity'Class)
      return Class_Instance renames GPS.Scripts.Entities.Create_Entity;
   --  Return a new entity. Entity parameter should be freed by the caller

   ----------------
   -- File_Class --
   ----------------

   function Get_File_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type renames GPS.Scripts.Files.Get_File_Class;
   --  Return the class to use for file types. This encapsulates a File_Info.
   --  This is more efficient than calling directly
   --  GPS.Kernel.Scripts.New_Class particularly when a File class has already
   --  been created.

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive)
      return GNATCOLL.VFS.Virtual_File renames GPS.Scripts.Files.Nth_Arg;
   procedure Set_Nth_Arg
     (Data : in out Callback_Data'Class;
      N    : Positive;
      File : GNATCOLL.VFS.Virtual_File) renames GPS.Scripts.Files.Set_Nth_Arg;
   function Get_Data
     (Instance : Class_Instance)
      return GNATCOLL.VFS.Virtual_File renames GPS.Scripts.Files.Get_Data;
   --  Retrieve the file information from an instance. This returns No_File
   --  if no instance is passed

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
      return Class_Instance renames GPS.Scripts.Files.Create_File;
   --  Return a new file

   ---------------
   -- GUI_Class --
   ---------------

   function Get_GUI_Class
     (Kernel : access Kernel_Handle_Record'Class) return Class_Type;
   --  Return the class to use for GUI elements. This encapsulate a Gtk_Widget

   --------------
   -- Debugger --
   --------------

   function Get_Or_Create_Instance
     (Script  : access Scripting_Language_Record'Class;
      Process : access Base_Visual_Debugger'Class) return Class_Instance;
   --  Get or create an existing instance associated with Process

   -------------------------
   -- File_Location_Class --
   -------------------------

   function Get_File_Location_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type
      renames GPS.Scripts.File_Locations.Get_File_Location_Class;
   --  Return the class used to represent locations in files. This encapsulates
   --  a File_Location_Info

   type File_Location_Info is
     new GPS.Scripts.File_Locations.File_Location_Info;
   No_File_Location : constant File_Location_Info;

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type) return Class_Instance
      renames GPS.Scripts.File_Locations.Create_File_Location;
   --  Return a new file.
   --  File mustn't be destroyed after this call.

   -------------------
   -- Project_Class --
   -------------------

   function Get_Project_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type;
   --  Return the class to use for projects. This encapsulates a Project_Type

   function Get_Data
     (Data : Callback_Data'Class; N : Positive)
      return GNATCOLL.Projects.Project_Type
      renames GNATCOLL.Scripts.Projects.Get_Data;
   --  Retrieve some project information in Instance

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : GNATCOLL.Projects.Project_Type)
      return Class_Instance
        renames GNATCOLL.Scripts.Projects.Create_Project;
   --  Return a new project

   -------------------
   -- Context_Class --
   -------------------

   function Create_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : GPS.Kernel.Selection_Context) return Class_Instance;
   --  Return an instance of one of the classes derived from
   --  Context_Class, depending on the type of Context.
   --  If Context is already associated with a Class_Instance, the same
   --  instance is returned.

   function Get_Context (Inst : Class_Instance) return Selection_Context;
   --  Return the context stored in the instance

   -------------------
   -- Language_Info --
   -------------------

   function Create_Language_Info
     (Script  : not null access Scripting_Language_Record'Class;
      Lang    : access Language_Root'Class) return Class_Instance;
   --  Wraps a Language in a python class

private
   No_File_Location : constant File_Location_Info :=
     File_Location_Info (GPS.Scripts.File_Locations.No_File_Location);

end GPS.Kernel.Scripts;
