-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2008, AdaCore             --
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

--  This module provides various types and subprograms to integrate various
--  external scripting languages in GPS.

with Basic_Types;
with Entities;
with GNAT.OS_Lib;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with Projects;

package GPS.Kernel.Scripts is

   GUI_Class_Name : constant String := "GUI";
   --  Name for the GPS.GUI class

   GPS_Shell_Name      : constant String := "Shell";

   function Get_Scripts
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return GNATCOLL.Scripts.Scripts_Repository;
   --  Return the scripts repository

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

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String) return String;
   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String);
   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List);
   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Execute the command in the GPS shell.
   --  This is only intended as a simpler form of
   --     Execute_Command
   --       (Lookup_Scripting_Language (Kernel, GPS_Shell_Name), Command, Args)

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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for entities. This encapsulates an
   --  Entity_Information.

   procedure Set_Data
     (Instance : Class_Instance; Entity : Entities.Entity_Information);
   function Get_Data
     (Data : Callback_Data'Class; N : Positive)
      return Entities.Entity_Information;
   --  The Entity class stores some Entity_Information data in Instance
   --  You should destroy the entity passed to Set_Data, but not the value
   --  returned by Get_Data

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Entities.Entity_Information) return Class_Instance;
   --  Return a new entity. Entity parameter should be freed by the caller

   ----------------
   -- File_Class --
   ----------------

   function Get_File_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Class_Type;
   --  Return the class to use for file types. This encapsulates a File_Info.
   --  This is more efficient than calling directly
   --  GPS.Kernel.Scripts.New_Class particularly when a File class has already
   --  been created.

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return VFS.Virtual_File;
   function Get_Data
     (Instance : Class_Instance) return VFS.Virtual_File;
   --  Retrieve the file information from an instance

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : VFS.Virtual_File) return Class_Instance;
   --  Return a new file

   ---------------
   -- GUI_Class --
   ---------------

   function Get_GUI_Class
     (Kernel : access Kernel_Handle_Record'Class) return Class_Type;
   --  Return the class to use for GUI elements. This encapsulate a Gtk_Widget

   -------------------------
   -- File_Location_Class --
   -------------------------

   function Get_File_Location_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class used to represent locations in files. This encapsulates
   --  a File_Location_Info

   type File_Location_Info is private;
   No_File_Location : constant File_Location_Info;

   function Get_File (Location : File_Location_Info) return Class_Instance;
   function Get_Line (Location : File_Location_Info) return Integer;
   function Get_Column
     (Location : File_Location_Info) return Basic_Types.Visible_Column_Type;
   --  Return the information stored in the file location

   function Get_Data (Data : Callback_Data'Class; N : Positive)
      return File_Location_Info;
   --  Retrieve the file location information from an instance

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type) return Class_Instance;
   --  Return a new file.
   --  File mustn't be destroyed after this call.

   ----------------
   -- Hook_Class --
   ----------------

   function Get_Hook_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class used to provide an interface to hooks

   -------------------
   -- Project_Class --
   -------------------

   function Get_Project_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for projects. This encapsulates a Project_Type

   function Get_Data (Data : Callback_Data'Class; N : Positive)
      return Projects.Project_Type;
   --  Retrieve some project information in Instance

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : Projects.Project_Type) return Class_Instance;
   --  Return a new project

   -------------------
   -- Context_Class --
   -------------------

   function Get_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the base class for all context-related classes

   function Create_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : GPS.Kernel.Selection_Context) return Class_Instance;
   --  Return an instance of one of the classes derived from
   --  Context_Class, depending on the type of Context.
   --  If Context is already associated with a Class_Instance, the same
   --  instance is returned.

   function Get_Data (Data : Callback_Data'Class; N : Positive)
      return GPS.Kernel.Selection_Context;
   function Get_Data (Instance : Class_Instance)
      return GPS.Kernel.Selection_Context;
   --  Retrieve some context information from instance

   function Get_Area_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the base class for contexts containing file areas

   function Get_File_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return a class for a File_Selection_Context

   function Get_Entity_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return a class for an Entity_Selection_Context

private
   type File_Location_Info is record
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
   end record;
   No_File_Location : constant File_Location_Info := (No_Class_Instance, 0, 0);

end GPS.Kernel.Scripts;
