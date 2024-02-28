------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Containers.Vectors;
with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with VSS.Strings;      use VSS.Strings;

with GPS.Kernel;       use GPS.Kernel;
with DAP.Clients;      use DAP.Clients;
with DAP.Tools;        use DAP.Tools;

with DAP.Clients.Variables;

package DAP.Modules.Scripts is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   type Variable_Data is record
      Full_Name : VSS.Strings.Virtual_String;
      Data      : DAP.Tools.Variable;
   end record;

   package Variable_Data_Vector is
     new Ada.Containers.Vectors (Positive, Variable_Data);

   procedure Create_Debugger_Variable_For_Callback
     (Callback : GNATCOLL.Scripts.Subprogram_Type;
      Client   : not null access DAP.Clients.DAP_Client'Class;
      Data     : Variable_Data);
   --  Creates an instance of the DebuggerVariable python class,
   --  fills it with Data and pass it to the callback.

   procedure Create_Debugger_Variables_For_Callback
     (Callback : GNATCOLL.Scripts.Subprogram_Type;
      Client   : not null access DAP.Clients.DAP_Client'Class;
      Data     : Variable_Data_Vector.Vector);
   --  Creates an array of instances of the DebuggerVariable python class,
   --  fills it with Data and pass it to the callback. Used to pass children
   --  to python callback.

   procedure Create_No_Debugger_Variable_For_Callback
     (Params : DAP.Clients.Variables.Request_Parameters);
   --  Calls callback with No_Class_Instance parameter when variable
   --  is not found.

end DAP.Modules.Scripts;
