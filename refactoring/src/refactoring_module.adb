-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2010, AdaCore             --
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

with Default_Preferences;     use Default_Preferences;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Intl;                use GPS.Intl;
with Refactoring.Rename;      use Refactoring.Rename;
with Refactoring.Parameters;  use Refactoring.Parameters;
with Refactoring.Subprograms; use Refactoring.Subprograms;

package body Refactoring_Module is

   type Refactoring_Module_Record is new Module_ID_Record with null record;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Refactoring_Module_Id : Module_ID;
   begin
      Refactoring_Module_Id := new Refactoring_Module_Record;
      Register_Module (Refactoring_Module_Id, Kernel, "refactoring");
      Refactoring.Rename.Register_Refactoring (Kernel);
      Refactoring.Parameters.Register_Refactoring (Kernel);
      Refactoring.Subprograms.Register_Refactoring (Kernel);

      Add_Subprogram_Box := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-Subprogram-Box",
         Default => True,
         Doc     => -(
           "This preference forces GPS to add a comment before bodies when it"
           & " creates new subprograms. This comment is a three line comment"
           & " box, containing the name of the subprogram, as in" & ASCII.LF
           & "----------------" & ASCII.LF
           & "-- Subprogram --" & ASCII.LF
           & "----------------" & ASCII.LF),
         Label   => -"Subprogram Box",
         Page    => -"Refactoring");

      Add_In_Keyword := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-In-Keyword",
         Default => False,
         Doc     => -(
           "Whether the keyword ""in"" should be added when creating new"
           & " subprograms, as in" & ASCII.LF
           & "    procedure Proc (A : in Integer);" & ASCII.LF
           & " as opposed to" & ASCII.LF
           & "    procedure Proc (A : Integer);"),
         Label   => -"Add ""in"" Keyword",
         Page    => -"Refactoring");

      Create_Subprogram_Decl  := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "Refactoring-Subprogram-Spec",
         Default => True,
         Doc     => -(
           "Whether GPS should create a declaration for the subprogram. If"
           & " set to False, only the body of the subprogram will be created"),
         Label   => -"Create Subprogram Declarations",
         Page    => -"Refactoring");

   end Register_Module;

end Refactoring_Module;
