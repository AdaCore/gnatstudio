-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

separate (GVD.Proc_Utils)

package body Builtin is

   type Builtin_Record is null record;

   --------------------
   -- Is_Implemented --
   --------------------

   function Is_Implemented return Boolean is
   begin
      return False;
   end Is_Implemented;

   ---------------------
   -- Close_Processes --
   ---------------------

   procedure Close_Processes (Handle : in out Builtin_Handle) is
   begin
      Handle := null;
   end Close_Processes;

   ------------------
   -- Next_Process --
   ------------------

   procedure Next_Process
     (Handle  : Builtin_Handle;
      Info    : out Process_Info;
      Success : out Boolean) is
   begin
      Success := False;
      Info :=
        (Id_Len   => 0,
         Info_Len => 0,
         Id       => "",
         Info     => "");
   end Next_Process;

   --------------------
   -- Open_Processes --
   --------------------

   procedure Open_Processes (Handle : out Builtin_Handle) is
   begin
      Handle := null;
   end Open_Processes;

end Builtin;
