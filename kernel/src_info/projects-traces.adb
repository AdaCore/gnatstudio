-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Prj.PP;   use Prj.PP;
with Projects.Registry; use Projects, Projects.Registry;
with Traces;   use Traces;

package body Projects.Traces is

   ------------------------
   -- Trace_Pretty_Print --
   ------------------------

   procedure Trace_Pretty_Print
     (Handle  : Debug_Handle;
      Project : Project_Type)
   is
      Buffer : String (1 .. 10000);
      Buffer_Index : Natural := Buffer'First - 1;

      procedure Write_Char (C : Character);
      procedure Write_Str  (S : String);
      procedure Write_Eol;
      --  Required functions to instanciate Pretty_Print

      ----------------
      -- Write_Char --
      ----------------

      procedure Write_Char (C : Character) is
      begin
         Buffer_Index := Buffer_Index + 1;
         Buffer (Buffer_Index) := C;
      end Write_Char;

      ---------------
      -- Write_Str --
      ---------------

      procedure Write_Str  (S : String) is
      begin
         Buffer (Buffer_Index + 1 .. Buffer_Index + S'Length) := S;
         Buffer_Index := Buffer_Index + S'Length;
      end Write_Str;

      ---------------
      -- Write_Eol --
      ---------------

      procedure Write_Eol is
      begin
         Buffer_Index := Buffer_Index + 1;
         Buffer (Buffer_Index) := ASCII.LF;
      end Write_Eol;

   begin
      if Active (Handle) then
         Pretty_Print
           (Project                          => Project,
            Increment                        => 3,
            Eliminate_Empty_Case_Constructions => False,
            Minimize_Empty_Lines             => True,
            W_Char                           => Write_Char'Unrestricted_Access,
            W_Eol                            => Write_Eol'Unrestricted_Access,
            W_Str                            => Write_Str'Unrestricted_Access);
         Trace (Handle, Buffer (Buffer'First .. Buffer_Index));
      end if;

   exception
      --  In case of buffer overflow, do not crash
      when Constraint_Error =>
         null;
   end Trace_Pretty_Print;

end Projects.Traces;
