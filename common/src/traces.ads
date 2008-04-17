-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007, AdaCore             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  See documentation in GNATCOLL.Traces.
--  This package is provided for backward compatibility only, and only exports
--  the subprograms that were used historically by GPS, to limit the number
--  of changes.

with Ada.Exceptions;
with GNAT.Source_Info;
with GNATCOLL.Traces;

package Traces is

   subtype Debug_Handle is GNATCOLL.Traces.Trace_Handle;

   function Create
     (Unit_Name : String;
      Default   : GNATCOLL.Traces.Default_Activation_Status :=
        GNATCOLL.Traces.From_Config;
      Stream    : String := "";
      Factory   : GNATCOLL.Traces.Handle_Factory := null;
      Finalize  : Boolean := True)
      return Debug_Handle
     renames GNATCOLL.Traces.Create;

   procedure Trace
     (Handle : Debug_Handle;
      E      : Ada.Exceptions.Exception_Occurrence;
      Msg    : String := "Unexpected exception: ")
     renames GNATCOLL.Traces.Trace;

   procedure Trace
     (Handle   : Debug_Handle;
      Message  : String;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity)
     renames GNATCOLL.Traces.Trace;

   procedure Assert
     (Handle             : Debug_Handle;
      Condition          : Boolean;
      Error_Message      : String;
      Message_If_Success : String := "";
      Raise_Exception    : Boolean := True;
      Location           : String := GNAT.Source_Info.Source_Location;
      Entity             : String := GNAT.Source_Info.Enclosing_Entity)
     renames GNATCOLL.Traces.Assert;

   function Active (Handle : Debug_Handle) return Boolean
     renames GNATCOLL.Traces.Active;

   Exception_Handle : constant Debug_Handle :=
     Create ("UNEXPECTED_EXCEPTION", Default => GNATCOLL.Traces.On);
   Testsuite_Handle : constant Debug_Handle :=
     Create ("TESTSUITE", Default => GNATCOLL.Traces.Off);

end Traces;
