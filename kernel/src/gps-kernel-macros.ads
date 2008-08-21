-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2005-2008, AdaCore                --
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

--  This package provides handling of the expansion of macros in the
--  customization file. These are the %f, %e,... special strings that are
--  substituted dynamically based on the current context.

with Remote; use Remote;

package GPS.Kernel.Macros is

   Special_Character : constant Character := '%';
   --  The special character that is found before the macros

   type Macro_Filter_Record is
     new GPS.Kernel.Action_Filter_Record with private;
   type Macro_Filter is access Macro_Filter_Record'Class;

   function Create_Filter
     (Command : String;
      Filter  : Macro_Filter := null) return Macro_Filter;
   --  Create a new filter, by checking in Command whether there is a %f, %e...
   --  If Filter is not null, it is returned after being modified to take into
   --  account the new macro expansion in Command.
   --  null is never returned

   function Substitute
     (Param   : String;
      Context : GPS.Kernel.Selection_Context;
      Quoted  : Boolean;
      Done    : access Boolean;
      Server  : Server_Type := GPS_Server)
      return String;
   --  Return the replacement suitable for %Param.
   --  This should mostly be used from String_Utils.Substitute.
   --  The empty string "" is returned if Param is not one of the macro
   --  parameters, and Done.all set to False.
   --  If Param is recognized and handled, Done.all is set to True.
   --  It is assumed that Context contains enough information for this
   --  substitution, and this can be checked with Macro_Filter above.
   --  Substrings that start with '%' but are not one of the macros are left
   --  as is.
   --  If Server is not GPS_Server, then all paths will be translated into the
   --  server's file system.
   --  Invalid_Substitution might be raised if the context is still invalid,
   --  although this isn't guaranteed in general and you must check with
   --  Macro_Filter first.

private
   type Requirements is record
      File        : Boolean := False;
      Directory   : Boolean := False;
      Entity      : Boolean := False;
      Line        : Boolean := False;
      Column      : Boolean := False;
      Category    : Boolean := False;
      Importing   : Boolean := False;
      Single_Line : Boolean := False;
      Project     : Character := ' ';
      --  'p' for current project, 'P' for root project
   end record;
   pragma Pack (Requirements);

   type Macro_Filter_Record is new GPS.Kernel.Action_Filter_Record with record
      Requires : Requirements;
   end record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Macro_Filter_Record;
      Context : Selection_Context) return Boolean;
   --  See doc for inherited subprogram.

end GPS.Kernel.Macros;
