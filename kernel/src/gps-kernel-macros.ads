------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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
     (Param     : String;
      Context   : GPS.Kernel.Selection_Context;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String;
   --  Return the replacement suitable for %Param.
   --  This should mostly be used from String_Utils.Substitute.
   --  If Param is not one of the macro parameters, set Done.all to False and
   --  return an empty string.
   --  If Param is recognized and handled, Done.all is set to True. Note that
   --  an empty string can be a valid substitution result if Done.all is set
   --  to True.
   --  It is assumed that Context contains enough information for this
   --  substitution, and this can be checked with Macro_Filter above.
   --  Substrings that start with '%' but are not one of the macros are left
   --  as is.
   --  If Server is not GPS_Server, then all paths will be translated into the
   --  server's file system.
   --  Invalid_Substitution might be raised if the context is still invalid,
   --  although this isn't guaranteed in general and you must check with
   --  Macro_Filter first.
   --  For_Shell must be set to True when the result string is to be used by a
   --  shell command. In this case it is needed to escape backslashes.

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
   --  See doc for inherited subprogram

end GPS.Kernel.Macros;
