------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

private with Shared_Macros;
with Remote; use Remote;

package GPS.Kernel.Macros is

   Special_Character : constant Character := '%';
   --  The special character that is found before the macros

   type Macro_Filter_Record is
     new GPS.Kernel.Action_Filter_Record with private;
   type Macro_Filter is access all Macro_Filter_Record'Class;

   function Create_Filter
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Command : String;
      Filter  : Macro_Filter := null) return Macro_Filter;
   --  Create a new filter, by checking in Command whether there is a %f, %e...
   --  If Filter is not null, it is returned after being modified to take into
   --  account the new macro expansion in Command.
   --  This function returns null if no requirement is necessary.

   function Substitute
     (Param     : String;
      Context   : GPS.Kernel.Selection_Context;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String;
   --  Return the replacement suitable for %Param.
   --  This should mostly be used from GNATCOLL.Templates.Substitute.
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

   Doc : constant String;
   --  Documents all supported macros from this package

private
   LF  : constant Character := ASCII.LF;
   Doc : constant String :=
      Shared_Macros.Doc & LF
      & LF & "Current Context Information" & LF
      & "%d       directory (of current file,...)" & LF
      & "%dk      krunched directory" & LF
      & "%e       entity name" & LF
      & "%ef      entity name, with indicator if xref is not up-to-date" & LF
      & "%s       entity name (if available) or current selection" & LF
      & "%S       entity name, current selection or current expression" & LF
      & "%l       line number" & LF
      & "%c       column number" & LF
      & "%a       category of current message (in Locations window)" & LF
      & "%i       name of importing project (in Project view)" & LF
      & "%ts      short title for the current window" & LF
      & "%tl      long title for the current window" & LF

      & LF & "System Information" & LF
      & "%rbl     name of the remote build host (or 'localhost')" & LF
      & "%GPS     user's directory to store GPS settings" & LF
      & "%system_bin_dir GPS install prefix" & LF
      & "%gnat    name of the GNAT driver to use" & LF
      & "%target  switch --target= to pass to various tools" & LF
      & "%(env:X) value of an enviroment variable with name X";

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
   overriding function Get_Debug_Name
     (Filter  : access Macro_Filter_Record) return String;
   --  See doc for inherited subprogram

end GPS.Kernel.Macros;
