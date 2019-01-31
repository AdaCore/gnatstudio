------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with Remote;                use Remote;

package Shared_Macros is

   function Shared_Macros_Substitute
     (Project_From_Kernel : Project_Type;
      Project_From_Param  : Project_Type;
      File_Information    : Virtual_File;
      Param               : String;
      Quoted              : Boolean;
      Done                : access Boolean;
      Server              : Server_Type := GPS_Server;
      For_Shell           : Boolean := False) return String;
   --  Return the replacement suitable for %Param.
   --  This should mostly be used from GPS.Kernel.Macros.Substitute and from
   --  Build_Command_Utils.Substitute function implementations.
   --  GPS calls uses GPS.Kernel.Macros.Substitute function.
   --  GNATbench calls uses Build_Command_Utils.Substitute function.
   --  The empty string "" is returned if Param is not one of the macro
   --  parameters, and Done.all set to False.
   --  If Param is recognized and handled, Done.all is set to True.
   --  Substrings that start with '%' but are not one of the macros are left
   --  as is.
   --  If Server is not GPS_Server, then all paths will be translated into the
   --  server's file system.
   --  Invalid_Substitution might be raised if the context is still invalid,
   --  although this isn't guaranteed in general and you must check with
   --  Macro_Filter first.
   --  For_Shell must be set to True when the result string is to be used by a
   --  shell command. In this case it is needed to escape backslashes.
   --  This function handles %f %F %fk %gnatmake %O %p* %P* macros.
   --  These macros are a superset of macros needed to expand argument in
   --  builder target command line expansion.
   --  It uses only parameters available from GNATbench.

   Doc : aliased constant String;
   --  Documents all supported macros from this package

private
   LF : constant Character := ASCII.LF;
   Doc : aliased constant String :=
        "Current File information" & LF
      & "%f      base name" & LF
      & "%fd     directory name" & LF
      & "%fk     krunched base name" & LF
      & "%F      absolute path" & LF

      & LF & "Project information" & LF
      & "%o      absolute path of object directory for current project" & LF
      & "%O      absolute path of object directory for root project" & LF
      & "%pps    switch -P with absolute path, if there is a project" & LF
      & "%PPs    switch -P with absolute path of root project" & LF
      & "%p      name of current project" & LF
      & "%P      name of current root project" & LF
      & "%Pl     lower-cased name of current project" & LF
      & "%Pb     base name for current project file" & LF
      & "%pp     absolute path for current project file" & LF
      & "%PP     absolute path for root project" & LF
      & "%prd    list of recursive absolute paths for source dirs" & LF
      & "%prf    list of recursive absolute paths for source files" & LF
      & "%prdf   tmp file with recursive absolute paths for source dirs" & LF
      & "%prsf   tmp file with recursive absolute paths for source files" & LF
      & "%pd     list of absolute paths for source dirs" & LF
      & "%pf     list of absolute paths for source files" & LF
      & "%pdf    tmp file with absolute paths for source dirs" & LF
      & "%psf    tmp file with absolute paths for source files";

end Shared_Macros;
