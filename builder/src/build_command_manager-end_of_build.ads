------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

--  Declare parser to complete each target build.

with Commands;                         use Commands;
with GPS.Tools_Output;                 use GPS.Tools_Output;

package Build_Command_Manager.End_Of_Build is

   type Parser is new Tools_Output_Parser with private;

   overriding procedure End_Of_Stream
     (Self    : not null access Parser;
      Status  : Integer;
      Command : access Root_Command'Class);

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with private;

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;

   procedure Set
     (Self    : access Output_Parser_Fabric;
      Builder : Builder_Context);
   --  ??? Missing comment

private

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with record
      Builder : Builder_Context;
   end record;

   type Parser is new Tools_Output_Parser with record
      Builder    : Builder_Context;
      Build      : Build_Information;
      Force_File : Virtual_File;
   end record;

end Build_Command_Manager.End_Of_Build;
