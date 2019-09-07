------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2013-2019, AdaCore                   --
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

with GNAT.Strings;
with GNATCOLL.JSON;
with Language;

private package GNATdoc.Backend.HTML.Source_Code is

   type Source_Code_Printer
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      is tagged limited private;

   not overriding procedure Start_File
     (Self         : in out Source_Code_Printer;
      File         : GNATCOLL.VFS.Virtual_File;
      Buffer       : not null GNAT.Strings.String_Access;
      First_Line   : Positive;
      Show_Private : Boolean;
      Continue     : in out Boolean);
   --  Called on start of processing source code file. When subprograms sets
   --  Continue to False processing is terminated and End_File doesn't
   --  executed.

   not overriding procedure End_File
     (Self     : in out Source_Code_Printer;
      Result   : out GNATCOLL.JSON.JSON_Value;
      Continue : in out Boolean);
   --  Called after processing of source code file independently it was
   --  terminated or file was processed completely. It is not called only when
   --  Start_File terminates processing.

   not overriding procedure Normal_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Identifier_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Partial_Identifier_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Block_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Type_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Number_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Aspect_Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Annotated_Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Annotated_Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Aspect_Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Aspect_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Character_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure String_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

   not overriding procedure Operator_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean);

private

   type Scope_Info is record
      Entity        : GNATdoc.Atree.Entity_Id;
      --  Entity of scope
      Private_First : Natural;
      Private_Last  : Natural;
      --  Range of lines of private part of scope
   end record;

   package Scope_Vectors is new Ada.Containers.Vectors (Positive, Scope_Info);

   type Source_Code_Printer
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is tagged limited record
      File          : GNATCOLL.VFS.Virtual_File;
      Buffer        : GNAT.Strings.String_Access;
      Result        : GNATCOLL.JSON.JSON_Array;
      Line          : GNATCOLL.JSON.JSON_Array;
      Current_Line  : Positive;

      Scope_Stack   : Scope_Vectors.Vector;
      Current_Scope : Scope_Info;
      Show_Private  : Boolean;
   end record;

end GNATdoc.Backend.HTML.Source_Code;
