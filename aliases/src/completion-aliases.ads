------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  This package offers a completion resolver for language keywords.

with Completion; use Completion;
with Aliases_Module; use Aliases_Module;

package Completion.Aliases is

   type Completion_Aliases is new Completion_Resolver with private;

   type Completion_Aliases_Access is access all Completion_Aliases'Class;

   overriding
   procedure Get_Completion_Root
     (Resolver   : access Completion_Aliases;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Completion_Aliases) return String;
   --  See inherited documentation

   overriding
   procedure Free (Resolver : in out Completion_Aliases) is null;
   --  See inherited documentation

private

   type Completion_Aliases is new Completion_Resolver with record
      Lang : Language_Access;
   end record;

   type Alias_Completion_Proposal is new Simple_Completion_Proposal
   with record
      Alias : Alias_Type;
   end record;

   overriding function Deep_Copy
     (Proposal : Alias_Completion_Proposal)
      return Completion_Proposal'Class;

   overriding function Get_Action_Name
     (Proposal : Alias_Completion_Proposal)
      return String;

   overriding function Get_Documentation
     (Proposal : Alias_Completion_Proposal)
      return String;

   overriding function Get_Custom_Icon_Name
     (Proposal : Alias_Completion_Proposal) return String;

   overriding function Get_Label
     (Proposal : Alias_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class) return String;

   overriding function To_Completion_Id
     (Proposal : Alias_Completion_Proposal;
      Dummy_Db : access Xref.General_Xref_Database_Record'Class)
   return Completion_Id is (Proposal.Name'Length,
                            "ALIAS   ",
                            Proposal.Name.all,
                            GNATCOLL.VFS.No_File, 0, 0);

end Completion.Aliases;
