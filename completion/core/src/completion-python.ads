------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2019, AdaCore                     --
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

--  This packages provides glue to implement completion resolvers in Python

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Completion;       use Completion;
with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package Completion.Python is

   type Generic_Completion_Manager is new Completion_Manager with null record;

   overriding function Get_Initial_Completion_List
     (Manager : access Generic_Completion_Manager;
      Context : Completion_Context)
      return Completion_List;
   --  See inherited documentation

   --------------
   -- Resolver --
   --------------

   --  This defines a completion resolver

   type Completion_Python is new Completion_Resolver with private;
   type Completion_Python_Access is access all Completion_Python'Class;

   function Create
     (Class : Class_Instance;
      Lang_Name : String) return Completion_Python_Access;
   --  Initialize a resolver for the given class

   overriding
   procedure Get_Completion_Root
     (Resolver   : access Completion_Python;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Completion_Python) return String;
   --  See inherited documentation

   overriding
   procedure Free (Resolver : in out Completion_Python) is null;
   --  See inherited documentation

private

   type Completion_Python is new Completion_Resolver with record
      Object    : Class_Instance;
      Id        : Positive;
      Lang_Name : Unbounded_String;
   end record;

   type Simple_Python_Completion_Proposal is new Simple_Completion_Proposal
   with record
      Label         : Unbounded_String;
      Documentation : Unbounded_String;
      Icon_Name     : Unbounded_String;
      Action_Name   : Unbounded_String;
   end record;

   overriding function Get_Action_Name
     (Proposal : Simple_Python_Completion_Proposal)
      return String;

   overriding function Get_Documentation
     (Proposal : Simple_Python_Completion_Proposal)
      return String;

   overriding function Get_Custom_Icon_Name
     (Proposal : Simple_Python_Completion_Proposal) return String;

   overriding function Get_Label
     (Proposal : Simple_Python_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class) return String;

   overriding function To_Completion_Id
     (Proposal : Simple_Python_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id;

   overriding function Deep_Copy
     (Proposal : Simple_Python_Completion_Proposal)
      return Completion_Proposal'Class;

   No_Proposal : constant Simple_Python_Completion_Proposal :=
     (null, null, Cat_Unknown,
      Null_Unbounded_String, Null_Unbounded_String,
      Null_Unbounded_String, Null_Unbounded_String);

end Completion.Python;
