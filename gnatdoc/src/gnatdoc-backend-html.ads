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

--  This package implements a HTML backend of Docgen.

private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Ordered_Sets;

with GNATCOLL.JSON;

with GNATdoc.Atree;        use GNATdoc.Atree;
with GNATdoc.Backend.Base; use GNATdoc.Backend.Base;

private package GNATdoc.Backend.HTML is

   type HTML_Backend is new GNATdoc.Backend.Base.Base_Backend with private;

   overriding procedure Initialize
     (Self    : in out HTML_Backend;
      Context : access constant Docgen_Context);
   --  Initialize the backend and create the destination directory with support
   --  files. Returns the backend structure used to collect information of all
   --  the processed files (used to generate the global indexes).

   overriding procedure Finalize
     (Self                : in out HTML_Backend;
      Update_Global_Index : Boolean);
   --  If Update_Global_Index is true then update the global indexes.

   overriding procedure Generate_Lang_Documentation
     (Self        : in out HTML_Backend;
      Tree        : access Tree_Type;
      Entity      : Entity_Id;
      Entities    : Collected_Entities;
      Scope_Level : Natural);
   --  Generate the documentation of a single <lang> file.

private

   function Less
     (Left  : GNATCOLL.JSON.JSON_Value;
      Right : GNATCOLL.JSON.JSON_Value) return Boolean;
   --  Compares values of "label" attributes of two objects,

   package Docs_Sets is
     new Ada.Containers.Ordered_Sets
       (GNATCOLL.JSON.JSON_Value, Less, GNATCOLL.JSON."=");

   type Docs_Group is record
      Name      : Unbounded_String;
      Doc_Files : Docs_Sets.Set;
   end record;

   type Docs_Group_Access is access all Docs_Group;

   package Docs_Maps is
     new Ada.Containers.Ordered_Maps
       (Unbounded_String, Docs_Group_Access);

   type HTML_Backend is new GNATdoc.Backend.Base.Base_Backend with record
      Doc_Groups : Docs_Maps.Map;
      Doc_Files  : Docs_Sets.Set;
      --  Set of generated documentation files.
   end record;

   overriding function Name (Self : HTML_Backend) return String;
   --  Returns name of the backend.

   function Get_Docs_Href (Entity : Entity_Id) return String;
   --  Returns hyper reference to documentation of entity.

   procedure Set_Label_And_Href
     (Object    : GNATCOLL.JSON.JSON_Value;
      Entity    : Entity_Id;
      Full_Name : Boolean := False);
   --  Set the "label" field of Object from the Short_Name or Full_Name of
   --  the Entity. If Entity contain hyper reference information, set the
   --  "href" field of object to it.

end GNATdoc.Backend.HTML;
