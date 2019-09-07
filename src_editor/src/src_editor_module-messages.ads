------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

--  This package implements the message listener which adds messages to the
--  side of editors, as well as implements line highlighting.

with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;

private with Ada.Containers.Hashed_Sets;

with GPS.Kernel.Messages; use GPS.Kernel.Messages;

package Src_Editor_Module.Messages is

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class);
   --  Creates and registers highlighting manager

   procedure Unregister (Kernel  : not null access Kernel_Handle_Record'Class);
   --  Unregister and destroys highlighting manager

private

   use Ada;
   use Ada.Strings.Unbounded;

   --  ??? Do we need all this? Why not create styles once and for
   --  all in the Style_Manager?

   function Hash
     (Item : Style_Access) return Containers.Hash_Type;
   --  Returns hash value constructed from style's name

   package Style_Sets is
     new Ada.Containers.Hashed_Sets (Style_Access, Hash, "=");

   type Style_Set_Access is access all Style_Sets.Set;

   type Key is record
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File;
   end record;

   function Hash (Item : Key) return Containers.Hash_Type;
   --  Returns has value constructed from the category's and file's name

   package Style_Maps is
     new Containers.Hashed_Maps (Key, Style_Set_Access, Hash, "=", "=");

   type Highlighting_Manager
     (Kernel : not null access Kernel_Handle_Record'Class)
     is new Abstract_Listener
   with record
      Map : Style_Maps.Map;
   end record;
   --  I826-008 workaround: we manage the set of all styles used per file per
   --  category to unhighlight all occurrences of what styles and avoid
   --  potencial glitches.
   type Highlighting_Manager_Access is access all Highlighting_Manager'Class;

   procedure File_Opened
     (Self : not null access Highlighting_Manager;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure File_Removed
     (Self     : not null access Highlighting_Manager;
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File);

   overriding procedure Message_Property_Changed
     (Self     : not null access Highlighting_Manager;
      Message  : not null access Abstract_Message'Class;
      Property : String);

   overriding procedure Message_Removed
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class);

   overriding procedure Message_Added
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class);

end Src_Editor_Module.Messages;
