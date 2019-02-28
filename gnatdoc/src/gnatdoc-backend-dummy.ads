------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                         Copyright (C) 2017-2019, AdaCore                 --
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

--  This package implements a dummy backend that don't generate any
--  documentation, but generates internal cm/pr files.

with GNATdoc.Atree;        use GNATdoc.Atree;
with GNATdoc.Backend.Base; use GNATdoc.Backend.Base;

private package GNATdoc.Backend.Dummy is

   type Dummy_Backend is new GNATdoc.Backend.Base.Base_Backend with private;

private

   type Dummy_Backend is
     new GNATdoc.Backend.Base.Base_Backend with null record;

   overriding procedure Finalize
     (Self                : in out Dummy_Backend;
      Update_Global_Index : Boolean) is null;

   overriding procedure Generate_Lang_Documentation
     (Self        : in out Dummy_Backend;
      Tree        : access Tree_Type;
      Entity      : Entity_Id;
      Entities    : Collected_Entities;
      Scope_Level : Natural) is null;

   overriding function Name (Self : Dummy_Backend) return String is ("dummy");
   --  Returns name of the backend.

end GNATdoc.Backend.Dummy;
