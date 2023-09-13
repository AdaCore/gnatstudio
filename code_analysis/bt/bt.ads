------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2023, AdaCore                     --
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
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

--  Root of hierarchy for the Backtrace database
--  IMPORTANT Note: this hierarchy is intended to be used outside CodePeer,
--  so please do not add any dependency other than standard (Ada or GNAT) ones,
--  in particular Utils.* should not be used.

--  The master of this file is located in the codepeer repository, under bt/
--  so always start by modifying the master.
--  Any change to bt/*.ad? need to be mirrored in the GNATStudio repository,
--  under code_analysis/src

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package BT is

   subtype Line_Number is Integer range 0 .. 2 ** 21 - 1;
   subtype Column_Number is Integer range 0 .. 2 ** 11 - 1;
   type Source_Position is record
      Line   : Line_Number;
      Column : Column_Number;
   end record;
   pragma Pack (Source_Position);
   --  Position in a source file.
   --  Line 0 represents an invalid/no location.

   No_Source_Position : constant Source_Position := (0, 0);

   type Vn_Values is record
      Vn_Image  : Unbounded_String;
      Set_Image : Unbounded_String;
   end record;

   type Vn_Values_Seq_Index is new Positive;
   package Vn_Values_Seqs is new Ada.Containers.Vectors
     (Element_Type => Vn_Values,
      Index_Type   => Vn_Values_Seq_Index);

end BT;
