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

--  backtrace operations that are independant from the backend, so can be
--  called from the tools using the backtraces

--  See bt-xml.ads for a description of the XML schema.

package BT.Xml.Reader is

   function Get_Srcpos_Vn_Values
     (File_Name : String;
      Srcpos    : Source_Position) return Vn_Values_Seqs.Vector;
   --  Given a source position, find all the available vn <-> value_sets pairs
   --  Note that a value_set is actually just a string representing the values

   function Get_Srcpos_Vn_Values
     (File_Name : String;
      Line      : Line_Number) return Vn_Values_Seqs.Vector;
   --  Given a line number, find all the available vn <-> value_sets pairs

   procedure Clear;
   --  Clears all cached data

   procedure Initialize (Output_Directory : String);
   --  Initialize module to load data from given output directory.

end BT.Xml.Reader;
