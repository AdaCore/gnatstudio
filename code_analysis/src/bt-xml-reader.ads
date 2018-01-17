------------------------------------------------------------------------------
--                              C O D E P E E R                             --
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
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

--  backtrace operations that are independant from the backend, so can be
--  called from the tools using the backtraces

--  See bt-xml.ads for a description of the XML schema.

package BT.Xml.Reader is

   procedure Read_File_Backtrace_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean);
   --  Read the backtrace-XML corresponding to this source_file name.
   --  Store the results in mappings used for queries

   procedure Read_File_Vals_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean);
   --  Read the vals-XML corresponding to this source_file name.
   --  Store the results in mappings used for queries

   procedure Get_Vn_Backtraces
     (Proc_Name  : String;
      Vn_Id      : Natural;
      Msg_Loc    : Source_Position;
      Backtraces : in out BT.BT_Info_Seqs.Vector);
   --  Returns a sequence of backtrace_info that contributed to this
   --  VN (associated with either precondition or error_message).

   function Get_Precondition_Callee_Name (Bt_Id : Natural) return String;
   --  Given a Precondition_Check backtrace, returns the callee name

   function Get_Precondition_VN (Bt_Id : Natural) return Natural;
   --  Given a Precondition_Check backtrace, returns the precondition_vn

   function Get_BT_File_Name (Bt_Id : Natural) return String;
   --  Returns the file_name in which the callee is declared
   --  (for preconditions).

   function Get_Callee_Srcpos (Bt_Id : Natural) return Source_Position;
   --  Returns the line associated with the callee (for preconditions).

   function Get_Callee_File_Name (Bt_Id : Natural) return String;
   --  Returns the file_name in which the callee is declared
   --  (for preconditions).

   function Get_Variable_Vn_Value
     (File          : String;
      Variable      : String;
      Srcpos        : Source_Position;
      Closest_Match : out Source_Position) return String;
   --  Returns the value_set associated with Variable on the closest
   --  source location to Srcpos available.

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
