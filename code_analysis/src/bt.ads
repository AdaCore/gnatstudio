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

--  Root of hierarchy for the Backtrace database
--  IMPORTANT Note: this hierarchy is intended to be used outside CodePeer,
--  so please do not add any dependency other than standard (Ada or GNAT) ones,
--  in particular Utils.* should not be used.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with Message_Kinds;

package BT is

   type Event_Enum is
     (Postcondition_Assume_Event, Induction_Var_Assume_Event,
      Other_From_Assume_Event, Non_Invalid_Input_Assume_Event,
      Precond_Assume_Event, Jump_Event, Check_Event, Precondition_Event);
   --  Events that may trigger a backtrace.

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

   type BT_Info is record
      Bt_Id   : Natural;
      Event   : Event_Enum;
      Kind    : Message_Kinds.BE_Message_Subkind;
      Text    : Unbounded_String;
      --  Text of event or message kind (for check events)
      Sloc    : Source_Position;
   end record;
   --  Simplified Backtrace record for information to display by external tools

   No_BT_Info : constant BT_Info :=
     (Bt_Id  => Natural'Last,
      Event  => Event_Enum'Last,
      Kind   => Message_Kinds.BE_Message_Subkind'Last,
      Text   => Null_Unbounded_String,
      Sloc   => (Line => 0, Column => 0));

   function EQ (B1, B2 : BT_Info) return Boolean;
      --  compare 2 backtrace records

   type BT_Seq_Index is new Positive;
   package BT_Info_Seqs is new Ada.Containers.Vectors
     (Element_Type => BT_Info,
      Index_Type   => BT_Seq_Index,
      "="          => EQ);

   type Src_Pos_Record is record
      File_Name : Unbounded_String;
      Sloc      : Source_Position;
   end record;

   type Src_Pos_Seq_Index is new Positive;
   package Src_Pos_Seqs is new Ada.Containers.Vectors
     (Element_Type => Src_Pos_Record,
      Index_Type   => Src_Pos_Seq_Index);

   type Vn_Values is record
      Vn_Image  : Unbounded_String;
      Set_Image : Unbounded_String;
   end record;

   type Vn_Values_Seq_Index is new Positive;
   package Vn_Values_Seqs is new Ada.Containers.Vectors
     (Element_Type => Vn_Values,
      Index_Type   => Vn_Values_Seq_Index);
   use Vn_Values_Seqs;

   function Same_Srcpos (S1, S2 : Src_Pos_Record) return Boolean;
   function Srcpos_Hash (S : Src_Pos_Record) return Ada.Containers.Hash_Type;

   package Srcpos_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Src_Pos_Record,
      Element_Type    => Vn_Values_Seqs.Vector,
      Hash            => Srcpos_Hash,
      Equivalent_Keys => Same_Srcpos);

   type Id_Seq_Index is new Positive;
   package Id_Seqs is new Ada.Containers.Vectors
     (Element_Type => Natural,
      Index_Type   => Id_Seq_Index);

   function Readable_Event_Kind (Event : Event_Enum) return String;
   --  Return a string to display when writing out backtraces

end BT;
