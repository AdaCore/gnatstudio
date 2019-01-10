------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;           use GNAT.Regpat;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with Codefix.Formal_Errors; use Codefix.Formal_Errors;

with Projects;              use Projects;

package Codefix.Error_Lists is

   type Error_Message_List is private;

   procedure Initialize (This : out Error_Message_List);
   --  Initialize the error message list - this has to be called before any of
   --  the primitives on this type.

   procedure Free (This : in out Error_Message_List);
   --  Free the memory associated to an error message list.

   procedure Add_Errors_From
     (List     : Error_Message_List;
      Registry : Project_Registry_Access;
      Messages : String);
   --  Parse and add all the errors that can be found from the messages to the
   --  list.

   procedure Add_Error
     (List    : Error_Message_List;
      File    : Virtual_File;
      Line    : Integer;
      Column  : Visible_Column_Type;
      Message : Unbounded_String;
      Order   : Long_Long_Integer);
   --  Add the given error to the list. Order is an optional field that may be
   --  set to force an order between messages at a similar location.

   procedure Clear_Messages (List : Error_Message_List);
   --  Remove all the messages from the list.

   procedure Clear_Messages_At_Location
     (List   : Error_Message_List;
      File   : Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type);
   --  Remove all the messages from the list that are referenced on the given
   --  location.

   type Error_Message_Iterator is private;

   function First (List : Error_Message_List) return Error_Message_Iterator;
   --  Return the first message of the list.

   function First_At_Location
     (List   : Error_Message_List;
      File   : Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type;
      Order  : Long_Long_Integer)
      return Error_Message_Iterator;
   --  Return an iterator starting on the first message matching the location
   --  given in parameter - and won't go beyond that location. The retreived
   --  message is the one starting at the given order

   function Next (It : Error_Message_Iterator) return Error_Message_Iterator;
   --  Move the iterator to the next error message.

   function Get_Message (It : Error_Message_Iterator) return Error_Message;
   --  Return the message currently pointed by this iterator.

   procedure Cancel_Message (It : Error_Message_Iterator);
   --  Cancel the message pointed by the iterator. Further iterations over the
   --  message list will jump over this message

   function At_End (It : Error_Message_Iterator) return Boolean;
   --  Return true if there is no more messages to process.

   procedure Set_Regexp
     (This                    : in out Error_Message_List;
      File_Location_Regexp    : GNAT.Regpat.Pattern_Matcher;
      File_Index_In_Regexp    : Integer;
      Line_Index_In_Regexp    : Integer;
      Col_Index_In_Regexp     : Integer;
      Msg_Index_In_Regexp     : Integer;
      Style_Index_In_Regexp   : Integer;
      Warning_Index_In_Regexp : Integer);
   --  Set the regular expression used to parse error messages as in
   --  Add_Errors_From

private

   function Lt (Left, Right : Error_Message) return Boolean;

   package Internal_Message_List_Pckg is new Ada.Containers.Ordered_Sets
     (Error_Message, "<" => Lt);

   use Internal_Message_List_Pckg;

   type Messages_Loc is record
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type;
   end record;

   function "<" (Left, Right : Messages_Loc) return Boolean;

   type Internal_List_Access is access all Internal_Message_List_Pckg.Set;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Message_List_Pckg.Set, Internal_List_Access);

   package Error_Message_Container is new Ada.Containers.Ordered_Maps
     (Messages_Loc, Internal_List_Access);

   use Error_Message_Container;

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Access);

   type Error_Message_List_Record is record
      Messages : Error_Message_Container.Map;

      File_Regexp : Pattern_Matcher_Access;
      File_Index, Line_Index, Col_Index, Msg_Index : Integer;
      Style_Index, Warning_Index : Integer;
   end record;

   type Error_Message_List is access all Error_Message_List_Record;

   type Error_Message_Iterator is record
      Map_Cur      : Error_Message_Container.Cursor;
      List_Cur     : Internal_Message_List_Pckg.Cursor;
      Message_List : Internal_List_Access;
   end record;

end Codefix.Error_Lists;
