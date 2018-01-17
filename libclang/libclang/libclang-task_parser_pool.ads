------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;
with Libclang.Index; use Libclang.Index;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with GNATCOLL.Utils; use GNATCOLL.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Hashed_Maps;

package Libclang.Task_Parser_Pool is

   type Parse_Callback is abstract tagged null record;
   procedure Call
     (Self : access Parse_Callback;
      File : Virtual_File; TU : Clang_Translation_Unit) is abstract;
   type Parse_Callback_Access is access all Parse_Callback'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Parse_Callback'Class, Parse_Callback_Access);

   package Callbacks_Vectors is
     new Ada.Containers.Vectors (Positive, Parse_Callback_Access);

   type Parsing_Request_Priority is (Low, High);
   type Parsing_Request_Kind is (Parse, Reparse);

   generic
      type User_Data is private;
   package Pool is

      type Unbounded_String_Array_Access is access all Unbounded_String_Array;

      type Parsing_Request_Record (Kind : Parsing_Request_Kind := Parse)
      is record
         Prio         : Parsing_Request_Priority;
         Options      : Clang_Translation_Unit_Flags;
         Context      : User_Data;
         File_Name    : Unbounded_String;
         Project_Name : Unbounded_String;
         Callbacks    : Callbacks_Vectors.Vector;
         Indexer      : Clang_Index;
         case Kind is
         when Parse =>
            Switches      : Unbounded_String_Array_Access;
         when Reparse =>
            TU            : Clang_Translation_Unit;
            Unsaved_Files : Unsaved_File_Array_Access;
         end case;
      end record;
      type Parsing_Request is access all Parsing_Request_Record;
      procedure Free is new
        Ada.Unchecked_Deallocation (Parsing_Request_Record, Parsing_Request);

      procedure Destroy (Request : in out Parsing_Request);

      function Unique_Key (Request : Parsing_Request) return Unbounded_String;

      type Parsing_Response is record
         TU        : Clang_Translation_Unit;
         File_Name : Unbounded_String;
         Context   : User_Data;
         Callbacks : Callbacks_Vectors.Vector;
      end record;

      function Get_Priority
        (PR : Parsing_Request) return Parsing_Request_Priority
      is (PR.Prio);

      function Before (Left, Right : Parsing_Request_Priority) return Boolean
      is (Parsing_Request_Priority'Pos (Left)
          > Parsing_Request_Priority'Pos (Right));

      package Parsing_Request_Queues_Interface
      is new Ada.Containers.Synchronized_Queue_Interfaces (Parsing_Request);
      package Parsing_Request_Queues
      is new Ada.Containers.Unbounded_Priority_Queues
        (Parsing_Request_Queues_Interface, Parsing_Request_Priority);

      package Parsing_Response_Queues_Interface
      is new Ada.Containers.Synchronized_Queue_Interfaces (Parsing_Response);
      package Parsing_Response_Queues
      is new Ada.Containers.Unbounded_Synchronized_Queues
        (Parsing_Response_Queues_Interface);

      package Request_Maps is new Ada.Containers.Hashed_Maps
        (Unbounded_String, Parsing_Request, Hash, "=");

      protected type Parsing_Requests_Queue_Type is
         procedure Enqueue (Request : in out Parsing_Request);
         entry Dequeue (Request : out Parsing_Request);
         function Length return Natural;
      private
         Map   : Request_Maps.Map;
         Queue : Parsing_Request_Queues.Queue;
      end Parsing_Requests_Queue_Type;

      Parsing_Request_Queue  : Parsing_Requests_Queue_Type;
      Parsing_Response_Queue : Parsing_Response_Queues.Queue;

      ------------------
      -- Parsing_Task --
      ------------------

      task type Parsing_Task is
         entry Start;
         entry Stop;
         entry Finish;
      end Parsing_Task;

      type Parsing_Task_Array is array (Positive range <>) of Parsing_Task;
   end Pool;

end Libclang.Task_Parser_Pool;
