------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with GNATCOLL.Traces; use GNATCOLL.Traces;
with Ada.Task_Identification; use Ada.Task_Identification;

package body Libclang.Task_Parser_Pool is

   Me : constant Trace_Handle := GNATCOLL.Traces.Create ("GPS.LIBCLANG.POOL");

   function "+"
     (S : Unbounded_String) return String renames To_String;
   --  Utility conversion operators from String to Unbounded_String

   package body Pool is

      procedure Free (Request : in out Parsing_Request_Record);

      ----------------
      -- Unique_Key --
      ----------------

      function Unique_Key (Request : Parsing_Request) return Unbounded_String
      is
        (Request.File_Name & Request.Project_Name & Request.Prio'Img);

      ---------------------------------
      -- Parsing_Requests_Queue_Type --
      ---------------------------------
      use type Ada.Containers.Count_Type;
      protected body Parsing_Requests_Queue_Type is

         -------------
         -- Enqueue --
         -------------

         procedure Enqueue (Request : in out Parsing_Request) is
            Req_Key : constant Unbounded_String := Unique_Key (Request);
         begin
            if Map.Contains (Req_Key) then
               --  There is already a request in, we need to check its status
               declare
                  Existing_Req : constant Parsing_Request :=
                    Map.Element (Req_Key);
                  use type Callbacks_Vectors.Vector;
               begin
                  --  if there is already a request and the new request is a
                  --  parse, the existing request will do in any case, so just
                  --  add the new callback if there is one
                  if Request.Kind = Parse then
                     Existing_Req.Callbacks :=
                       Request.Callbacks & Existing_Req.Callbacks;
                     Destroy (Request);
                  else
                     Request.Callbacks :=
                       Request.Callbacks & Existing_Req.Callbacks;

                     Free (Existing_Req.all);
                     Existing_Req.all := Request.all;
                     Free (Request);

                     --  Here, in the case where both requests are reparse, we
                     --  assume that Request is more recent than Existing_Req,
                     --  most notably in regards to unsaved_files. This fits
                     --  with the rest of the model, where only the main
                     --  threads enqueues requests, and the only unsaved
                     --  file is the current buffer
                  end if;
                  return;
               end;
            end if;

            --  Simple case, we don't yet have a request in, just add it
            Map.Include (Req_Key, Request);

            pragma Warnings (Off);
            Queue.Enqueue (Request);
            pragma Warnings (On);
         end Enqueue;

         -------------
         -- Dequeue --
         -------------

         entry Dequeue (Request : out Parsing_Request)
           when Queue.Current_Use > 0
         is
         begin
            pragma Warnings (Off);
            Queue.Dequeue (Request);
            pragma Warnings (On);
            Map.Delete (Unique_Key (Request));
         end Dequeue;

         function Length return Natural
         is
         begin
            return Natural (Queue.Current_Use);
         end Length;
      end Parsing_Requests_Queue_Type;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (Request : in out Parsing_Request) is
      begin
         Free (Request.all);
         Free (Request);
      end Destroy;

      ----------
      -- Free --
      ----------

      procedure Free (Request : in out Parsing_Request_Record) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Unbounded_String_Array, Unbounded_String_Array_Access);
      begin
         case Request.Kind  is
         when Parse =>
            Unchecked_Free (Request.Switches);
         when Reparse =>
            Destroy (Request.Unsaved_Files);
         end case;
      end Free;

      ------------------
      -- Parsing_Task --
      ------------------

      task body Parsing_Task is
         Request   : Parsing_Request;
         Dummy     : Boolean;
         Stopped   : Boolean := False;
         Do_Finish : Boolean := False;
      begin
         Trace (Me, "In parsing task " & Image (Current_Task));
         loop
            declare
               TU        : Clang_Translation_Unit;
               Responded : Boolean := False;
            begin
               select
                  accept Start do Stopped := False; end Start;
               or
                  accept Stop do Stopped := True; end Stop;
               or
                  accept Finish do
                     Trace (Me, "Task " & Image (Current_Task) & "finishing");
                     Do_Finish := True;
                  end Finish;
               or
                  delay 0.01;
               end select;

               if Do_Finish then
                  goto End_Label;
               end if;

               if Stopped then
                  goto Cont;
               end if;

               select
                  Parsing_Request_Queue.Dequeue (Request);
               or
                    --  If we cannot dequeue, timeout and restart the loop, so
                    --  that the entries get a chance to get evaluated
                  delay 0.01;
                  goto Cont;
               end select;

               if Request.File_Name = "" then
                  Parsing_Response_Queue.Enqueue
                    (Parsing_Response'
                       (No_Translation_Unit,
                        Request.File_Name, Request.Context,
                        Request.Callbacks));
               else
                  Trace (Me, "Parsing " & (+Request.File_Name)
                         & ", From task :" & Image (Current_Task)
                         & ", Priority : " & Request.Prio'Img
                         & ", Request kind : " & Request.Kind'Img);

                  case Request.Kind is
                  when Parse =>
                     TU := Parse_Translation_Unit
                       (Request.Indexer,
                        Source_Filename   => +Request.File_Name,
                        Command_Line_Args => Request.Switches.all,
                        Unsaved_Files     => No_Unsaved_Files,
                        Options           => Request.Options);
                  when Reparse =>
                     Dummy := Reparse_Translation_Unit
                       (Request.TU, Request.Unsaved_Files.all,
                        Options => Request.Options);
                     TU := Request.TU;
                  end case;

                  Parsing_Response_Queue.Enqueue
                    (Parsing_Response'
                       (TU, Request.File_Name, Request.Context,
                        Request.Callbacks));

               end if;

               Destroy (Request);
               Responded := True;

               <<Cont>>
            exception
               when others =>

                  --  Queue a response anyway so as to not block a thread
                  --  eventually waiting for the TU
                  if not Responded then
                     Parsing_Response_Queue.Enqueue
                       (Parsing_Response'
                          (No_Translation_Unit,
                           Request.File_Name, Request.Context,
                           Request.Callbacks));

                     Free (Request);
                  end if;
                  Trace
                    (Me, "Exception in parsing task" & Image (Current_Task));
            end;
         end loop;

         <<End_Label>>

      end Parsing_Task;

   end Pool;
end Libclang.Task_Parser_Pool;
