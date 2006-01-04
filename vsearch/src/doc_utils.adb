-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                           --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Entities;              use Entities;
with Entities.Queries;      use Entities.Queries;
with Language;              use Language;
with String_Utils;          use String_Utils;
with Find_Utils;            use Find_Utils;
with Traces;                use Traces;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with VFS;                   use VFS;
with Language_Handlers; use Language_Handlers;

package body Doc_Utils is
   Me : constant Debug_Handle := Create ("Doc_Utils");

   procedure Get_Documentation_Before
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural);
   procedure Get_Documentation_After
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural);
   --  Get the comment just before or just after Decl_Index, skipping code
   --  lines as needed.

   ------------------------------
   -- Get_Documentation_Before --
   ------------------------------

   procedure Get_Documentation_Before
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural)
   is
      Current : Integer;
   begin
      Current := Decl_Index;
      Comment_Start := Current;
      Skip_To_Previous_Comment_Start (Context, Buffer, Comment_Start);

      if Comment_Start /= 0 then
         Comment_End := Comment_Start;
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);

         if Active (Me) then
            Trace (Me,
                   "Get_Documentation: Found a comment before the entity,"
                   & " from" & Comment_Start'Img & " to" & Comment_End'Img);
         end if;
      end if;
   end Get_Documentation_Before;

   -----------------------------
   -- Get_Documentation_After --
   -----------------------------

   procedure Get_Documentation_After
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural) is
   begin
      --  Else look after the comment after the declaration (which is the
      --  first block of comments after the declaration line, and not
      --  separated by a blank line)
      Comment_Start := Decl_Index;
      Skip_To_Next_Comment_Start (Context, Buffer, Comment_Start);

      if Comment_Start /= 0 then
         Comment_End := Comment_Start;
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);

         if Active (Me) then
            Trace (Me,
                   "Get_Documentation: Found a comment after the entity,"
                   & " from" & Comment_Start'Img & " to" & Comment_End'Img);
         end if;
      end if;
   end Get_Documentation_After;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Lang_Handler : access Language_Handlers.Language_Handler_Record'Class;
      Entity       : Entities.Entity_Information;
      Declaration_File_Contents : String := "") return String
   is
      Buffer             : String_Access :=
                             Declaration_File_Contents'Unrestricted_Access;
      Index              : Natural := Declaration_File_Contents'First;
      Declaration_File   : constant Virtual_File :=
                             Get_Filename (Get_Declaration_Of (Entity).File);
      Lang               : constant Language_Access :=
                             Get_Language_From_File
                               (Language_Handler (Lang_Handler),
                                Declaration_File);
      Current, Beginning : Natural;
      Context            : Language_Context_Access;
      Location           : File_Location;
      Must_Free_Buffer   : Boolean := False;
      Line, Column       : Integer;

   begin
      if Lang = null then
         if Active (Me) then
            Trace (Me, "Get_Documentation language unknown for "
                   & Full_Name (Declaration_File).all);
         end if;

         return "";
      end if;

      Context := Get_Language_Context (Lang);

      if Declaration_File_Contents = "" then
         Buffer := Read_File (Declaration_File);

         if Buffer = null then
            if Active (Me) then
               Trace (Me, "Get_Documentation, no file found "
                      & Full_Name (Declaration_File).all);
            end if;

            return "";
         end if;

         Must_Free_Buffer := True;
         Index := Buffer'First;
      end if;

      Line   := Get_Declaration_Of (Entity).Line;
      Column := Integer (Get_Declaration_Of (Entity).Column);
      Find_Closest_Match
        (Buffer.all, Line, Column,
         Get_Name (Entity).all,
         Case_Sensitive => Context.Case_Sensitive);
      Skip_Lines (Buffer.all, Line - 1, Index);

      --  First, look for documentation before the entity's declaration.

      Get_Documentation_Before
        (Context       => Context.all,
         Buffer        => Buffer.all,
         Decl_Index    => Index,
         Comment_Start => Beginning,
         Comment_End   => Current);

      if Beginning = 0 then
         --  No documentation has been found before the entity.

         --  Search for documentation after the declaration.

         Get_Documentation_After
           (Context       => Context.all,
            Buffer        => Buffer.all,
            Decl_Index    => Index,
            Comment_Start => Beginning,
            Comment_End   => Current);

         --  ??? The documentation should be returned only if followed with a
         --  blank line.
      end if;

      if Beginning = 0 then
         --  We search for documentation after the end of the declaration.
         declare
            Iter     : Entity_Reference_Iterator;
            Ref      : Entity_Reference;
            Kind     : Reference_Kind;
            Location : File_Location := No_File_Location;
         begin
            Get_End_Of_Scope (Entity, Location, Kind);

            if Kind /= End_Of_Spec then
               --  The end of scope does not correspond to the end of spec
               --  that needs to be search in the reference list.

               Location := No_File_Location;

               Find_All_References
                 (Iter,
                  Entity,
                  In_File => Get_Declaration_Of (Entity).File,
                  Filter  => (End_Of_Spec => True, others => False));

               if not At_End (Iter) then
                  Ref := Get (Iter);

                  if Get_Kind (Ref) = End_Of_Spec then
                     Location := Get_Location (Ref);
                  end if;
               end if;

               Destroy (Iter);
            end if;

            if Location /= No_File_Location then
               Index := Buffer'First;
               Skip_Lines
                 (Buffer.all,
                  Get_Line (Location) - 1,
                  Index);
               Index := Line_Start (Buffer.all, Index);

               Get_Documentation_After
                 (Context       => Context.all,
                  Buffer        => Buffer.all,
                  Decl_Index    => Index,
                  Comment_Start => Beginning,
                  Comment_End   => Current);
            end if;
         end;
      end if;

      --  If not found, check the comment just before the body

      if Beginning = 0  then
         if Must_Free_Buffer then
            Free (Buffer);
         end if;

         Find_Next_Body (Entity, Location => Location);

         if Location /= No_File_Location
           and then Location /= Get_Declaration_Of (Entity)
         then
            Buffer := Read_File (Get_Filename (Location.File));

            --  Buffer = null means that reading the file failed (e.g.
            --  the file does not exist or is not readable).

            if Buffer /= null then
               Must_Free_Buffer := True;
               Index := Buffer'First;

               Skip_Lines (Buffer.all, Location.Line - 1, Index);
               Get_Documentation_Before
                 (Context       => Context.all,
                  Buffer        => Buffer.all,
                  Decl_Index    => Index,
                  Comment_Start => Beginning,
                  Comment_End   => Current);
            end if;
         end if;
      end if;

      if Beginning /= 0 then
         declare
            Result : constant String := Comment_Block
              (Lang,
               Buffer (Line_Start (Buffer.all, Beginning) .. Current),
               Comment => False,
               Clean   => True);
         begin
            if Must_Free_Buffer then
               Free (Buffer);
            end if;

            return Result;
         end;
      else
         if Must_Free_Buffer then
            Free (Buffer);
         end if;

         return "";
      end if;
   end Get_Documentation;

end Doc_Utils;
