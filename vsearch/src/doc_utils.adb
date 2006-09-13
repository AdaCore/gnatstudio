-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
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

with Basic_Types;            use Basic_Types;
with Glib.Convert;
with Entities;               use Entities;
with Entities.Queries;       use Entities.Queries;
with Language;               use Language;
with String_Utils;           use String_Utils;
with Find_Utils;             use Find_Utils;
with Interfaces.C.Strings;   use Interfaces.C.Strings;
with Traces;                 use Traces;
with GPS.Kernel.Charsets;    use GPS.Kernel.Charsets;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with VFS;                    use VFS;
with Language_Handlers;      use Language_Handlers;
with Language.Documentation; use Language.Documentation;

package body Doc_Utils is

   Me         : constant Debug_Handle := Create ("Doc_Utils");
   Extract_Me : constant Debug_Handle := Create ("COMMENT_EXTRACTION", On);

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Lang_Handler : access Language_Handlers.Language_Handler_Record'Class;
      Entity       : Entities.Entity_Information;
      Declaration_File_Contents : String := "") return String
   is
      Buffer             : Unchecked_String_Access :=
                             Declaration_File_Contents'Unrestricted_Access;
      Buffer_Len         : aliased Natural := Declaration_File_Contents'Length;
      Read               : aliased Natural;
      Chars_Buffer       : chars_ptr;

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
      Lines_Skipped      : Natural;
      Line               : Integer;
      Column             : Basic_Types.Character_Offset_Type;

   begin
      if not Active (Extract_Me) then
         Trace (Me, "Comment extraction disabled");
         return "";
      end if;

      if Lang = null then
         if Active (Me) then
            Trace (Me, "Get_Documentation language unknown for "
                   & Full_Name (Declaration_File).all);
         end if;

         return "";
      end if;

      Context := Get_Language_Context (Lang);

      if Declaration_File_Contents = "" then
         declare
            Tmp_Buffer : GNAT.OS_Lib.String_Access;
         begin
            Tmp_Buffer := Read_File (Declaration_File);

            if Tmp_Buffer = null then
               if Active (Me) then
                  Trace (Me, "Get_Documentation, no file found "
                         & Full_Name (Declaration_File).all);
               end if;

               return "";
            end if;

            Chars_Buffer := Glib.Convert.Convert
              (Str          => Tmp_Buffer.all,
               To_Codeset   => "UTF-8",
               From_Codeset => Get_File_Charset (Declaration_File),
               Bytes_Read   => Read'Unchecked_Access,
               Bytes_Written => Buffer_Len'Unchecked_Access);
            Free (Tmp_Buffer);

            if Chars_Buffer = Null_Ptr then
               --  In case the conversion to UTF-8 failed
               return "";
            end if;

            Buffer := To_Unchecked_String (Chars_Buffer);
            Index := 1;
         end;
      end if;

      Line   := Get_Declaration_Of (Entity).Line;
      Column := Basic_Types.Character_Offset_Type
        (Get_Declaration_Of (Entity).Column);
      Find_Closest_Match
        (Buffer (1 .. Buffer_Len), Line, Column,
         Get_Name (Entity).all,
         Case_Sensitive => Context.Case_Sensitive);
      Skip_Lines (Buffer (1 .. Buffer_Len), Line - 1, Index, Lines_Skipped);

      if Lines_Skipped /= Line - 1 then
         return "";
      end if;

      --  First, look for documentation before the entity's declaration.

      Get_Documentation_Before
        (Context       => Context.all,
         Buffer        => Buffer (1 .. Buffer_Len),
         Decl_Index    => Index,
         Comment_Start => Beginning,
         Comment_End   => Current,
         Debug         => Me);

      if Beginning = 0 then
         --  No documentation has been found before the entity.

         --  Search for documentation after the declaration.

         Get_Documentation_After
           (Context       => Context.all,
            Buffer        => Buffer (1 .. Buffer_Len),
            Decl_Index    => Index,
            Comment_Start => Beginning,
            Comment_End   => Current,
            Debug         => Me);

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
               Index := 1; --  Buffer'First
               Skip_Lines
                 (Buffer (1 .. Buffer_Len),
                  Get_Line (Location) - 1,
                  Index,
                  Lines_Skipped);

               if Lines_Skipped = Get_Line (Location) - 1 then
                  Get_Documentation_After
                    (Context       => Context.all,
                     Buffer        => Buffer (1 .. Buffer_Len),
                     Decl_Index    => Index,
                     Comment_Start => Beginning,
                     Comment_End   => Current,
                     Debug         => Me);
               end if;
            end if;
         end;
      end if;

      --  If not found, check the comment just before the body

      if Beginning = 0  then
         if Chars_Buffer /= Null_Ptr then
            Free (Chars_Buffer);
            Chars_Buffer := Null_Ptr;
            Buffer := null;
         end if;

         Find_Next_Body (Entity, Location => Location);

         if Location /= No_File_Location
           and then Location /= Get_Declaration_Of (Entity)
         then
            declare
               Tmp_Buffer : GNAT.OS_Lib.String_Access;
            begin
               Tmp_Buffer := Read_File (Get_Filename (Location.File));

               if Tmp_Buffer /= null then
                  Chars_Buffer := Glib.Convert.Convert
                    (Str          => Tmp_Buffer.all,
                     To_Codeset   => "UTF-8",
                     From_Codeset =>
                       Get_File_Charset (Get_Filename (Location.File)),
                     Bytes_Read   => Read'Unchecked_Access,
                     Bytes_Written => Buffer_Len'Unchecked_Access);
                  Free (Tmp_Buffer);

                  if Chars_Buffer = Null_Ptr then
                     return "";
                  end if;

                  Buffer := To_Unchecked_String (Chars_Buffer);

                  Index := 1;

                  Skip_Lines
                    (Buffer (1 .. Buffer_Len), Location.Line - 1, Index,
                     Lines_Skipped);

                  if Lines_Skipped = Location.Line - 1 then
                     Get_Documentation_Before
                       (Context       => Context.all,
                        Buffer        => Buffer (1 .. Buffer_Len),
                        Decl_Index    => Index,
                        Comment_Start => Beginning,
                        Comment_End   => Current,
                        Debug         => Me);
                  end if;
               end if;
            end;
         end if;
      end if;

      if Beginning /= 0 then
         declare
            Result : constant String := Comment_Block
              (Lang,
               Buffer (Line_Start (Buffer (1 .. Buffer_Len), Beginning)
                       .. Current),
               Comment => False,
               Clean   => True);
         begin
            if Chars_Buffer /= Null_Ptr then
               Free (Chars_Buffer);
               Chars_Buffer := Null_Ptr;
               Buffer := null;
            end if;

            return Result;
         end;
      else
         if Chars_Buffer /= Null_Ptr then
            Free (Chars_Buffer);
            Chars_Buffer := Null_Ptr;
            Buffer := null;
         end if;

         return "";
      end if;
   end Get_Documentation;

end Doc_Utils;
