------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNAT.Regpat;                use GNAT.Regpat;

with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

package body Docgen2.Comments.List is

   ---------------
   -- Version_2 --
   ---------------

   --  Local package containing the behavior required for Docgen version 2

   package Version_2 is

      function Find_Doc
        (Sloc_Start : Source_Location;
         Sloc_End   : Source_Location;
         Comments   : Comments_List;
         File_Doc   : Boolean := False) return Comment_Type;
      --  Find a comment placed just before Sloc_Start or just after Sloc_End.
      --  If File_Doc is set, then the first comment found before Sloc_Start is
      --  returned. Used when finding the global documentation for a file or
      --  package.

   end Version_2;

   ---------------
   -- Version_3 --
   ---------------

   --  Local package containing the behavior required for Docgen version 3

   package Version_3 is

      function Find_Doc
        (Regexp     : GNAT.Expect.Pattern_Matcher_Access;
         Sloc_Start : Source_Location;
         Sloc_End   : Source_Location;
         Category   : Entity_Info_Category;
         Comments   : Comments_List;
         File_Doc   : Boolean := False) return Comment_Type;
      --  Filter comments using the Regexp pattern and find a comment placed
      --  just before Sloc_Start or just after Sloc_End (or in middle if the
      --  comment corresponds with the Category of a parameter).
      --  If File_Doc is set, then the first comment found before Sloc_Start is
      --  returned. Used when finding the global documentation for a file or
      --  package.

   end Version_3;

   ----------------------
   -- Add_Comment_Line --
   ----------------------

   procedure Add_Comment_Line
     (Sloc_Start : Source_Location;
      Sloc_Stop  : Source_Location;
      Comment    : String;
      Force_New  : Boolean;
      List       : in out Comments_List)
   is
      Elem : Comment_Access;

   begin
      if Comment = "" then
         return;
      end if;

      if not Force_New
        and then not List.Vector.Is_Empty
        and then List.Vector.Last_Element.Sloc_Stop.Line + 1 >= Sloc_Start.Line
      then
         Elem := List.Vector.Last_Element;
         Elem.Block := Elem.Block & Comment;
         Elem.Sloc_Stop := Sloc_Stop;

      else
         Elem := new Comment_Type'
           (Block      => To_Unbounded_String (Comment),
            Sloc_Start => Sloc_Start,
            Sloc_Stop  => Sloc_Stop,
            Analysed   => False,
            Filtered   => False,
            Retrieved  => False);
         List.Vector.Append (Elem);
      end if;
   end Add_Comment_Line;

   --------------
   -- Find_Doc --
   --------------

   function Find_Doc
     (Regexp     : GNAT.Expect.Pattern_Matcher_Access;
      Sloc_Start : Source_Location;
      Sloc_End   : Source_Location;
      Category   : Entity_Info_Category;
      Comments   : Comments_List;
      File_Doc   : Boolean := False) return Comment_Type
   is
   begin
      if not Active (DOCGEN_V3) then
         return Version_2.Find_Doc
                  (Sloc_Start, Sloc_End, Comments, File_Doc);
      else
         return Version_3.Find_Doc
                  (Regexp, Sloc_Start, Sloc_End, Category, Comments, File_Doc);
      end if;
   end Find_Doc;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Comments_List) is
      procedure Internal is
         new Ada.Unchecked_Deallocation (Comment_Type, Comment_Access);

      Comment : Comment_Access;
   begin
      for J in reverse List.Vector.First_Index .. List.Vector.Last_Index loop
         Comment := List.Vector.Element (J);
         Free (Comment.all);
         Internal (Comment);
         List.Vector.Delete (J);
      end loop;
   end Free;

   ---------------
   -- Version_2 --
   ---------------

   package body Version_2 is

      --------------
      -- Find_Doc --
      --------------

      function Find_Doc
        (Sloc_Start : Source_Location;
         Sloc_End   : Source_Location;
         Comments   : Comments_List;
         File_Doc   : Boolean := False) return Comment_Type
      is
      begin
         for J in Comments.Vector.First_Index .. Comments.Vector.Last_Index
         loop
            if File_Doc
              and then
                Comments.Vector.Element (J).Sloc_Start.Line < Sloc_Start.Line
              and then
                (J = Comments.Vector.Last_Index
                   or else Comments.Vector.Element (J + 1).Sloc_Start.Line
                             > Sloc_Start.Line)
            then
               return Comments.Vector.Element (J).all;

            elsif Sloc_Start.Line - 1
                    = Comments.Vector.Element (J).Sloc_Stop.Line
              or else Sloc_End.Line + 1
                    = Comments.Vector.Element (J).Sloc_Start.Line
            then
               --  ??? Should not return twice a comment. Mark it as used ?
               return Comments.Vector.Element (J).all;
            end if;

            exit when Comments.Vector.Element (J).Sloc_Start > Sloc_End;
         end loop;

         return No_Comment;
      end Find_Doc;

   end Version_2;

   ---------------
   -- Version_3 --
   ---------------

   package body Version_3 is

      --------------
      -- Find_Doc --
      --------------

      function Find_Doc
        (Regexp     : GNAT.Expect.Pattern_Matcher_Access;
         Sloc_Start : Source_Location;
         Sloc_End   : Source_Location;
         Category   : Entity_Info_Category;
         Comments   : Comments_List;
         File_Doc   : Boolean := False) return Comment_Type
      is
         use type GNAT.Expect.Pattern_Matcher_Access;

         procedure Filter (Comment : Comment_Access);
         --  Filter lines contained in Comment.Block using the regular
         --  expression

         procedure Filter (Comment : Comment_Access) is
         begin
            --  Handle cases in which no action is needed

            if Comment.Filtered
              or else Regexp = null
              or else Comment.Block = Null_Unbounded_String
              or else Comment.Block = ""
            then
               return;
            end if;

            --  Apply the user-defined filter to each line of the block of
            --  comments

            declare
               S         : constant String := To_String (Comment.Block);
               Matches   : Match_Array (0 .. 0);
               New_Block : Unbounded_String;
               F         : Natural;
               L         : Natural;

            begin
               L := S'First;

               while L <= S'Last loop

                  --  Identify the next comment line

                  F := L;

                  while L <= S'Last and then S (L) /= ASCII.LF loop
                     L := L + 1;
                  end loop;

                  --  Apply to it the user-defined filter

                  declare
                     Line : constant String := S (F .. L - 1);

                  begin
                     Match (Regexp.all, Line, Matches);

                     --  If the line has the pattern then remove from it
                     --  the matching pattern and append it to the new
                     --  block of comments

                     if Matches (0) /= No_Match then
                        declare
                           F1 : constant Natural := Matches (0).First;
                           L1 : constant Natural := Matches (0).Last;

                           Filtered_Line : constant String :=
                             Line (Line'First .. F1 - 1) &
                             Line (L1 + 1 .. Line'Last);
                        begin
                           Append (New_Block, Filtered_Line & ASCII.LF);
                        end;
                     end if;
                  end;

                  --  Skip line terminators

                  while L <= S'Last and then S (L) = ASCII.LF loop
                     L := L + 1;
                  end loop;
               end loop;

               Comment.Block := New_Block;
               Comment.Filtered := True;
            end;
         end Filter;

         --  Local variables

         Comment : Comment_Access;
      begin
         for J in Comments.Vector.First_Index .. Comments.Vector.Last_Index
         loop
            Comment := Comments.Vector.Element (J);
            Filter (Comment);

            --  Skip comments previously returned or empty comments (since
            --  filtering may have discarded the full contents of this
            --  message)

            if Comments.Vector.Element (J).Retrieved
              or else Comment.Block = Null_Unbounded_String
            then
               null;

            --  Give more priority to comments of parameters which are located
            --  in the line of declaration of the parameter

            elsif not File_Doc
              and then Category = Cat_Parameter
              and then Comment.Sloc_Start.Line >= Sloc_Start.Line
              and then Comment.Sloc_Stop.Line <= Sloc_End.Line
            then
               Comment.Retrieved := True;
               return Comment.all;

            elsif File_Doc
              and then Comment.Sloc_Start.Line < Sloc_Start.Line
              and then
                (J = Comments.Vector.Last_Index
                   or else Comments.Vector.Element (J + 1).Sloc_Start.Line
                             > Sloc_Start.Line)
            then
               Comment.Retrieved := True;
               return Comment.all;

            elsif Sloc_Start.Line - 1 = Comment.Sloc_Stop.Line
              or else Sloc_End.Line + 1 = Comment.Sloc_Start.Line
            then
               Comment.Retrieved := True;
               return Comment.all;
            end if;

            exit when Comment.Sloc_Start > Sloc_End;
         end loop;

         return No_Comment;
      end Find_Doc;

   end Version_3;

end Docgen2.Comments.List;
