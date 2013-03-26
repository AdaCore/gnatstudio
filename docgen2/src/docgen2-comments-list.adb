------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

   end Version_2;

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
            Analysed   => False);
         List.Vector.Append (Elem);
      end if;
   end Add_Comment_Line;

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
      return Version_2.Find_Doc
               (Sloc_Start, Sloc_End, Comments, File_Doc);
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

end Docgen2.Comments.List;
