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

with Ada.Containers.Vectors;
with Docgen2.Entities;       use Docgen2.Entities;
with Language;               use Language;

package Docgen2.Comments.List is

   type Comments_List is private;

   procedure Add_Comment_Line
     (Sloc_Start : Source_Location;
      Sloc_Stop  : Source_Location;
      Comment    : String;
      Force_New  : Boolean;
      List       : in out Comments_List);
   --  Add a new comment line to the comments_list

   function Find_Doc
     (Regexp     : GNAT.Expect.Pattern_Matcher_Access;
      Sloc_Start : Source_Location;
      Sloc_End   : Source_Location;
      Category   : Entity_Info_Category;
      Comments   : Comments_List;
      File_Doc   : Boolean := False) return Comment_Type;
   --  Filter comments using the Regexp pattern and find a comment placed just
   --  before Sloc_Start or just after Sloc_End (or in middle of entities whose
   --  Category is a parameter under Docgen V3).
   --  Comments is the list of comments constructed using Add_Comment_Line and
   --  Analyse_Comments.
   --  If File_Doc is set, then the first comment found before Sloc_Start is
   --  returned. Used when finding the global documentation for a file or
   --  package.
   --  The returned comment is a deep copy of the object in the list, so that
   --  the list can be freed while the returned values remain OK. This should
   --  then be freed manually.

   procedure Free (List : in out Comments_List);
   --  Free memory allocated by List.

private

   package Container is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Comment_Access);

   type Comments_List is record
      Vector : Container.Vector;
   end record;

end Docgen2.Comments.List;
