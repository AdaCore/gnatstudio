-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Language;              use Language;

package Docgen2_Backend is

   type Backend_Record is abstract tagged null record;
   type Backend_Handle is access all Backend_Record'Class;

   type Template_Kind is
     (Tmpl_Spec,
      --  Public entities documentation
      Tmpl_Class_Tree,
      --  Global Class Tree
      Tmpl_Class_Tree_Elem,
      --  Class Tree Element
      Tmpl_Index,
      --  packages, source files and entities Index
      Tmpl_User_Defined_File,
      --  user defined doc file
      Tmpl_Src,
      --  Source code
      Tmpl_Src_Index
      --  Source code index
     );

   function Get_Template (Backend    : access Backend_Record;
                          System_Dir : String;
                          Kind       : Template_Kind) return String
                          is abstract;
   --  Return the full path to the template used for transformation
   --  System_Dir is the main GPS directory

   function Get_Support_Dir (Backend    : access Backend_Record;
                             System_Dir : String) return String
                             is abstract;
   --  Return the full path to the directory containing the documentation's
   --  support files (scripts, css files, etc.)

   function To_Destination_Name
     (Backend  : access Backend_Record;
      Basename : String)
      return String is abstract;
   --  Return a basename's destination file from source file.
   --  for example: gps-kernel.ads will be converted to gps-kernel.ads.html
   --  by the html backend

   function To_Destination_Name
     (Backend  : access Backend_Record;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String is abstract;
   --  Return a basename's destination file from a package location and a
   --  package index.
   --  for example: "gps-kernel.ads:5:4" and "2" will return
   --  gps-kernel.ads-2.html

   function To_Href
     (Backend  : access Backend_Record;
      Location : String;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String is abstract;
   --  Translate a location into an href that can be used by the Gen_Href
   --  function below.
   --  Location is the location of the entity, in the form file:line:col
   --  Pkg_Loc is the location of the pkg, or empty string if not applicable
   --  Pkg_Nb is the indes of the pkg in the source file, or "0" if not
   --   applicable

   function Gen_Paragraph
     (Backend : access Backend_Record;
      Msg     : String) return String is abstract;
   --  Return Msg as new paragraph.
   --  For example, html will return <p>Msg</p>

   function Gen_Ref
     (Backend : access Backend_Record;
      Name    : String) return String is abstract;
   --  Generate the ref that will allow the navigation to this point.
   --  for example: in HTML, this will generate <a name="Name"></a>

   function Gen_Href
     (Backend                : access Backend_Record;
      Name, Href, Title : String)
      return String is abstract;
   --  Generate the actual code used by the backend to navigate to xref.
   --  for example: html backend will return
   --  <a href="Href" title="Title">Name</a>

   function Gen_Tag
     (Backend  : access Backend_Record;
      Tag      : Language_Entity;
      Value    : String;
      Emphasis : Boolean := False)
      return String is abstract;
   --  Generate a tag. The possible tags are
   --  'keyword'
   --  Tags that have cross-references are treated by Gen_Href instead of
   --  Gen_Tag
   --  for example: html backend will generate "<span class='name'>Name</span>"
   --  for 'name' tags.

   function Gen_User_Tag
     (Backend    : access Backend_Record;
      User_Tag   : String;
      Attributes : String;
      Value      : String) return String is abstract;
   --  Generate a text block corresponding to the user tag.

   function Filter
     (Backend : access Backend_Record;
      S       : String) return String is abstract;
   --  Filter the text so that it is compatible with the backend.
   --  For example: for html backend, it will replace '<' '>' and '&' by
   --  respectively &lt; &gt; and &amp;

   procedure Begin_Handle_Code
     (Backend : access Backend_Record;
      Buffer  : in out Unbounded_String;
      Current : out Unbounded_String) is abstract;
   --  See inherited doc

   procedure End_Handle_Code
     (Backend : access Backend_Record;
      Buffer  : in out Unbounded_String;
      Current : in out Unbounded_String;
      Line    : in out Natural) is abstract;
   --  See inherited doc

   procedure Handle_Code
     (Backend : access Backend_Record;
      Text    :        String;
      Buffer  : in out Unbounded_String;
      Current : in out Unbounded_String;
      Line    : in out Natural;
      Cb      : access function (S : String) return String) is abstract;
   --  Append new text to S, so that all LF characters are formatted for code
   --  output.
   --  Each new line appened is filtered via Cb

end Docgen2_Backend;
