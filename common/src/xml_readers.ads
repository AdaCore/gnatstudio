-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004                          --
--                             AdaCore                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Sax.Readers;          use Sax.Readers;
with Sax.Attributes;
with Sax.Exceptions;
with Sax.Locators;
with Unicode.CES;
with Glib.XML;

generic
   type XML_Specific_Data is private;
   No_Specific_Data : XML_Specific_Data;
   with package Glib_XML is new Glib.XML (XML_Specific_Data);
package XML_Readers is

   type Gtk_Reader is new Reader with private;
   type Gtk_Reader_Access is access all Gtk_Reader'Class;
   --  Special SAX Reader that creates a tree compatible with Glib.XML, found
   --  in the GtkAda distribution. This allows replacing the parser in that
   --  package with the better one in XML/Ada.
   --  You should free the previous Tree before calling Parse multiple times
   --  if you want to avoid memory leaks.

   function Parse (File : String) return Glib_XML.Node_Ptr;
   --  Same as Glib.Xml.Parse, but uses XML/Ada as the XML parser instead.
   --  Errors in the XML file will return a null value, but the error itself
   --  is no longer accessible.

   procedure Parse
       (File  : String;
        Tree  : out Glib_XML.Node_Ptr;
        Error : out Unicode.CES.Byte_Sequence_Access);
   --  Same as above, except error messages are made available to the caller.
   --  Both return value must be freed by the user.
   --  If there is an error, Tree is always set to null.

   procedure Parse_Buffer
     (Buffer     : Glib.UTF8_String;
      Tree       : out Glib_XML.Node_Ptr;
      Error      : out Unicode.CES.Byte_Sequence_Access;
      From_File  : String := "<input>";
      Start_Line : Natural := 1);
   --  Same as Parse, but the string to parse is already in memory.
   --  (From_File, Start_Line) can be used to identify where the buffer was
   --  read from, and will show up in error messages locations

   function Get_Tree (Read : Gtk_Reader) return Glib_XML.Node_Ptr;
   --  Get the tree that Read created

   procedure Free (Read : in out Gtk_Reader);
   --  Free the memory occupied by Read.
   --  This doesn't free the tree, you must do it yourself before this call.

   procedure Set_Warnings_As_Errors
     (Read : in out Gtk_Reader; Warnings_As_Error : Boolean);
   --  iF Warnings_As_Error is True, then all warnings will raise a fatal error
   --  exception, just like a fatal error. Otherwise, warnings are ignored.

private

   type Gtk_Reader is new Reader with record
      Tree                       : Glib_XML.Node_Ptr;
      Start_Line                 : Natural := 1;
      Current_Node               : Glib_XML.Node_Ptr;
      Internal_Encoding          : Unicode.CES.Encoding_Scheme;
      Warnings_As_Error          : Boolean := False;
   end record;

   procedure Start_Document (Handler : in out Gtk_Reader);
   procedure Set_Document_Locator
     (Handler : in out Gtk_Reader; Loc : in out Sax.Locators.Locator);
   procedure Start_Element
     (Handler       : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Ignorable_Whitespace
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Error
     (Handler : in out Gtk_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Warning
     (Handler : in out Gtk_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
end XML_Readers;
