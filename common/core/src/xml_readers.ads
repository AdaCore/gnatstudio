------------------------------------------------------------------------------
--                      XML/Ada - An XML suite for Ada                      --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

with Sax.Readers;          use Sax.Readers;
with Sax.Attributes;
with Sax.Exceptions;
with Sax.Locators;
with Unicode.CES;
with XML_Utils;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

package XML_Readers is

   type Gtk_Reader is new Reader with private;
   type Gtk_Reader_Access is access all Gtk_Reader'Class;
   --  Special SAX Reader that creates a tree compatible with Glib.XML, found
   --  in the GtkAda distribution. This allows replacing the parser in that
   --  package with the better one in XML/Ada.
   --  You should free the previous Tree before calling Parse multiple times
   --  if you want to avoid memory leaks.

   function Parse (File : Virtual_File) return XML_Utils.Node_Ptr;
   --  Same as Glib.Xml.Parse, but uses XML/Ada as the XML parser instead.
   --  Errors in the XML file will return a null value, but the error itself
   --  is no longer accessible.

   procedure Parse
       (File  : Virtual_File;
        Tree  : out XML_Utils.Node_Ptr;
        Error : out Unicode.CES.Byte_Sequence_Access);
   --  Same as above, except error messages are made available to the caller.
   --  Both return value must be freed by the user.
   --  If there is an error, Tree is always set to null.

   procedure Parse_Buffer
     (Buffer     : XML_Utils.UTF8_String;
      Tree       : out XML_Utils.Node_Ptr;
      Error      : out Unicode.CES.Byte_Sequence_Access;
      From_File  : String := "<input>";
      Start_Line : Natural := 1);
   --  Same as Parse, but the string to parse is already in memory.
   --  (From_File, Start_Line) can be used to identify where the buffer was
   --  read from, and will show up in error messages locations

   function Get_Tree (Read : Gtk_Reader) return XML_Utils.Node_Ptr;
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
      Tree                       : XML_Utils.Node_Ptr;
      Start_Line                 : Natural := 1;
      Current_Node               : XML_Utils.Node_Ptr;
      Internal_Encoding          : Unicode.CES.Encoding_Scheme;
      Warnings_As_Error          : Boolean := False;
   end record;

   overriding procedure Start_Document (Handler : in out Gtk_Reader);
   overriding procedure Set_Document_Locator
     (Handler : in out Gtk_Reader; Loc : in out Sax.Locators.Locator);
   overriding procedure Start_Element
     (Handler       : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   overriding procedure End_Element
     (Handler : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   overriding procedure Characters
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   overriding procedure Ignorable_Whitespace
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   overriding procedure Error
     (Handler : in out Gtk_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   overriding procedure Warning
     (Handler : in out Gtk_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
end XML_Readers;
