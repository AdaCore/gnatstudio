------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
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

with Sax.Attributes;        use Sax.Attributes;
with Sax.Locators;          use Sax.Locators;
with Unicode;               use Unicode;
with Unicode.CES;           use Unicode.CES;
with Unicode.CES.Utf8;      use Unicode.CES.Utf8;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Input_Sources;         use Input_Sources;
with Input_Sources.Mmap;    use Input_Sources.Mmap;
with Input_Sources.Strings; use Input_Sources.Strings;

package body XML_Readers is

   use XML_Utils;

   function Attributes_From_List
     (Atts : Sax.Attributes.Attributes'Class) return XML_Utils.String_Ptr;
   --  Convert a list of attributes to a string suitable for XML_Utils

   procedure Parse
     (Start_Line : Natural := 1;
      Input : in out Input_Source'Class;
      Tree  : out XML_Utils.Node_Ptr;
      Error : out Unicode.CES.Byte_Sequence_Access);
   --  Parse an input stream.
   --  Input is freed before returning from this procedure
   --  Start_Line indicates the extra offset that should be added to line
   --  numbers in the error messages for proper location in their actual
   --  source file.

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document (Handler : in out Gtk_Reader) is
   begin
      Handler.Tree := null;
      Handler.Current_Node := null;
   end Start_Document;

   --------------------------
   -- Attributes_From_List --
   --------------------------

   function Attributes_From_List
     (Atts : Sax.Attributes.Attributes'Class) return XML_Utils.String_Ptr
   is
      Str    : Unbounded_String;
      Length : constant Natural := Get_Length (Atts);
   begin
      for J in 0 .. Length - 1 loop
         if J /= 0 then
            Append (Str, " ");
         end if;

         Append (Str, Get_Local_Name (Atts, J));
         Append (Str, "=""");
         Append (Str, Protect (Get_Value (Atts, J)));
         Append (Str, '"');
      end loop;

      return new String'(To_String (Str));
   end Attributes_From_List;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Handler : in out Gtk_Reader; Loc : in out Sax.Locators.Locator) is
   begin
      Set_Line_Number (Loc,
                       Get_Line_Number (Loc) + Handler.Start_Line - 1);
   end Set_Document_Locator;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Local_Name, Namespace_URI);
      N : Node_Ptr;
   begin
      N := new XML_Utils.Node'
        (Tag           => new String'(Qname),
         Attributes    => Attributes_From_List (Atts),
         Value         => new String'(""),
         Parent        => null,
         Child         => null,
         Next          => null,
         Specific_Data => 0);

      if Handler.Current_Node /= null then
         Add_Child (Handler.Current_Node, N, Append => True);
      else
         Handler.Tree := N;
      end if;

      Handler.Current_Node := N;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Warnings (Off, Namespace_URI);
      pragma Warnings (Off, Local_Name);
      pragma Warnings (Off, Qname);
   begin
      Handler.Current_Node := Handler.Current_Node.Parent;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Handler : in out Gtk_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      S : XML_Utils.String_Ptr;
   begin
      if Handler.Current_Node /= null then
         if Handler.Current_Node.Value /= null then
            --  Take care not to allocate anything on the stack here, as
            --  read values might be very large.
            S := new String
              (1 .. Handler.Current_Node.Value'Length + Ch'Length);
            S (1 .. Handler.Current_Node.Value'Length) :=
              Handler.Current_Node.Value.all;
            S (Handler.Current_Node.Value'Length + 1 .. S'Last) := Ch;
            Free (Handler.Current_Node.Value);
            Handler.Current_Node.Value := S;
         else
            Handler.Current_Node.Value := new String'(Ch);
         end if;
      end if;
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   overriding procedure Ignorable_Whitespace
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
   begin
      --  Consider that whitespace is significant in some of the nodes.
      --  Ideally, this should be done from the grammar for the XML files, but
      --  we currently do not use it.
      if Handler.Current_Node /= null
        and then Handler.Current_Node.Tag.all = "clipboard"
      then
         Characters (Handler, Ch);
      end if;
   end Ignorable_Whitespace;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Read : Gtk_Reader) return XML_Utils.Node_Ptr is
   begin
      return Read.Tree;
   end Get_Tree;

   -----------
   -- Error --
   -----------

   overriding procedure Error
     (Handler : in out Gtk_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Fatal_Error (Handler, Except);
   end Error;

   -------------
   -- Warning --
   -------------

   overriding procedure Warning
     (Handler : in out Gtk_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      if Handler.Warnings_As_Error then
         Fatal_Error (Handler, Except);
      end if;
   end Warning;

   ----------------------------
   -- Set_Warnings_As_Errors --
   ----------------------------

   procedure Set_Warnings_As_Errors
     (Read : in out Gtk_Reader; Warnings_As_Error : Boolean) is
   begin
      Read.Warnings_As_Error := Warnings_As_Error;
   end Set_Warnings_As_Errors;

   ----------
   -- Free --
   ----------

   procedure Free (Read : in out Gtk_Reader) is
   begin
      Read.Tree := null;
   end Free;

   -----------
   -- Parse --
   -----------

   function Parse (File : Virtual_File) return XML_Utils.Node_Ptr is
      Tree  : XML_Utils.Node_Ptr;
      Error : Unicode.CES.Byte_Sequence_Access;
   begin
      Parse (File, Tree, Error);
      Free (Error);
      return Tree;
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (File  : Virtual_File;
      Tree  : out XML_Utils.Node_Ptr;
      Error : out Unicode.CES.Byte_Sequence_Access)
   is
      Input : Mmap_Input;
   begin
      Open (File.Full_Name, Input);
      Parse (1, Input, Tree, Error);
      Close (Input);
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Start_Line : Natural := 1;
      Input : in out Input_Source'Class;
      Tree  : out XML_Utils.Node_Ptr;
      Error : out Unicode.CES.Byte_Sequence_Access)
   is
      Reader : Gtk_Reader;
   begin
      Set_Warnings_As_Errors (Reader, True);
      Set_Feature (Reader, Validation_Feature, False);
      Set_Feature (Reader, Test_Valid_Chars_Feature, True);
      Reader.Start_Line := Start_Line;

      Parse (Reader, Input);
      Tree  := Get_Tree (Reader);
      Error := null;

      Free (Reader);
      Close (Input);

   exception
      when E : XML_Fatal_Error =>
         Free (Reader.Tree);
         Free (Reader);
         Close (Input);
         Error := new Byte_Sequence'(Exception_Message (E));
         Tree := null;
   end Parse;

   ------------------
   -- Parse_Buffer --
   ------------------

   procedure Parse_Buffer
     (Buffer     : XML_Utils.UTF8_String;
      Tree       : out XML_Utils.Node_Ptr;
      Error      : out Unicode.CES.Byte_Sequence_Access;
      From_File  : String := "<input>";
      Start_Line : Natural := 1)
   is
      Input : String_Input;
   begin
      Open (Buffer'Unrestricted_Access, Utf8_Encoding, Input);
      Set_Public_Id (Input, From_File);
      Set_System_Id (Input, From_File);
      Parse (Start_Line, Input, Tree, Error);
   end Parse_Buffer;

end XML_Readers;
