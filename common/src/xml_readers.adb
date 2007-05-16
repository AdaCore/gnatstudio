-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                    Copyright (C) 2004-2007                        --
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

with Sax.Attributes;        use Sax.Attributes;
with Sax.Locators;          use Sax.Locators;
with Unicode;               use Unicode;
with Unicode.CES;           use Unicode.CES;
with Unicode.CES.Utf8;      use Unicode.CES.Utf8;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Glib;                  use Glib;
with Input_Sources;         use Input_Sources;
with Input_Sources.File;    use Input_Sources.File;
with Input_Sources.Strings; use Input_Sources.Strings;

package body XML_Readers is

   use Glib_XML;

   function Attributes_From_List
     (Atts : Sax.Attributes.Attributes'Class) return Glib.String_Ptr;
   --  Convert a list of attributes to a string suitable for Glib.XML

   procedure Parse
     (Start_Line : Natural := 1;
      Input : in out Input_Source'Class;
      Tree  : out Glib_XML.Node_Ptr;
      Error : out Unicode.CES.Byte_Sequence_Access);
   --  Parse an input stream.
   --  Input is freed before returning from this procedure
   --  Start_Line indicates the extra offset that should be added to line
   --  numbers in the error messages for proper location in their actual
   --  source file.

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Gtk_Reader) is
   begin
      Handler.Tree := null;
      Handler.Current_Node := null;
   end Start_Document;

   --------------------------
   -- Attributes_From_List --
   --------------------------

   function Attributes_From_List
     (Atts : Sax.Attributes.Attributes'Class) return Glib.String_Ptr
   is
      Str    : Unbounded_String;
      Length : constant Natural := Get_Length (Atts);
   begin
      for J in 0 .. Length - 1 loop
         Str := Str & Get_Local_Name (Atts, J) & "="""
           & Protect (Get_Value (Atts, J)) & """ ";
      end loop;

      return new String'(To_String (Str));
   end Attributes_From_List;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Gtk_Reader; Loc : in out Sax.Locators.Locator) is
   begin
      Set_Line_Number (Loc,
                       Get_Line_Number (Loc) + Handler.Start_Line - 1);
   end Set_Document_Locator;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Local_Name, Namespace_URI);
      N : Node_Ptr;
   begin
      N := new Glib_XML.Node'
        (Tag           => new String'(Qname),
         Attributes    => Attributes_From_List (Atts),
         Value         => new String'(""),
         Parent        => Handler.Current_Node,
         Child         => null,
         Next          => null,
         Specific_Data => No_Specific_Data);

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

   procedure End_Element
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

   procedure Characters
     (Handler : in out Gtk_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      S : Glib.String_Ptr;
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
            Glib.Free (Handler.Current_Node.Value);
            Handler.Current_Node.Value := S;
         else
            Handler.Current_Node.Value := new String'(Ch);
         end if;
      end if;
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
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

   function Get_Tree (Read : Gtk_Reader) return Glib_XML.Node_Ptr is
   begin
      return Read.Tree;
   end Get_Tree;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Gtk_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Fatal_Error (Handler, Except);
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
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

   function Parse (File : String) return Glib_XML.Node_Ptr is
      Tree  : Glib_XML.Node_Ptr;
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
     (File  : String;
      Tree  : out Glib_XML.Node_Ptr;
      Error : out Unicode.CES.Byte_Sequence_Access)
   is
      Input  : File_Input;
   begin
      Open (File, Input);
      Set_Public_Id (Input, File);
      Set_System_Id (Input, File);
      Parse (1, Input, Tree, Error);
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Start_Line : Natural := 1;
      Input : in out Input_Source'Class;
      Tree  : out Glib_XML.Node_Ptr;
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

      Close (Input);

   exception
      when E : XML_Fatal_Error =>
         Free (Reader.Tree);
         Free (Reader);
         Error := new Byte_Sequence'(Exception_Message (E));
         Tree := null;
   end Parse;

   ------------------
   -- Parse_Buffer --
   ------------------

   procedure Parse_Buffer
     (Buffer     : Glib.UTF8_String;
      Tree       : out Glib_XML.Node_Ptr;
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
