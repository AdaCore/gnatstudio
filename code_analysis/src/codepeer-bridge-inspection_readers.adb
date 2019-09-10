------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with CodePeer.Bridge.Inspection_Readers.V4_5;
with CodePeer.Bridge.Inspection_Readers.V6;

package body CodePeer.Bridge.Inspection_Readers is

   Inspection_Tag          : constant String := "inspection";

   Command_Line_Main_Attribute  : constant String := "command_line_main";
   Command_Line_Switches_Attribute :
     constant String := "command_line_switches";
   Format_Attribute             : constant String := "format";
   Identifier_Attribute         : constant String := "identifier";
   Previous_Attribute           : constant String := "previous";
   Previous_Command_Line_Main_Attribute :
     constant String := "previous_command_line_main";
   Previous_Command_Line_Switches_Attribute :
     constant String := "previous_command_line_switches";
   Previous_Timestamp_Attribute : constant String := "previous_timestamp";
   Timestamp_Attribute          : constant String := "timestamp";
   Library_File_Attribute       : constant String := "library_file";

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

   begin
      if Self.Ignore_Depth /= 0 then
         --  Decrase depth of ignored XML element.

         Self.Ignore_Depth := Self.Ignore_Depth - 1;

      elsif Self.Reader_Depth /= 0 then
         Self.Reader_Depth := Self.Reader_Depth - 1;
         Self.Reader.End_Element (Qname);
      end if;
   end End_Element;

   ----------
   -- Hash --
   ----------

   function Hash (Item : CWE_Identifier) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self                  : in out Reader;
      Input                 : in out Input_Sources.Input_Source'Class;
      Kernel                : GPS.Kernel.Kernel_Handle;
      Tree                  : out Code_Analysis.Code_Analysis_Tree;
      Annotation_Categories : out Annotation_Category_Maps.Map;
      Messages              : out CodePeer.Message_Maps.Map;
      Version               : out Supported_Format_Version;
      Race_Category         : out CodePeer.Message_Category_Access)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Abstract_Inspection_Reader'Class, Inspection_Reader_Access);

   begin
      Self.Kernel          := Kernel;
      Self.Base_Directory  :=
        GNATCOLL.VFS.Create (Filesystem_String (Input.Get_System_Id)).Dir;
      Self.Root_Inspection := new CodePeer.Project_Data;

      Self.Version         := Supported_Format_Version'First;
      Self.Ignore_Depth    := 0;
      Self.Messages        := Messages'Unchecked_Access;

      Self.Parse (Input);

      Self.Reader.End_Document;

      Tree                  := Self.Reader.Get_Code_Analysis_Tree;
      Version               := Self.Version;
      Race_Category         := Self.Reader.Get_Race_Category;
      Annotation_Categories := Self.Reader.Get_Annotation_Categories;

      Free (Self.Reader);
   end Parse;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

      function Get_Value
        (Attrs      : Sax.Attributes.Attributes'Class;
         Local_Name : String;
         Default    : Ada.Calendar.Time := CodePeer.Unknown_Timestamp)
            return Ada.Calendar.Time;
      --  Returns values of given attribute if present or value of
      --  Default.

      function Get_Value
        (Attrs      : Sax.Attributes.Attributes'Class;
         Local_Name : String) return Ada.Strings.Unbounded.Unbounded_String;
      --  Returns values of given attribute if present or empty string.

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value
        (Attrs      : Sax.Attributes.Attributes'Class;
         Local_Name : String;
         Default    : Ada.Calendar.Time := CodePeer.Unknown_Timestamp)
            return Ada.Calendar.Time
      is
         Index : constant Integer := Attrs.Get_Index (Local_Name);

      begin
         if Index /= -1 then
            return
              Ada.Calendar.Formatting.Value (Attrs.Get_Value (Index));

         else
            return Default;
         end if;
      end Get_Value;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value
        (Attrs      : Sax.Attributes.Attributes'Class;
         Local_Name : String) return Ada.Strings.Unbounded.Unbounded_String
      is
         Index : constant Integer := Attrs.Get_Index (Local_Name);

      begin
         if Index /= -1 then
            return
              Ada.Strings.Unbounded.To_Unbounded_String
                (Attrs.Get_Value (Index));

         else
            return Ada.Strings.Unbounded.Null_Unbounded_String;
         end if;
      end Get_Value;

   begin
      if Self.Ignore_Depth /= 0 then
         Self.Ignore_Depth := Self.Ignore_Depth + 1;

      elsif Self.Reader_Depth /= 0 then
         Self.Reader_Depth := Self.Reader_Depth + 1;
         Self.Reader.Start_Element (Qname, Attrs);

      elsif Qname = Inspection_Tag then
         declare
            Data : CodePeer.Project_Data'Class
              renames CodePeer.Project_Data'Class (Self.Root_Inspection.all);

         begin
            Data.Current.Inspection :=
              Natural'Value (Attrs.Get_Value (Identifier_Attribute));
            Data.Current.Timestamp := Get_Value (Attrs, Timestamp_Attribute);
            Data.Current.Main :=
              Get_Value (Attrs, Command_Line_Main_Attribute);
            Data.Current.Switches :=
              Get_Value (Attrs, Command_Line_Switches_Attribute);
            Data.Current.Library_File :=
              Get_Value (Attrs, Library_File_Attribute);

            Data.Baseline.Inspection :=
              Natural'Value (Attrs.Get_Value (Previous_Attribute));
            Data.Baseline.Timestamp :=
              Get_Value (Attrs, Previous_Timestamp_Attribute);
            Data.Baseline.Main :=
              Get_Value (Attrs, Previous_Command_Line_Main_Attribute);
            Data.Baseline.Switches :=
              Get_Value (Attrs, Previous_Command_Line_Switches_Attribute);
            Data.Baseline.Library_File :=
              Get_Value (Attrs, Library_File_Attribute);

            Self.Version :=
              Format_Version'Value (Attrs.Get_Value (Format_Attribute));

            case Self.Version is
               when 4 .. 5 =>
                  Self.Reader :=
                    CodePeer.Bridge.Inspection_Readers.V4_5
                      .Create_Inspection_Reader_V4_5
                        (Self.Kernel,
                         Self.Base_Directory,
                         Self.Root_Inspection,
                         Self.Messages);

               when 6 =>
                  Self.Reader :=
                    CodePeer.Bridge.Inspection_Readers.V6
                      .Create_Inspection_Reader_V6
                        (Self.Kernel,
                         Self.Base_Directory,
                         Self.Root_Inspection,
                         Self.Messages);
            end case;

            Self.Reader_Depth := 1;
         end;

      else
         --  Activate ignore of nested XML elements to be able to load data
         --  files of newer version then supported by GPS.

         Self.Ignore_Depth := 1;
      end if;
   end Start_Element;

end CodePeer.Bridge.Inspection_Readers;
