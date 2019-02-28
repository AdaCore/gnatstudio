------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded.Hash;

private with Sax.Attributes;
with Sax.Readers;
private with Unicode.CES;

with GNATStack.Data_Model;

package GNATStack.Readers is

   type Reader is new Sax.Readers.Reader with private;

   function Get_Data
     (Self : Reader'Class) return GNATStack.Data_Model.Analysis_Information;
   --  Returns loaded information.

private

   use GNATStack.Data_Model;

   type Parser_State_Kinds is
     (None_State,
      Subprogram_Set_State,
      Subprogram_Called_Set_State,
      Location_Set_State,
      Entry_Set_State,
      Entry_State,
      Cycle_Set_State,
      Cycle_State,
      Unbounded_Set_State,
      Unbounded_State,
      External_Set_State,
      External_State,
      Indirect_Set_State,
      Indirect_State,
      Indirect_Call_State,
      Subprogram_State,
      Boolean_Value_State,
      Integer_Value_State,
      String_Value_State);

   subtype Value_Kinds is Parser_State_Kinds
     range Boolean_Value_State .. String_Value_State;

   type Parser_State (Kind : Parser_State_Kinds := None_State) is record
      case Kind is
         when None_State =>
            null;

         when Subprogram_Set_State =>
            null;

         when Cycle_Set_State =>
            null;

         when Unbounded_Set_State =>
            null;

         when External_Set_State =>
            null;

         when Entry_Set_State =>
            null;

         when Indirect_Set_State =>
            null;

         when Indirect_Call_State =>
            Indirect : Indirect_Call_Information;

         when Subprogram_Called_Set_State =>
            Called_Set : Subprogram_Information_Sets.Set;

         when Location_Set_State =>
            Location_Set : Subprogram_Location_Sets.Set;

         when Entry_State =>
            C_Id        : Ada.Strings.Unbounded.Unbounded_String;
            Entry_Usage : Stack_Usage_Information;
            Chain       : Subprogram_Information_Vectors.Vector;

         when External_State =>
            E_Id : Ada.Strings.Unbounded.Unbounded_String;

         when Indirect_State =>
            I_Id          : Ada.Strings.Unbounded.Unbounded_String;
            I_Subprogram  : Subprogram_Information_Access;

         when Subprogram_State =>
            S_Id          : Ada.Strings.Unbounded.Unbounded_String;
            S_Prefix_Name : Ada.Strings.Unbounded.Unbounded_String;
            S_Linker_Name : Ada.Strings.Unbounded.Unbounded_String;
            S_Locations   : Subprogram_Location_Sets.Set;
            Is_Reference  : Boolean;
            Global_Usage  : Stack_Usage_Information;
            Local_Usage   : Stack_Usage_Information;
            Calls         : Subprogram_Information_Sets.Set;
            Unbounded     : Object_Information_Vectors.Vector;

         when Cycle_State =>
            Cycle : Subprogram_Information_Vectors.Vector;

         when Unbounded_State =>
            null;

         when Value_Kinds =>
            Value_Tag : Boolean := False;

            case Kind is
               when Boolean_Value_State =>
                  Boolean_Value : Boolean;

               when Integer_Value_State =>
                  Integer_Value : Integer;

               when String_Value_State =>
                  String_Value : Ada.Strings.Unbounded.Unbounded_String;

               when others =>
                  null;
            end case;
      end case;
   end record;

   package Parser_State_Vectors is
     new Ada.Containers.Vectors (Positive, Parser_State);

   package Unbounded_To_Subprogram_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Strings.Unbounded.Unbounded_String,
        GNATStack.Data_Model.Subprogram_Information_Access,
        Ada.Strings.Unbounded.Hash,
        Ada.Strings.Unbounded."=");

   type Reader is new Sax.Readers.Reader with record
      State          : Parser_State;
      Stack          : Parser_State_Vectors.Vector;
      Analysis       : Analysis_Information;
      Subprograms    : Unbounded_To_Subprogram_Maps.Map;
      Global_Section : Boolean;
      --  This flag indicates processing of child element of 'global' element
      --  because 'unboundedobjectset' elements must not be processed as
      --  child of 'global' element.
   end record;

   function Resolve_Or_Create
     (Self : not null access Reader;
      Id   : Ada.Strings.Unbounded.Unbounded_String)
      return Subprogram_Information_Access;
   --  Resolves subprogram information record or creates new one.

   procedure Push (Self : in out Reader);
   --  Saves parser's current state into the stack. Current state is resetted
   --  to None.

   procedure Pop (Self : in out Reader);
   --  Sets parser's state from the stack. Previous state is lost.

   procedure Analyze_accurate_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "accurate" element

   procedure Analyze_accurate_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "accurate" element

   procedure Analyze_callchain_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "callchain" element

   procedure Analyze_callchain_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "callchain" element

   procedure Analyze_cycle_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "cycle" element

   procedure Analyze_cycle_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "cycle" element

   procedure Analyze_cycleset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "cycleset" element

   procedure Analyze_cycleset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "cycleset" element

   procedure Analyze_entry_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "entry" element

   procedure Analyze_entry_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "entry" element

   procedure Analyze_entryset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "entryset" element

   procedure Analyze_entryset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "entryset" element

   procedure Analyze_external_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "external" element

   procedure Analyze_external_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "external" element

   procedure Analyze_externalset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "externalset" element

   procedure Analyze_externalset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "externalset" element

   procedure Analyze_file_Start_Tag
     (Self       : in out Reader'Class;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "file" element

   procedure Analyze_file_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "file" element

   procedure Analyze_global_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "global" element

   procedure Analyze_global_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "global" element

   procedure Analyze_globalstackusage_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyze start tag of "globalstackusage" element

   procedure Analyze_globalstackusage_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "globalstackusage" element

   procedure Analyze_indirect_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "indirect" element

   procedure Analyze_indirect_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "indirect" element

   procedure Analyze_indirectcall_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "indirectcall" element

   procedure Analyze_indirectcall_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "indirectcall" element

   procedure Analyze_indirectcallset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "indirectcallset" element

   procedure Analyze_indirectcallset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "indirectcallset" element

   procedure Analyze_indirectset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "indirectset" element

   procedure Analyze_indirectset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "indirectset" element

   procedure Analyze_line_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "line" element

   procedure Analyze_line_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "line" element

   procedure Analyze_localstackusage_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "localstackusage" element.

   procedure Analyze_localstackusage_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "localstackusage" element

   procedure Analyze_location_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "location" element

   procedure Analyze_location_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "location" element

   procedure Analyze_locationset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "locationset" element

   procedure Analyze_locationset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "locationset" element

   procedure Analyze_subprogram_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "subprogram" element

   procedure Analyze_subprogram_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "subprogram" element

   procedure Analyze_subprogramcalledset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyze start tag of "subprogramcalledset" element

   procedure Analyze_subprogramcalledset_End_Tag (Self : in out Reader);
   --  Analyzes end tag of "subprogramcalledset" element

   procedure Analyze_subprogramset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "subprogramset" element

   procedure Analyze_subprogramset_End_Tag (Self : in out Reader);
   --  Analyze end tag of "subprogramset" element

   procedure Analyze_unbounded_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "unbounded" element

   procedure Analyze_unbounded_End_Tag (Self : in out Reader);
   --  Analyze end tag of "unbounded" element

   procedure Analyze_unboundedobject_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "unboundedobject" element

   procedure Analyze_unboundedobject_End_Tag (Self : in out Reader);
   --  Analyze end tag of "unboundedobject" element

   procedure Analyze_unboundedobjectset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "unboundedobjectset" element

   procedure Analyze_unboundedobjectset_End_Tag (Self : in out Reader);
   --  Analyze end tag of "unboundedobjectset" element

   procedure Analyze_unboundedset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class);
   --  Analyzes start tag of "unboundedset" element

   procedure Analyze_unboundedset_End_Tag (Self : in out Reader);
   --  Analyze end tag of "unboundedset" element

   --  Overrided subprogram

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");

   overriding procedure Characters
     (Self : in out Reader;
      Text : Unicode.CES.Byte_Sequence);

   overriding procedure End_Document (Self : in out Reader);

end GNATStack.Readers;
