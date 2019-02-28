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

with GNATCOLL.Traces; use GNATCOLL.Traces;

package body GNATStack.Readers is
   Me : constant Trace_Handle := Create ("GPS.CODE_ANALYSIS.GNATSTACK");

   use Ada.Strings.Unbounded;
   use Subprogram_Information_Maps;
   use Subprogram_Location_Sets;

   ------------------------------
   -- Analyze_accurate_End_Tag --
   ------------------------------

   procedure Analyze_accurate_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Boolean_Value_State);

      Value : constant Boolean := Self.State.Boolean_Value;

   begin
      Self.Pop;
      Self.Analysis.Accurate := Value;
   end Analyze_accurate_End_Tag;

   --------------------------------
   -- Analyze_accurate_Start_Tag --
   --------------------------------

   procedure Analyze_accurate_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.Stack.Is_Empty);
      pragma Assert (Attributes.Get_Value ("type") = "booleans");

   begin
      Self.Push;
      Self.State := (Kind => Boolean_Value_State, others => <>);
   end Analyze_accurate_Start_Tag;

   -------------------------------
   -- Analyze_callchain_End_Tag --
   -------------------------------

   procedure Analyze_callchain_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Entry_State);

   begin
      null;
   end Analyze_callchain_End_Tag;

   ---------------------------------
   -- Analyze_callchain_Start_Tag --
   ---------------------------------

   procedure Analyze_callchain_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.State.Kind = Entry_State);

   begin
      null;
   end Analyze_callchain_Start_Tag;

   ---------------------------
   -- Analyze_cycle_End_Tag --
   ---------------------------

   procedure Analyze_cycle_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Cycle_State);

      Cycle : constant Subprogram_Information_Vectors.Vector :=
                Self.State.Cycle;

   begin
      Self.Pop;
      Self.Analysis.Cycle_Set.Append (Cycle);
   end Analyze_cycle_End_Tag;

   -----------------------------
   -- Analyze_cycle_Start_Tag --
   -----------------------------

   procedure Analyze_cycle_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.State.Kind = Cycle_Set_State);

   begin
      Self.Push;
      Self.State := (Kind => Cycle_State, others => <>);
   end Analyze_cycle_Start_Tag;

   ------------------------------
   -- Analyze_cycleset_End_Tag --
   ------------------------------

   procedure Analyze_cycleset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Cycle_Set_State);
      pragma Assert (Self.Stack.Is_Empty);
      --  "cycleset" is child of "global" element

   begin
      null;
   end Analyze_cycleset_End_Tag;

   --------------------------------
   -- Analyze_cycleset_Start_Tag --
   --------------------------------

   procedure Analyze_cycleset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "cycleset" is top level element

   begin
      Self.State := (Kind => Cycle_Set_State);
   end Analyze_cycleset_Start_Tag;

   ---------------------------
   -- Analyze_entry_End_Tag --
   ---------------------------

   procedure Analyze_entry_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Entry_State);

      Subprogram : constant Subprogram_Information_Access :=
                     Self.Resolve_Or_Create (Self.State.C_Id);
      Value      : constant Subprogram_Information_Vectors.Vector :=
                     Self.State.Chain;
      Usage      : constant Stack_Usage_Information :=
                     Self.State.Entry_Usage;

   begin
      Self.Pop;
      Self.Analysis.Entry_Set.Insert (Subprogram);
      Subprogram.Chain := Value;
      Subprogram.Entry_Usage := Usage;
   end Analyze_entry_End_Tag;

   -----------------------------
   -- Analyze_entry_Start_Tag --
   -----------------------------

   procedure Analyze_entry_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.State.Kind = Entry_Set_State);
      pragma Assert (Self.Stack.Is_Empty);

   begin
      Self.Push;
      Self.State :=
        (Kind   => Entry_State,
         C_Id   => To_Unbounded_String (Attributes.Get_Value ("id")),
         others => <>);
   end Analyze_entry_Start_Tag;

   ------------------------------
   -- Analyze_entryset_End_Tag --
   ------------------------------

   procedure Analyze_entryset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Entry_Set_State);
      pragma Assert (Self.Stack.Is_Empty);
      --  "entryset" is top level element

   begin
      null;
   end Analyze_entryset_End_Tag;

   --------------------------------
   -- Analyze_entryset_Start_Tag --
   --------------------------------

   procedure Analyze_entryset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "entryset" is top level element

   begin
      Self.State := (Kind => Entry_Set_State);
   end Analyze_entryset_Start_Tag;

   ------------------------------
   -- Analyze_external_End_Tag --
   ------------------------------

   procedure Analyze_external_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = External_State);

      Subprogram : constant Subprogram_Information_Access :=
                     Self.Resolve_Or_Create (Self.State.E_Id);

   begin
      Self.Pop;
      Subprogram.Is_External := True;
   end Analyze_external_End_Tag;

   --------------------------------
   -- Analyze_external_Start_Tag --
   --------------------------------

   procedure Analyze_external_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.State.Kind = External_Set_State);

   begin
      Self.Push;
      Self.State :=
        (Kind   => External_State,
         E_Id   => To_Unbounded_String (Attributes.Get_Value ("id")));
   end Analyze_external_Start_Tag;

   ---------------------------------
   -- Analyze_externalset_End_Tag --
   ---------------------------------

   procedure Analyze_externalset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = External_Set_State);
      pragma Assert (Self.Stack.Is_Empty);
      --  "externalset" is top level element

   begin
      null;
   end Analyze_externalset_End_Tag;

   -----------------------------------
   -- Analyze_externalset_Start_Tag --
   -----------------------------------

   procedure Analyze_externalset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "externalset" is top level element

   begin
      Self.State := (Kind => External_Set_State);
   end Analyze_externalset_Start_Tag;

   --------------------------
   -- Analyze_file_End_Tag --
   --------------------------

   procedure Analyze_file_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = String_Value_State);

      Value : constant Unbounded_String := Self.State.String_Value;

   begin
      Self.Pop;

      if Self.State.Kind = Indirect_Call_State then
         Self.State.Indirect.File := Value;
      end if;
   end Analyze_file_End_Tag;

   ----------------------------
   -- Analyze_file_Start_Tag --
   ----------------------------

   procedure Analyze_file_Start_Tag
     (Self       : in out Reader'Class;
      Attributes : Sax.Attributes.Attributes'Class'Class)
   is
      pragma Assert (Self.State.Kind = Indirect_Call_State);
      pragma Assert (Attributes.Get_Value ("type") = "strings");

   begin
      Self.Push;
      Self.State := (Kind => String_Value_State, others => <>);
   end Analyze_file_Start_Tag;

   ----------------------------
   -- Analyze_global_End_Tag --
   ----------------------------

   procedure Analyze_global_End_Tag (Self : in out Reader) is
      pragma Assert (Self.Stack.Is_Empty);
      --  "global" is top level element

   begin
      Self.Global_Section := False;
   end Analyze_global_End_Tag;

   ------------------------------
   -- Analyze_global_Start_Tag --
   ------------------------------

   procedure Analyze_global_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "global" is top level element

   begin
      Self.Global_Section := True;
   end Analyze_global_Start_Tag;

   --------------------------------------
   -- Analyze_globalstackusage_End_Tag --
   --------------------------------------

   procedure Analyze_globalstackusage_End_Tag (Self : in out Reader) is
   begin
      null;
   end Analyze_globalstackusage_End_Tag;

   ----------------------------------------
   -- Analyze_globalstackusage_Start_Tag --
   ----------------------------------------

   procedure Analyze_globalstackusage_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.State.Kind = Subprogram_State);
      pragma Assert (not Self.State.Is_Reference);

      Value : Stack_Usage_Information;

   begin
      Value.Size := Integer'Value (Attributes.Get_Value ("size"));
      Value.Qualifier :=
        To_Unbounded_String (Attributes.Get_Value ("qualifier"));
      Self.State.Global_Usage := Value;
   end Analyze_globalstackusage_Start_Tag;

   ------------------------------
   -- Analyze_indirect_End_Tag --
   ------------------------------

   procedure Analyze_indirect_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Indirect_State);

      Subprogram : constant Subprogram_Information_Access :=
                     Self.State.I_Subprogram;

   begin
      Self.Pop;
      Self.Analysis.Indirect_Set.Insert (Subprogram);
   end Analyze_indirect_End_Tag;

   --------------------------------
   -- Analyze_indirect_Start_Tag --
   --------------------------------

   procedure Analyze_indirect_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.State.Kind = Indirect_Set_State);

   begin
      Self.Push;
      Self.State :=
        (Kind   => Indirect_State,
         I_Id   => To_Unbounded_String (Attributes.Get_Value ("id")),
         others => <>);
   end Analyze_indirect_Start_Tag;

   ----------------------------------
   -- Analyze_indirectcall_End_Tag --
   ----------------------------------

   procedure Analyze_indirectcall_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Indirect_Call_State);

      Value : constant Indirect_Call_Information := Self.State.Indirect;

   begin
      Self.Pop;
      Self.State.I_Subprogram.Indirects.Append (Value);
   end Analyze_indirectcall_End_Tag;

   ------------------------------------
   -- Analyze_indirectcall_Start_Tag --
   ------------------------------------

   procedure Analyze_indirectcall_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.State.Kind = Indirect_State);

   begin
      Self.Push;
      Self.State := (Kind => Indirect_Call_State, others => <>);
   end Analyze_indirectcall_Start_Tag;

   -------------------------------------
   -- Analyze_indirectcallset_End_Tag --
   -------------------------------------

   procedure Analyze_indirectcallset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Indirect_State);

   begin
      null;
   end Analyze_indirectcallset_End_Tag;

   ---------------------------------------
   -- Analyze_indirectcallset_Start_Tag --
   ---------------------------------------

   procedure Analyze_indirectcallset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);
      pragma Assert (Self.State.Kind = Indirect_State);

   begin
      Self.State.I_Subprogram :=
        Self.Resolve_Or_Create (Self.State.I_Id);
   end Analyze_indirectcallset_Start_Tag;

   ---------------------------------
   -- Analyze_indirectset_End_Tag --
   ---------------------------------

   procedure Analyze_indirectset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Indirect_Set_State);
      pragma Assert (Self.Stack.Is_Empty);
      --  "indirectset" is child of "global" element

   begin
      null;
   end Analyze_indirectset_End_Tag;

   -----------------------------------
   -- Analyze_indirectset_Start_Tag --
   -----------------------------------

   procedure Analyze_indirectset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "indirectset" is child of "global" element

   begin
      Self.State := (Kind => Indirect_Set_State);
   end Analyze_indirectset_Start_Tag;

   --------------------------
   -- Analyze_line_End_Tag --
   --------------------------

   procedure Analyze_line_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Integer_Value_State);

      Value : constant Integer := Self.State.Integer_Value;

   begin
      Self.Pop;

      if Self.State.Kind = Indirect_Call_State then
         Self.State.Indirect.Line := Value;
      end if;
   end Analyze_line_End_Tag;

   ----------------------------
   -- Analyze_line_Start_Tag --
   ----------------------------

   procedure Analyze_line_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert
        (Self.State.Kind = Indirect_Call_State);
      pragma Assert (Attributes.Get_Value ("type") = "integers");

   begin
      Self.Push;
      Self.State := (Kind => Integer_Value_State, others => <>);
   end Analyze_line_Start_Tag;

   -------------------------------------
   -- Analyze_localstackusage_End_Tag --
   -------------------------------------

   procedure Analyze_localstackusage_End_Tag (Self : in out Reader) is
   begin
      null;
   end Analyze_localstackusage_End_Tag;

   ---------------------------------------
   -- Analyze_localstackusage_Start_Tag --
   ---------------------------------------

   procedure Analyze_localstackusage_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert
        (Self.State.Kind = Entry_State
           or else Self.State.Kind = Subprogram_State);
      pragma Assert
        (Self.State.Kind /= Subprogram_State
           or else not Self.State.Is_Reference);

      Value : Stack_Usage_Information;

   begin
      Value.Size := Integer'Value (Attributes.Get_Value ("size"));
      Value.Qualifier :=
        To_Unbounded_String (Attributes.Get_Value ("qualifier"));

      if Self.State.Kind = Entry_State then
         Self.State.Entry_Usage := Value;

      elsif Self.State.Kind = Subprogram_State then
         Self.State.Local_Usage := Value;
      end if;
   end Analyze_localstackusage_Start_Tag;

   ------------------------------
   -- Analyze_location_End_Tag --
   ------------------------------

   procedure Analyze_location_End_Tag (Self : in out Reader) is
   begin
      null;
   end Analyze_location_End_Tag;

   --------------------------------
   -- Analyze_location_Start_Tag --
   --------------------------------

   procedure Analyze_location_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert (Self.State.Kind = Location_Set_State);

      Location : Subprogram_Location;

   begin
      if Attributes.Get_Index ("subprogramname") /= -1 then
         Location.Name :=
           To_Unbounded_String (Attributes.Get_Value ("subprogramname"));
      end if;

      if Attributes.Get_Index ("file") /= -1 then
         Location.File := To_Unbounded_String (Attributes.Get_Value ("file"));
         Location.Line := Integer'Value (Attributes.Get_Value ("line"));
         Location.Column := Integer'Value (Attributes.Get_Value ("column"));
      end if;

      Self.State.Location_Set.Insert (Location);
   end Analyze_location_Start_Tag;

   ---------------------------------
   -- Analyze_locationset_End_Tag --
   ---------------------------------

   procedure Analyze_locationset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Location_Set_State);

      Value : constant Subprogram_Location_Sets.Set :=
                Self.State.Location_Set;

   begin
      Self.Pop;

      if Self.State.Kind = Subprogram_State then
         Self.State.S_Locations := Value;
      end if;
   end Analyze_locationset_End_Tag;

   -----------------------------------
   -- Analyze_locationset_Start_Tag --
   -----------------------------------

   procedure Analyze_locationset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.State.Kind = Subprogram_State);

   begin
      Self.Push;
      Self.State := (Kind => Location_Set_State, others => <>);
   end Analyze_locationset_Start_Tag;

   --------------------------------
   -- Analyze_subprogram_End_Tag --
   --------------------------------

   procedure Analyze_subprogram_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Subprogram_State);

      Prefix_Name : constant Unbounded_String := Self.State.S_Prefix_Name;
      Linker_Name : constant Unbounded_String := Self.State.S_Linker_Name;
      Locations   : constant Subprogram_Location_Sets.Set
        := Self.State.S_Locations;

      Value      : constant Parser_State := Self.State;
      Subprogram : Subprogram_Information_Access;

   begin
      Self.Pop;
      Subprogram := Self.Resolve_Or_Create (Value.S_Id);

      if Prefix_Name /= "" then
         Subprogram.Identifier.Prefix_Name := Prefix_Name;
      end if;

      if Linker_Name /= "" then
         Subprogram.Identifier.Linker_Name := Linker_Name;
      end if;

      if not Locations.Is_Empty then
         Subprogram.Identifier.Locations := Locations;
      end if;

      if Self.State.Kind = Entry_State then
         --  Insert subprogram into the chain

         Self.State.Chain.Append (Subprogram);

      elsif Self.State.Kind = Subprogram_Set_State then
         --  Fill data

         Subprogram.Global_Usage := Value.Global_Usage;
         Subprogram.Local_Usage := Value.Local_Usage;
         Subprogram.Calls := Value.Calls;
         Subprogram.Unbounded := Value.Unbounded;

         if not Subprogram.Unbounded.Is_Empty then
            Self.Analysis.Unbounded_Set.Insert (Subprogram);
         end if;

      elsif Self.State.Kind = Subprogram_Called_Set_State then
         --  Insert subprogram into the set when it is not in the set

         if not Self.State.Called_Set.Contains (Subprogram) then
            Self.State.Called_Set.Insert (Subprogram);
         end if;

      elsif Self.State.Kind = Cycle_State then
         --  Insert subprogram into the chain

         Self.State.Cycle.Append (Subprogram);
      end if;
   end Analyze_subprogram_End_Tag;

   ----------------------------------
   -- Analyze_subprogram_Start_Tag --
   ----------------------------------

   procedure Analyze_subprogram_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert
        (Self.State.Kind = Entry_State
           or else Self.State.Kind = Cycle_State
           or else Self.State.Kind = Subprogram_Called_Set_State
           or else Self.State.Kind = Subprogram_Set_State);

      Is_Reference : constant Boolean :=
                       Self.State.Kind /= Subprogram_Set_State;

   begin
      Self.Push;
      Self.State :=
        (Kind         => Subprogram_State,
         S_Id         => To_Unbounded_String (Attributes.Get_Value ("id")),
         Is_Reference => Is_Reference,
         others       => <>);

      if Attributes.Get_Index ("prefixname") /= -1 then
         Self.State.S_Prefix_Name :=
           To_Unbounded_String (Attributes.Get_Value ("prefixname"));
      end if;

      if Attributes.Get_Index ("linkername") /= -1 then
         Self.State.S_Linker_Name :=
           To_Unbounded_String (Attributes.Get_Value ("linkername"));
      end if;
   end Analyze_subprogram_Start_Tag;

   -----------------------------------------
   -- Analyze_subprogramcalledset_End_Tag --
   -----------------------------------------

   procedure Analyze_subprogramcalledset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Subprogram_Called_Set_State);

      Set : constant Subprogram_Information_Sets.Set := Self.State.Called_Set;

   begin
      Self.Pop;
      Self.State.Calls := Set;
   end Analyze_subprogramcalledset_End_Tag;

   -------------------------------------------
   -- Analyze_subprogramcalledset_Start_Tag --
   -------------------------------------------

   procedure Analyze_subprogramcalledset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.State.Kind = Subprogram_State);

   begin
      Self.Push;
      Self.State := (Kind => Subprogram_Called_Set_State, others => <>);
   end Analyze_subprogramcalledset_Start_Tag;

   -----------------------------------
   -- Analyze_subprogramset_End_Tag --
   -----------------------------------

   procedure Analyze_subprogramset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Subprogram_Set_State);

      pragma Assert (Self.Stack.Is_Empty);
      --  "subprogramset" is top level element

   begin
      null;
   end Analyze_subprogramset_End_Tag;

   -------------------------------------
   -- Analyze_subprogramset_Start_Tag --
   -------------------------------------

   procedure Analyze_subprogramset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "subprogramset" is top level element

   begin
      Self.State := (Kind => Subprogram_Set_State);
   end Analyze_subprogramset_Start_Tag;

   -------------------------------
   -- Analyze_unbounded_End_Tag --
   -------------------------------

   procedure Analyze_unbounded_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Unbounded_State);

   begin
      Self.Pop;
   end Analyze_unbounded_End_Tag;

   ---------------------------------
   -- Analyze_unbounded_Start_Tag --
   ---------------------------------

   procedure Analyze_unbounded_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.State.Kind = Unbounded_Set_State);

   begin
      Self.Push;
      Self.State := (Kind => Unbounded_State);
   end Analyze_unbounded_Start_Tag;

   -------------------------------------
   -- Analyze_unboundedobject_End_Tag --
   -------------------------------------

   procedure Analyze_unboundedobject_End_Tag (Self : in out Reader) is
   begin
      null;
   end Analyze_unboundedobject_End_Tag;

   ---------------------------------------
   -- Analyze_unboundedobject_Start_Tag --
   ---------------------------------------

   procedure Analyze_unboundedobject_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert
        (Self.State.Kind = Unbounded_State
           or else Self.State.Kind = Subprogram_State);

      Value : Object_Information;

   begin
      Value.Name := To_Unbounded_String (Attributes.Get_Value ("object"));
      Value.File := To_Unbounded_String (Attributes.Get_Value ("file"));
      Value.Line := Integer'Value (Attributes.Get_Value ("line"));
      Value.Column := Integer'Value (Attributes.Get_Value ("column"));

      if not Self.Global_Section then
         pragma Assert (Self.State.Kind = Subprogram_State);

         Self.State.Unbounded.Append (Value);
      end if;
   end Analyze_unboundedobject_Start_Tag;

   ----------------------------------------
   -- Analyze_unboundedobjectset_End_Tag --
   ----------------------------------------

   procedure Analyze_unboundedobjectset_End_Tag (Self : in out Reader) is
      pragma Assert
        (Self.State.Kind = Unbounded_State
           or else Self.State.Kind = Subprogram_State);
      --  "unboundedobjectset" element is ignored, parser's state is unchanged
      --  to allow direct access to members of Unbounded_Set state.

   begin
      null;
   end Analyze_unboundedobjectset_End_Tag;

   ------------------------------------------
   -- Analyze_unboundedobjectset_Start_Tag --
   ------------------------------------------

   procedure Analyze_unboundedobjectset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Assert
        (Self.State.Kind = Unbounded_State
           or else Self.State.Kind = Subprogram_State);

   begin
      --  "unboundedobjectset" element is ignored, parser's state is unchanged
      --  to allow direct access to members of Unbounded_Set state.

      null;
   end Analyze_unboundedobjectset_Start_Tag;

   ----------------------------------
   -- Analyze_unboundedset_End_Tag --
   ----------------------------------

   procedure Analyze_unboundedset_End_Tag (Self : in out Reader) is
      pragma Assert (Self.State.Kind = Unbounded_Set_State);
      pragma Assert (Self.Stack.Is_Empty);
      --  "unboundedset" is top level element

   begin
      null;
   end Analyze_unboundedset_End_Tag;

   ------------------------------------
   -- Analyze_unboundedset_Start_Tag --
   ------------------------------------

   procedure Analyze_unboundedset_Start_Tag
     (Self       : in out Reader;
      Attributes : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Attributes);

      pragma Assert (Self.Stack.Is_Empty);
      --  "unboundedset" is child of "global" element

   begin
      Self.State := (Kind => Unbounded_Set_State);
   end Analyze_unboundedset_Start_Tag;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Reader;
      Text : Unicode.CES.Byte_Sequence) is
   begin
      if Self.State.Kind in Value_Kinds then
         case Self.State.Kind is
            when Boolean_Value_State =>
               Self.State.Boolean_Value := Boolean'Value (Text);

            when Integer_Value_State =>
               Self.State.Integer_Value := Integer'Value (Text);

            when String_Value_State =>
               Append (Self.State.String_Value, Text);

            when others =>
               raise Program_Error;
               --  Must never be happen.
         end case;
      end if;
   end Characters;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document (Self : in out Reader) is

      procedure Compute_Subprogram_Map_And_External_Set
        (Position : Subprogram_Information_Sets.Cursor);

      ---------------------------------------------
      -- Compute_Subprogram_Map_And_External_Set --
      ---------------------------------------------

      procedure Compute_Subprogram_Map_And_External_Set
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Subprogram : constant Subprogram_Information_Access :=
                        Subprogram_Information_Sets.Element (Position);

      begin
         if Subprogram.Is_External then
            Self.Analysis.External_Set.Insert (Subprogram);
         end if;

         if Subprogram.Identifier.Linker_Name /= "" then
            Self.Analysis.Subprogram_Map.Insert
              (Subprogram.Identifier.Linker_Name, Subprogram);
            --  ??? Adding of subprogram can fail for some unknown reasons,
            --  need to be investiagtes. There are three reasons to be
            --  investigated first: indirect calls, dispatching calls, and
            --  calls inside generic instantication.
            --
            --  After changes for JB22-004 this situation should disapper.
         end if;

      exception
         when X : Constraint_Error =>
            Trace (Me, X);
      end Compute_Subprogram_Map_And_External_Set;

   begin
      --  Postprocess data

      Self.Analysis.Subprogram_Set.Iterate
        (Compute_Subprogram_Map_And_External_Set'Access);
   end End_Document;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI, Qname);

   begin
      if Local_Name = "GNATstack_Information" then
         null;

         pragma Assert (Self.Stack.Is_Empty);
         --  "GNATstack_Information" is root element

      elsif Local_Name = "accurate" then
         Self.Analyze_accurate_End_Tag;

      elsif Local_Name = "callchain" then
         Self.Analyze_callchain_End_Tag;

      elsif Local_Name = "cycle" then
         Self.Analyze_cycle_End_Tag;

      elsif Local_Name = "cycleset" then
         Self.Analyze_cycleset_End_Tag;

      elsif Local_Name = "entry" then
         Self.Analyze_entry_End_Tag;

      elsif Local_Name = "entryset" then
         Self.Analyze_entryset_End_Tag;

      elsif Local_Name = "external" then
         Self.Analyze_external_End_Tag;

      elsif Local_Name = "externalset" then
         Self.Analyze_externalset_End_Tag;

      elsif Local_Name = "file" then
         Self.Analyze_file_End_Tag;

      elsif Local_Name = "global" then
         Self.Analyze_global_End_Tag;

      elsif Local_Name = "globalstackusage" then
         Self.Analyze_globalstackusage_End_Tag;

      elsif Local_Name = "indirect" then
         Self.Analyze_indirect_End_Tag;

      elsif Local_Name = "indirectcall" then
         Self.Analyze_indirectcall_End_Tag;

      elsif Local_Name = "indirectcallset" then
         Self.Analyze_indirectcallset_End_Tag;

      elsif Local_Name = "indirectset" then
         Self.Analyze_indirectset_End_Tag;

      elsif Local_Name = "line" then
         Self.Analyze_line_End_Tag;

      elsif Local_Name = "localstackusage" then
         Self.Analyze_localstackusage_End_Tag;

      elsif Local_Name = "location" then
         Self.Analyze_location_End_Tag;

      elsif Local_Name = "locationset" then
         Self.Analyze_locationset_End_Tag;

      elsif Local_Name = "subprogram" then
         Self.Analyze_subprogram_End_Tag;

      elsif Local_Name = "subprogramcalledset" then
         Self.Analyze_subprogramcalledset_End_Tag;

      elsif Local_Name = "subprogramset" then
         Self.Analyze_subprogramset_End_Tag;

      elsif Local_Name = "unbounded" then
         Self.Analyze_unbounded_End_Tag;

      elsif Local_Name = "unboundedobject" then
         Self.Analyze_unboundedobject_End_Tag;

      elsif Local_Name = "unboundedobjectset" then
         Self.Analyze_unboundedobjectset_End_Tag;

      elsif Local_Name = "unboundedset" then
         Self.Analyze_unboundedset_End_Tag;

      elsif Local_Name = "value" then
         null;

      else
         raise Program_Error;
      end if;
   end End_Element;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Self : Reader'Class) return Analysis_Information is
   begin
      return Self.Analysis;
   end Get_Data;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Reader) is
   begin
      Self.State := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (Self : in out Reader) is
   begin
      Self.Stack.Append (Self.State);
      Self.State := (Kind => None_State);
   end Push;

   -----------------------
   -- Resolve_Or_Create --
   -----------------------

   function Resolve_Or_Create
     (Self : not null access Reader;
      Id   : Ada.Strings.Unbounded.Unbounded_String)
      return Subprogram_Information_Access
   is
      use Unbounded_To_Subprogram_Maps;

      Position : constant Unbounded_To_Subprogram_Maps.Cursor :=
                   Self.Subprograms.Find (Id);
      Info     : Subprogram_Information_Access;

   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         Info :=
           new Subprogram_Information'
             (Id         => Id,
              others     => <>);
         Self.Analysis.Subprogram_Set.Insert (Info);
         Self.Subprograms.Insert (Id, Info);

         return Info;
      end if;
   end Resolve_Or_Create;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Qname);

   begin
      if Local_Name = "GNATstack_Information" then
         null;

      elsif Local_Name = "accurate" then
         Self.Analyze_accurate_Start_Tag (Atts);

      elsif Local_Name = "callchain" then
         Self.Analyze_callchain_Start_Tag (Atts);

      elsif Local_Name = "cycle" then
         Self.Analyze_cycle_Start_Tag (Atts);

      elsif Local_Name = "cycleset" then
         Self.Analyze_cycleset_Start_Tag (Atts);

      elsif Local_Name = "entry" then
         Self.Analyze_entry_Start_Tag (Atts);

      elsif Local_Name = "entryset" then
         Self.Analyze_entryset_Start_Tag (Atts);

      elsif Local_Name = "external" then
         Self.Analyze_external_Start_Tag (Atts);

      elsif Local_Name = "externalset" then
         Self.Analyze_externalset_Start_Tag (Atts);

      elsif Local_Name = "file" then
         Self.Analyze_file_Start_Tag (Atts);

      elsif Local_Name = "global" then
         Self.Analyze_global_Start_Tag (Atts);

      elsif Local_Name = "globalstackusage" then
         Self.Analyze_globalstackusage_Start_Tag (Atts);

      elsif Local_Name = "indirect" then
         Self.Analyze_indirect_Start_Tag (Atts);

      elsif Local_Name = "indirectcall" then
         Self.Analyze_indirectcall_Start_Tag (Atts);

      elsif Local_Name = "indirectcallset" then
         Self.Analyze_indirectcallset_Start_Tag (Atts);

      elsif Local_Name = "indirectset" then
         Self.Analyze_indirectset_Start_Tag (Atts);

      elsif Local_Name = "line" then
         Self.Analyze_line_Start_Tag (Atts);

      elsif Local_Name = "localstackusage" then
         Self.Analyze_localstackusage_Start_Tag (Atts);

      elsif Local_Name = "location" then
         Self.Analyze_location_Start_Tag (Atts);

      elsif Local_Name = "locationset" then
         Self.Analyze_locationset_Start_Tag (Atts);

      elsif Local_Name = "subprogram" then
         Self.Analyze_subprogram_Start_Tag (Atts);

      elsif Local_Name = "subprogramcalledset" then
         Self.Analyze_subprogramcalledset_Start_Tag (Atts);

      elsif Local_Name = "subprogramset" then
         Self.Analyze_subprogramset_Start_Tag (Atts);

      elsif Local_Name = "unbounded" then
         Self.Analyze_unbounded_Start_Tag (Atts);

      elsif Local_Name = "unboundedobject" then
         Self.Analyze_unboundedobject_Start_Tag (Atts);

      elsif Local_Name = "unboundedobjectset" then
         Self.Analyze_unboundedobjectset_Start_Tag (Atts);

      elsif Local_Name = "unboundedset" then
         Self.Analyze_unboundedset_Start_Tag (Atts);

      elsif Local_Name = "value" then
         pragma Assert (Self.State.Kind in Value_Kinds);
         pragma Assert (not Self.State.Value_Tag);

         Self.State.Value_Tag := True;

      else
         raise Program_Error with "Unexpected element '" & Local_Name & ''';
      end if;
   end Start_Element;

end GNATStack.Readers;
