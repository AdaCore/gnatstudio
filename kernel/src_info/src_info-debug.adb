-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with Src_Info.ALI_Maps; use Src_Info.ALI_Maps;
with String_Utils;      use String_Utils;

package body Src_Info.Debug is

   --  ??? Add the exception handlers everywhere...

   use type Types.Time_Stamp_Type;

   procedure Print_Exception_Info
     (E : Exception_Occurrence; In_Subprogram : String);
   --  Print a message to stdout with the exception name, the exception
   --  message, and the name of the subprogram (where the exception was
   --  raised).

   procedure Dump_Pos_And_E_Kind (FL : File_Location; Kind : E_Kind);
   --  Print the line/column position separated by the character associated
   --  to the given E_Kind.

   procedure Dump_Pos_And_R_Kind (FL : File_Location; Kind : Reference_Kind);
   --  Print the line/column position separated by the character associated
   --  to the given Reference_Kind.

   procedure Dump_D_Line
     (Filename : String; Timestamp : Types.Time_Stamp_Type);
   --  Print a 'D' line with the filename and the timestamp.

   procedure Dump_D_Line (SF : Source_File; Timestamp : Types.Time_Stamp_Type);
   --  Print a 'D' line for a dependency to Source_File with the given
   --  Timestamp.

   --------------------------
   -- Print_Exception_Info --
   --------------------------

   procedure Print_Exception_Info
     (E : Exception_Occurrence; In_Subprogram : String) is
   begin
      --  Be extra cautious not to raise any exception here :-).
      Put_Line
        ("<unhandled exception '" & Exception_Name (E) & ':' &
         Exception_Message (E) & "' trapped in " & In_Subprogram);
      --  ??? Maybe use backtraces here, that will probably help avoiding
      --  ??? to pass in the subprgram name.
   end Print_Exception_Info;

   -------------------------
   -- Dump_Pos_And_E_Kind --
   -------------------------

   procedure Dump_Pos_And_E_Kind (FL : File_Location; Kind : E_Kind) is
   begin
      Put (Image (FL.Line) & E_Kind_To_Char (Kind) & Image (FL.Column));
   end Dump_Pos_And_E_Kind;

   -------------------------
   -- Dump_Pos_And_R_Kind --
   -------------------------

   procedure Dump_Pos_And_R_Kind (FL : File_Location; Kind : Reference_Kind) is
   begin
      Put
        (Image (FL.Line) & Reference_Kind_To_Char (Kind) & Image (FL.Column));
   end Dump_Pos_And_R_Kind;

   -----------------
   -- Dump_D_Line --
   -----------------

   procedure Dump_D_Line
     (Filename : String; Timestamp : Types.Time_Stamp_Type) is
   begin
      Put_Line ("D " & Filename & ' ' & String (Timestamp));
   end Dump_D_Line;

   procedure Dump_D_Line (SF : Source_File; Timestamp : Types.Time_Stamp_Type)
   is
      FI : File_Info_Ptr := Get_File_Info (SF);
   begin
      Put ("D " & FI.Source_Filename.all & ' ' & String (Timestamp));
      --  If the unit we're depending on is a subunit, print its name
      case SF.Part is
         when Unit_Spec | Unit_Body =>
            --  Nothing to print
            null;
         when Unit_Separate =>
            Put (' ' & FI.Unit_Name.all);
      end case;
      --  If Original_Filename is not null, print the location in the orignal
      --  filename where this source file comes from.
      if FI.Original_Filename /= null then
         Put (Positive'Image (FI.Original_Line) & ':');
         Put (FI.Original_Filename.all);
      end if;
      New_Line;
   end Dump_D_Line;

   ----------------------
   -- Dump_LI_File_Ptr --
   ----------------------

   procedure Dump_LI_File_Ptr (LIFP : LI_File_Ptr) is
   begin
      Dump_LI_File (LIFP.LI);
   end Dump_LI_File_Ptr;

   ----------------------------
   -- Dump_LI_File_From_Name --
   ----------------------------

   procedure Dump_LI_File_From_Name
     (LIFL : LI_File_List;
      Unit_Name : String)
   is
   begin
      null;
      --  ??? Will be implemented later...
   end Dump_LI_File_From_Name;

   ----------
   -- Dump --
   ----------

   procedure Dump (LIFL : LI_File_List) is
   begin
      null;
      --  ??? Will be implemented later...
   end Dump;

   ----------------------
   -- Dump_Source_File --
   ----------------------

   procedure Dump_Source_File (SF : Source_File) is
   begin
      Put (Get_Source_Filename (SF));
   exception
      when E : others =>
         Print_Exception_Info (E, "Dump_Source_File");
   end Dump_Source_File;

   ------------------------
   -- Dump_File_Location --
   ------------------------

   procedure Dump_File_Location (FL : File_Location) is
   begin
      Dump_Source_File (FL.File);
      Put (':' & Image (FL.Line) & ':' & Image (FL.Column));
   end Dump_File_Location;

   ----------------------
   -- Dump_E_Reference --
   ----------------------

   procedure Dump_E_Reference (ER : E_Reference) is
   begin
      Dump_Pos_And_R_Kind (ER.Location, ER.Kind);
   end Dump_E_Reference;

   ---------------------------
   -- Dump_E_Reference_List --
   ---------------------------

   procedure Dump_E_Reference_List (ERL : E_Reference_List)
   is
      Current_File : Source_File :=
        (LI => null, Part => Unit_Spec, Unit_Name => null);
      --  We print the filename associated to the location only
      --  for the first reference associated to that file. All following
      --  reference locations for this file are printed without this
      --  information. This avoids redundancy and follows the GNAT ALI file
      --  syntax. This is why we maintain this Current_File variable. The
      --  initialization value is chosen so that the filename will always
      --  be printed for the first reference.
      Current_Reference : E_Reference_List := ERL;
   begin
      while Current_Reference /= null loop
         Put (' ');
         if Current_Reference.Value.Location.File /= Current_File then
            Current_File := Current_Reference.Value.Location.File;
            Dump_Source_File (Current_File);
            Put ('|');
         end if;
         Dump_E_Reference (Current_Reference.Value);

         Current_Reference := Current_Reference.Next;
      end loop;
   end Dump_E_Reference_List;

   ------------------------
   -- Dump_E_Declaration --
   ------------------------

   procedure Dump_E_Declaration (ED : E_Declaration) is
   begin
      --  Dump declaration position, kind, scope and name
      Dump_Pos_And_E_Kind (ED.Location, ED.Kind);
      Put (E_Scope_To_Char (ED.Scope) & ED.Name.all & ' ');
      --  If there is a parent declaration, print it as well
      if Is_File_Location (ED.Parent_Location) then
         Put ('<');
         if ED.Parent_Location.File /= ED.Location.File then
            Dump_Source_File (ED.Parent_Location.File);
            Put ('|');
         end if;
         Dump_Pos_And_E_Kind (ED.Parent_Location, ED.Parent_Kind);
         Put ('>');
      end if;
      --  If there is an end of scope reference, print it too.
      if Is_File_Location (ED.End_Of_Scope.Location) then
         Put (" End=");
         Dump_Pos_And_R_Kind (ED.End_Of_Scope.Location, ED.End_Of_Scope.Kind);
      end if;
   end Dump_E_Declaration;

   -----------------------------
   -- Dump_E_Declaration_Info --
   -----------------------------

   procedure Dump_E_Declaration_Info (EDI : E_Declaration_Info) is
   begin
      Dump_E_Declaration (EDI.Declaration);
      Dump_E_Reference_List (EDI.References);
   end Dump_E_Declaration_Info;

   ----------------------------------
   -- Dump_E_Declaration_Info_List --
   ----------------------------------

   procedure Dump_E_Declaration_Info_List (EDIL : E_Declaration_Info_List) is
      Current_Decl_Info : E_Declaration_Info_List := EDIL;
   begin
      while Current_Decl_Info /= null loop
         Dump_E_Declaration_Info (Current_Decl_Info.Value);
         New_Line;
         Current_Decl_Info := Current_Decl_Info.Next;
      end loop;
   end Dump_E_Declaration_Info_List;

   --------------------
   -- Dump_File_Info --
   --------------------

   procedure Dump_File_Info (FI : File_Info; ALI_Format : Boolean := True) is
   begin
      if FI.Declarations = null then
         --  No need to generate the X line and the associated Xrefs, since
         --  there are no Xrefs...
         return;
      end if;

      Put ("X " & FI.Source_Filename.all);
      if not ALI_Format then
         Put (' ' & FI.Unit_Name.all);
         if FI.Original_Filename /= null then
            Put
              (' ' & FI.Original_Filename.all &
               ':' & Image (FI.Original_Line));
         end if;
         if FI.File_Timestamp /= Types.Empty_Time_Stamp then
            Put (' ' & String (FI.File_Timestamp));
         end if;
      end if;
      New_Line;
      Dump_E_Declaration_Info_List (FI.Declarations);
   end Dump_File_Info;

   -------------------------------
   -- Dump_Dependency_File_Info --
   -------------------------------

   procedure Dump_Dependency_File_Info
     (DFI        : Dependency_File_Info;
      ALI_Format : Boolean := True) is
   begin
      if DFI.Declarations = null then
         --  No need to generate the X line and the associated Xrefs, since
         --  there are no Xrefs...
         return;
      end if;

      Put ("X ");
      Dump_Source_File (DFI.File);
      if not ALI_Format then
         if DFI.Dep_Info.Depends_From_Spec then
            Put (" (Spec)");
         end if;
         if DFI.Dep_Info.Depends_From_Body then
            Put (" (Body)");
         end if;
         if DFI.File_Timestamp /= Types.Empty_Time_Stamp then
            Put (' ' & String (DFI.File_Timestamp));
         end if;
      end if;
      New_Line;
      Dump_E_Declaration_Info_List (DFI.Declarations);
   end Dump_Dependency_File_Info;

   ------------------------------------
   -- Dump_Dependency_File_Info_List --
   ------------------------------------

   procedure Dump_Dependency_File_Info_List
     (DFIL       : Dependency_File_Info_List;
      ALI_Format : Boolean := True)
   is
      Current_Dep_File_Info : Dependency_File_Info_List := DFIL;
   begin
      while Current_Dep_File_Info /= null loop
         Dump_Dependency_File_Info (Current_Dep_File_Info.Value, ALI_Format);
         Current_Dep_File_Info := Current_Dep_File_Info.Next;
      end loop;
   end Dump_Dependency_File_Info_List;

   -----------------------------
   -- Dump_Dependency_Section --
   -----------------------------

   procedure Dump_Unit_Dependency_Section
     (LIF   : LI_File; Part : Unit_Part)
   is
      Current_Dep_File_Info : Dependency_File_Info_List :=
        LIF.Dependencies_Info;
   begin
      --  First print the 'U' line
      Put ("U ");
      case Part is
         when Unit_Spec =>
            Put (LIF.Spec_Info.Unit_Name.all);
            Put ("%s " & LIF.Spec_Info.Source_Filename.all);
         when Unit_Body =>
            Put (LIF.Body_Info.Unit_Name.all);
            Put ("%b " & LIF.Body_Info.Source_Filename.all);
         when Unit_Separate =>
            --  This is not allowed.
            Put_Line ("<programing error, invalid Unit_Part!>");
            return;
      end case;
      New_Line;

      --  Now print the 'W' lines
      while Current_Dep_File_Info /= null loop
         if (Part = Unit_Spec
             and then Current_Dep_File_Info.Value.Dep_Info.Depends_From_Spec)
           or else
            (Part = Unit_Body
             and then Current_Dep_File_Info.Value.Dep_Info.Depends_From_Body)
         then
            Put ("W ");
            Put (Get_File_Info
               (Current_Dep_File_Info.Value.File).Unit_Name.all);
            New_Line;
         end if;
         Current_Dep_File_Info := Current_Dep_File_Info.Next;
      end loop;
   end Dump_Unit_Dependency_Section;

   ----------------------------------
   -- Dump_File_Dependency_Section --
   ----------------------------------

   procedure Dump_File_Dependency_Section (LIF : LI_File)
   is
      Current_Dependency_File : Dependency_File_Info_List :=
        LIF.Dependencies_Info;
   begin
      --  Generate the line for the spec if applicable
      if LIF.Spec_Info /= null then
         Dump_D_Line
           (LIF.Spec_Info.Source_Filename.all,
            LIF.Spec_Info.File_Timestamp);
      end if;

      --  Generate the line for the body if applicable
      if LIF.Body_Info /= null then
         Dump_D_Line
           (LIF.Body_Info.Source_Filename.all,
            LIF.Body_Info.File_Timestamp);
      end if;

      --  Generate all the other lines...
      while Current_Dependency_File /= null loop
         Dump_D_Line
           (Current_Dependency_File.Value.File,
            Current_Dependency_File.Value.File_Timestamp);
         Current_Dependency_File := Current_Dependency_File.Next;
      end loop;
   end Dump_File_Dependency_Section;

   --------------------
   -- Dump_LI_File --
   --------------------

   procedure Dump_LI_File (LIF : LI_File) is
   begin
      --  If the associated unit information file (the ALI file in the
      --  case of Ada files compiled with GNAT) has not been parsed,
      --  which means that this LI_File entry is just a stub, then say
      --  so, and then abort since there is no more information we can
      --  print at this stage.
      if not LIF.Parsed then
         Put_Line ("Unit_Name: " & LIF.LI_Filename.all & "<not parsed!>");
         return;
      end if;

      --  Generate the 'P' line
      Put ('P');
      if LIF.Compilation_Errors_Found then
         Put (" CE");
      end if;
      New_Line;

      --  Jump one line
      New_Line;

      --  Generate the unit dependency sections
      Dump_Unit_Dependency_Section (LIF, Unit_Body);
      New_Line;
      Dump_Unit_Dependency_Section (LIF, Unit_Spec);
      New_Line;

      --  Generate the file dependency section
      Dump_File_Dependency_Section (LIF);
      New_Line;

      --  Generate the references information
      if LIF.Spec_Info /= null then
         Dump_File_Info (LIF.Spec_Info.all);
      end if;
      if LIF.Body_Info /= null then
         Dump_File_Info (LIF.Body_Info.all);
      end if;
      Dump_Dependency_File_Info_List (LIF.Dependencies_Info);

   end Dump_LI_File;

end Src_Info.Debug;

