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

with Ada.Strings.Fixed;            use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with Default_Preferences;          use Default_Preferences;
with GPS.Editors.Line_Information;
use GPS.Editors, GPS.Editors.Line_Information;
with GPS.Default_Styles;           use GPS.Default_Styles;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;

package body GNATStack.Module.Editors is

   use GNATCOLL.VFS;
   use GNATStack.Data_Model;
   use GNATStack.Data_Model.Object_Information_Vectors;
   use GNATStack.Data_Model.Subprogram_Information_Sets;
   use GNATStack.Data_Model.Subprogram_Location_Sets;

   procedure Show_Subprogram_Stack_Usage
     (Buffer                       : GPS.Editors.Editor_Buffer'Class;
      Subprogram                   : Subprogram_Information_Access;
      Subprogram_Location          : Data_Model.Subprogram_Location;
      Subprogram_Location_Position : Subprogram_Location_Sets.Cursor);
   --  Shows subprogram's stack usage information in source editor.

   procedure Hide_Subprogram_Stack_Usage
     (Buffer                       : GPS.Editors.Editor_Buffer'Class;
      Subprogram                   : Subprogram_Information_Access;
      Subprogram_Location          : Data_Model.Subprogram_Location;
      Subprogram_Location_Position : Subprogram_Location_Sets.Cursor);
   --  Hides subprogram's stack usage information in source editor.

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been closed

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been opened

   ----------------------
   -- Hide_Stack_Usage --
   ----------------------

   procedure Hide_Stack_Usage
     (Module : not null access GNATStack_Module_Id_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
   begin
      if File = No_File then
         return;
      end if;

      declare
         Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                    Module.Kernel.Get_Buffer_Factory.Get (File);

         procedure Process_Subprogram
           (Position : Subprogram_Information_Sets.Cursor);
         --  Process subprogram.

         ------------------------
         -- Process_Subprogram --
         ------------------------

         procedure Process_Subprogram
           (Position : Subprogram_Information_Sets.Cursor)
         is
            Subprogram        : constant Subprogram_Information_Access :=
                                  Element (Position);
            Location_Position : Subprogram_Location_Sets.Cursor :=
                                  Subprogram.Identifier.Locations.First;

         begin
            while Has_Element (Location_Position) loop
               exit when
                 Element (Location_Position).File
                   = String (File.Full_Name.all);

               Next (Location_Position);
            end loop;

            if Has_Element (Location_Position)
              and then not Element (Location_Position).Mark.Is_Empty
            then
               Hide_Subprogram_Stack_Usage
                 (Buffer,
                  Subprogram,
                  Element (Location_Position),
                  Location_Position);
            end if;
         end Process_Subprogram;

      begin
         if Buffer /= GPS.Editors.Nil_Editor_Buffer
           and then Buffer in GPS_Editor_Buffer'Class
         then
            Module.Data.Subprogram_Set.Iterate (Process_Subprogram'Access);
         end if;
      end;
   end Hide_Stack_Usage;

   ----------------------------------------
   -- Hide_Stack_Usage_In_Opened_Editors --
   ----------------------------------------

   procedure Hide_Stack_Usage_In_Opened_Editors
     (Module : not null access GNATStack_Module_Id_Record'Class)
   is
      use GPS.Editors.Buffer_Lists;

      Buffers         : constant GPS.Editors.Buffer_Lists.List :=
        Module.Kernel.Get_Buffer_Factory.Buffers;
      Buffer_Position : GPS.Editors.Buffer_Lists.Cursor;

   begin
      Buffer_Position := Buffers.First;

      while Has_Element (Buffer_Position) loop
         Hide_Stack_Usage (Module, Element (Buffer_Position).File);
         Next (Buffer_Position);
      end loop;
   end Hide_Stack_Usage_In_Opened_Editors;

   ---------------------------------
   -- Hide_Subprogram_Stack_Usage --
   ---------------------------------

   procedure Hide_Subprogram_Stack_Usage
     (Buffer                       : GPS.Editors.Editor_Buffer'Class;
      Subprogram                   : Subprogram_Information_Access;
      Subprogram_Location          : Data_Model.Subprogram_Location;
      Subprogram_Location_Position : Subprogram_Location_Sets.Cursor)
   is
      Mark : GPS.Editors.Editor_Mark_Holders.Holder
        := Subprogram_Location.Mark;

   begin
      Remove_Special_Lines
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Mark.Element,
         Subprogram_Location.Lines);

      declare
         Aux : GPS.Editors.Editor_Mark'Class := Mark.Element;

      begin
         Aux.Delete;
         Mark.Clear;
      end;

      Replace_Element
        (Subprogram.Identifier.Locations,
         Subprogram_Location_Position,
         (Name   => Subprogram_Location.Name,
          File   => Subprogram_Location.File,
          Line   => Subprogram_Location.Line,
          Column => Subprogram_Location.Column,
          Mark   => <>,
          Lines  => 0));
   end Hide_Subprogram_Stack_Usage;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      if Module.Loaded then
         Hide_Stack_Usage (Module, File);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      if Module.Loaded then
         Show_Stack_Usage (Module, File);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Module : not null access GNATStack_Module_Id_Record'Class) is
   begin
      Module.Annotations_Style := Editor_Code_Annotations_Style;

      File_Closed_Hook.Add (new On_File_Closed);
      File_Edited_Hook.Add (new On_File_Edited);
   end Register_Module;

   ---------------------------------
   -- Show_Subprogram_Stack_Usage --
   ---------------------------------

   procedure Show_Subprogram_Stack_Usage
     (Buffer                       : GPS.Editors.Editor_Buffer'Class;
      Subprogram                   : Subprogram_Information_Access;
      Subprogram_Location          : Data_Model.Subprogram_Location;
      Subprogram_Location_Position : Subprogram_Location_Sets.Cursor)
   is
      Indent : constant String := (Subprogram_Location.Column - 1) * ' ';
      Mark   : GPS.Editors.Editor_Mark_Holders.Holder;
      Lines  : Natural := 0;

   begin
      Mark := GPS.Editors.Editor_Mark_Holders.To_Holder
        (Add_Special_Line (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--",
         Module.Annotations_Style));
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--  "
         & To_String (Subprogram.Identifier.Prefix_Name),
         Module.Annotations_Style);
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--",
         Module.Annotations_Style);
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--  Stack usage: local: "
         & Image (Subprogram.Local_Usage)
         & ", global: "
         & Image (Subprogram.Global_Usage),
         Module.Annotations_Style);
      Lines := Lines + 1;

      if not Subprogram.Calls.Is_Empty then
         Add_Special_Line
           (GPS_Editor_Buffer'Class (Buffer),
            Subprogram_Location.Line,
            Indent & "--  Calls:",
            Module.Annotations_Style);
         Lines := Lines + 1;
      end if;

      if not Subprogram.Unbounded.Is_Empty then
         Add_Special_Line
           (GPS_Editor_Buffer'Class (Buffer),
            Subprogram_Location.Line,
            Indent & "--  Unbounded objects:",
            Module.Annotations_Style);
         Lines := Lines + 1;
      end if;

      if not Subprogram.Indirects.Is_Empty then
         Add_Special_Line
           (GPS_Editor_Buffer'Class (Buffer),
            Subprogram_Location.Line,
            Indent & "--  Has indirect calls",
            Module.Annotations_Style);
         Lines := Lines + 1;
      end if;

      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--",
         Module.Annotations_Style);
      Lines := Lines + 1;

      Replace_Element
        (Subprogram.Identifier.Locations,
         Subprogram_Location_Position,
         (Name   => Subprogram_Location.Name,
          File   => Subprogram_Location.File,
          Line   => Subprogram_Location.Line,
          Column => Subprogram_Location.Column,
          Mark   => Mark,
          Lines  => Lines));
   end Show_Subprogram_Stack_Usage;

   ----------------------
   -- Show_Stack_Usage --
   ----------------------

   procedure Show_Stack_Usage
     (Module : not null access GNATStack_Module_Id_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Module.Kernel.Get_Buffer_Factory.Get (File);

      procedure Process_Subprogram
        (Position : Subprogram_Information_Sets.Cursor);
      --  Process subprogram.

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Subprogram        : constant Subprogram_Information_Access :=
                               Element (Position);
         Location_Position : Subprogram_Location_Sets.Cursor :=
                               Subprogram.Identifier.Locations.First;

      begin
         while Has_Element (Location_Position) loop
            exit when
              Element (Location_Position).File = String (File.Full_Name.all);

            Next (Location_Position);
         end loop;

         if Has_Element (Location_Position)
           and then Element (Location_Position).Mark.Is_Empty
         then
            Show_Subprogram_Stack_Usage
              (Buffer,
               Subprogram,
               Element (Location_Position),
               Location_Position);
         end if;
      end Process_Subprogram;

   begin
      --  Check whether file exists needed for two reasons:
      --   - it is not user frendly to display annotations in empty file;
      --   - source editor raises exception when line is outside of range of
      --     lines (see JB25-013).

      if Buffer /= GPS.Editors.Nil_Editor_Buffer
        and then Buffer in GPS_Editor_Buffer'Class
        and then File.Is_Regular_File
      then
         Module.Data.Subprogram_Set.Iterate (Process_Subprogram'Access);
      end if;
   end Show_Stack_Usage;

   ----------------------------------------
   -- Show_Stack_Usage_In_Opened_Editors --
   ----------------------------------------

   procedure Show_Stack_Usage_In_Opened_Editors
     (Module : not null access GNATStack_Module_Id_Record'Class)
   is
      use GPS.Editors.Buffer_Lists;

      Buffers         : constant GPS.Editors.Buffer_Lists.List :=
        Module.Kernel.Get_Buffer_Factory.Buffers;
      Buffer_Position : GPS.Editors.Buffer_Lists.Cursor;

   begin
      Buffer_Position := Buffers.First;

      while Has_Element (Buffer_Position) loop
         Show_Stack_Usage (Module, Element (Buffer_Position).File);
         Next (Buffer_Position);
      end loop;
   end Show_Stack_Usage_In_Opened_Editors;

end GNATStack.Module.Editors;
