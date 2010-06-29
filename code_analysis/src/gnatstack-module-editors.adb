-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GPS.Editors.Line_Information;
with GPS.Intl;
with GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;

package body GNATStack.Module.Editors is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use GPS.Editors;
   use GPS.Intl;
   use GNATStack.Data_Model;
   use GNATStack.Data_Model.Object_Information_Vectors;
   use GNATStack.Data_Model.Subprogram_Information_Sets;
   use GNATStack.Data_Model.Subprogram_Location_Sets;
   use GPS.Editors.Line_Information;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (GPS.Editors.Editor_Mark'Class, Editor_Mark_Access);

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

   procedure On_Preferences_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure On_File_Closed_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Called when a file has been closed

   procedure On_File_Edited_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Called when a file has been opened

   ----------------------
   -- Hide_Stack_Usage --
   ----------------------

   procedure Hide_Stack_Usage
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
           and then Element (Location_Position).Mark /= null
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
   end Hide_Stack_Usage;

   ---------------------------------
   -- Hide_Subprogram_Stack_Usage --
   ---------------------------------

   procedure Hide_Subprogram_Stack_Usage
     (Buffer                       : GPS.Editors.Editor_Buffer'Class;
      Subprogram                   : Subprogram_Information_Access;
      Subprogram_Location          : Data_Model.Subprogram_Location;
      Subprogram_Location_Position : Subprogram_Location_Sets.Cursor)
   is
      Mark : Editor_Mark_Access := Subprogram_Location.Mark;

   begin
      Remove_Special_Lines
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Mark.all,
         Subprogram_Location.Lines);
      Mark.Delete;
      Free (Mark);

      Replace_Element
        (Subprogram.Identifier.Locations,
         Subprogram_Location_Position,
         (Name   => Subprogram_Location.Name,
          File   => Subprogram_Location.File,
          Line   => Subprogram_Location.Line,
          Column => Subprogram_Location.Column,
          Mark   => null,
          Lines  => 0));
   end Hide_Subprogram_Stack_Usage;

   -------------------------
   -- On_File_Closed_Hook --
   -------------------------

   procedure On_File_Closed_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

      D : constant GPS.Kernel.Standard_Hooks.File_Hooks_Args :=
                     GPS.Kernel.Standard_Hooks.File_Hooks_Args (Data.all);

   begin
      if Module.Loaded then
         Hide_Stack_Usage (Module, D.File);
      end if;
   end On_File_Closed_Hook;

   -------------------------
   -- On_File_Edited_Hook --
   -------------------------

   procedure On_File_Edited_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

      D : constant GPS.Kernel.Standard_Hooks.File_Hooks_Args :=
                     GPS.Kernel.Standard_Hooks.File_Hooks_Args (Data.all);

   begin
      if Module.Loaded then
         Show_Stack_Usage (Module, D.File);
      end if;
   end On_File_Edited_Hook;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);

   begin
      Module.Annotations_Style.Set_Background
        (Module.Annotations_Background.Get_Pref);
      Module.Annotations_Style.Set_Foreground
        (Module.Annotations_Foreground.Get_Pref);
   end On_Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Module : not null access GNATStack_Module_Id_Record'Class) is
   begin
      Module.Annotations_Foreground :=
        Default_Preferences.Create
          (Module.Kernel.Get_Preferences,
           "GNATStack-Annotations-Foreground",
           -"Color for annotations foregorund",
           -"Plugins/GNATStack",
           -"Color to be used for the foregorund of annotations",
           "#000000");
      Module.Annotations_Background :=
        Default_Preferences.Create
          (Module.Kernel.Get_Preferences,
           "GNATStack-Annotations-Background",
           -"Color for annotations backgorund",
           -"Plugins/GNATStack",
           -"Color to be used for the backgorund of annotations",
           "#E9E9E9");
      Module.Annotations_Style :=
        GPS.Kernel.Styles.Get_Or_Create_Style
          (Module.Kernel, GNATStack_Editor_Annotations);
      Module.Annotations_Style.Set_Foreground
        (Module.Annotations_Foreground.Get_Pref);
      Module.Annotations_Style.Set_Background
        (Module.Annotations_Background.Get_Pref);

      GPS.Kernel.Hooks.Add_Hook
        (Module.Kernel, GPS.Kernel.Preferences_Changed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Preferences_Changed'Access),
         "gnatstack.preferences_changed");
      GPS.Kernel.Hooks.Add_Hook
        (Module.Kernel, GPS.Kernel.File_Closed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_File_Closed_Hook'Access),
         Name  => "gnatstack.file_closed");
      GPS.Kernel.Hooks.Add_Hook
        (Module.Kernel, GPS.Kernel.File_Edited_Hook,
         GPS.Kernel.Hooks.Wrapper (On_File_Edited_Hook'Access),
         Name  => "gnatstack.file_edited");
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
      Mark   : Editor_Mark_Access;
      Lines  : Natural := 0;

   begin
      Mark :=
        new GPS.Editors.Editor_Mark'Class'
          (Add_Special_Line
               (GPS_Editor_Buffer'Class (Buffer),
                Subprogram_Location.Line,
                Indent & "--",
                GNATStack_Editor_Annotations));
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--  "
         & To_String (Subprogram.Identifier.Prefix_Name),
         GNATStack_Editor_Annotations);
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--",
         GNATStack_Editor_Annotations);
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--  Local stack usage: "
         & Trim (Integer'Image (Subprogram.Local_Usage.Size), Both)
         & "/" & To_String (Subprogram.Local_Usage.Qualifier),
         GNATStack_Editor_Annotations);
      Lines := Lines + 1;
      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--  Global stack usage: "
         & Trim (Integer'Image (Subprogram.Global_Usage.Size), Both)
         & "/" & To_String (Subprogram.Global_Usage.Qualifier),
         GNATStack_Editor_Annotations);
      Lines := Lines + 1;

      if not Subprogram.Calls.Is_Empty then
         Add_Special_Line
           (GPS_Editor_Buffer'Class (Buffer),
            Subprogram_Location.Line,
            Indent & "--  Calls:",
            GNATStack_Editor_Annotations);
         Lines := Lines + 1;
      end if;

      if not Subprogram.Unbounded.Is_Empty then
         Add_Special_Line
           (GPS_Editor_Buffer'Class (Buffer),
            Subprogram_Location.Line,
            Indent & "--  Has unbounded objects:",
            GNATStack_Editor_Annotations);
         Lines := Lines + 1;
      end if;

      if not Subprogram.Indirects.Is_Empty then
         Add_Special_Line
           (GPS_Editor_Buffer'Class (Buffer),
            Subprogram_Location.Line,
            Indent & "--  Has indirect calls",
            GNATStack_Editor_Annotations);
         Lines := Lines + 1;
      end if;

      Add_Special_Line
        (GPS_Editor_Buffer'Class (Buffer),
         Subprogram_Location.Line,
         Indent & "--",
         GNATStack_Editor_Annotations);
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
           and then Element (Location_Position).Mark = null
         then
            Show_Subprogram_Stack_Usage
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
   end Show_Stack_Usage;

   ----------------------------------------
   -- Show_Stack_Usage_In_Opened_Editors --
   ----------------------------------------

   procedure Show_Stack_Usage_In_Opened_Editors
     (Module : not null access GNATStack_Module_Id_Record'Class)
   is
--        use Code_Analysis.Project_Maps;
--        use Code_Analysis.File_Maps;
      use GPS.Editors.Buffer_Lists;

      Buffers         : constant GPS.Editors.Buffer_Lists.List :=
        Module.Kernel.Get_Buffer_Factory.Buffers;
--        Project_Position : Code_Analysis.Project_Maps.Cursor :=
--          Self.Tree.First;
--        File_Position    : Code_Analysis.File_Maps.Cursor;
--        File             : GNATCOLL.VFS.Virtual_File;
      Buffer_Position : GPS.Editors.Buffer_Lists.Cursor;

   begin
      Buffer_Position := Buffers.First;

      while Has_Element (Buffer_Position) loop
         Show_Stack_Usage (Module, Element (Buffer_Position).File);
         Next (Buffer_Position);
      end loop;

--        Projects :
--        while Has_Element (Project_Position) loop
--           File_Position := Element (Project_Position).Files.First;
--
--           while Has_Element (File_Position) loop
--              File := Element (File_Position).Name;
--              Buffer_Position := Buffers.First;
--
--              while Has_Element (Buffer_Position) loop
--                 if Element (Buffer_Position).File = File then
--                if Element (Buffer_Position) in GPS_Editor_Buffer'Class then
--                       Show_Annotations
--                        (GPS_Editor_Buffer'Class (Element (Buffer_Position)),
--                          Element (File_Position));
--                    end if;
--
--                    Delete (Buffers, Buffer_Position);
--
--                    exit Projects when Buffers.Is_Empty;
--                    exit;
--                 end if;
--
--                 Next (Buffer_Position);
--              end loop;
--
--              Next (File_Position);
--           end loop;
--
--           Next (Project_Position);
--        end loop Projects;
   end Show_Stack_Usage_In_Opened_Editors;

end GNATStack.Module.Editors;
