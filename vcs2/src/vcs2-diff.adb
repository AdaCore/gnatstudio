------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Basic_Types;                  use Basic_Types;
with Commands.Interactive;         use Commands, Commands.Interactive;

with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.GtkAda;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;

with GNATCOLL.VFS;                 use GNATCOLL.VFS;

with Gtkada.MDI;                   use Gtkada.MDI;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Text_Tag;                 use Gtk.Text_Tag;
with Gtk.Widget;                   use Gtk.Widget;

with VCS2.Engines;                 use VCS2.Engines;
with Vdiff2_Module.Utils;

package body VCS2.Diff is

   Diff_Name : constant Filesystem_String := "vcs2_diff.diff";

   type Diff_Head_For_File is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Diff_Head_For_File;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Show the local changes for the current file

   type Diff_Head_For_File_In_Editor is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Diff_Head_For_File_In_Editor;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Show the local changes for the current file

   type Diff_Head is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Diff_Head;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Show the local changes for the current file

   type On_Diff_Visitor is new Task_Visitor with record
      Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Ref    : Unbounded_String;
   end record;
   overriding procedure On_Diff_Computed
     (Self   : not null access On_Diff_Visitor;
      Diff   : String);
   overriding procedure On_File_Computed
     (Self     : not null access On_Diff_Visitor;
      Contents : String);

   ---------------------------------
   -- Create_Or_Reuse_Diff_Editor --
   ---------------------------------

   procedure Create_Or_Reuse_Diff_Editor
     (Kernel : Kernel_Handle;
      Patch  : String;
      Title  : String := "";
      Header : String := "")
   is
      File   : constant Virtual_File :=
        Create_From_Dir (Get_Project (Kernel).Artifacts_Dir, Diff_Name);
      Buffer : constant GPS_Editor_Buffer'Class :=
        GPS_Editor_Buffer'Class
          (Kernel.Get_Buffer_Factory.Get (File => File));

      procedure Highlight_Header;

      ----------------------
      -- Highlight_Header --
      ----------------------

      procedure Highlight_Header
      is
         From_Line : constant Integer :=
           Buffer.End_Of_Buffer.Line;
         To_Line   : Integer;
      begin
         if Header /= "" then
            Buffer.Insert (Buffer.End_Of_Buffer, Header & ASCII.LF);
            To_Line := Buffer.End_Of_Buffer.Line;
            Buffer.Apply_Style_To_Lines
              ("Editor ephemeral highlighting simple",
               Editable_Line_Type (From_Line),
               Editable_Line_Type (To_Line));
         end if;
      end Highlight_Header;
   begin
      Buffer.Set_Read_Only (False);
      Highlight_Header;
      Buffer.Insert (Buffer.End_Of_Buffer, Patch & ASCII.LF);
      Buffer.Save (Interactive => False);
      Buffer.Set_Read_Only (True);

      if Title /= "" then
         GPS.Editors.GtkAda.Get_MDI_Child
           (Buffer.Current_View).Set_Title (Title);
      end if;
   end Create_Or_Reuse_Diff_Editor;

   -----------------------
   -- Clear_Diff_Editor --
   -----------------------

   procedure Clear_Diff_Editor (Kernel : Kernel_Handle)
   is
      File   : constant Virtual_File :=
        Create_From_Dir (Get_Project (Kernel).Artifacts_Dir, Diff_Name);
      Buffer : constant GPS_Editor_Buffer'Class :=
        GPS_Editor_Buffer'Class
          (Kernel.Get_Buffer_Factory.Get (File => File));
   begin
      Buffer.Set_Read_Only (False);
      Buffer.Delete (Buffer.Beginning_Of_Buffer, Buffer.End_Of_Buffer);
      Buffer.Set_Read_Only (True);
   end Clear_Diff_Editor;

   ----------------------
   -- On_Diff_Computed --
   ----------------------

   overriding procedure On_Diff_Computed
     (Self   : not null access On_Diff_Visitor;
      Diff   : String) is
   begin
      if Diff = "" then
         if Self.File = No_File then
            Insert (Self.Kernel, "No difference found");
         else
            Insert
              (Self.Kernel, "No difference found for "
               & Self.File.Display_Full_Name);
         end if;
      else
         Clear_Diff_Editor (Kernel_Handle (Self.Kernel));
         Create_Or_Reuse_Diff_Editor
           (Kernel => Kernel_Handle (Self.Kernel),
            Patch  => Diff,
            Title  => "Diff " & Self.File.Display_Base_Name
            & " [" & To_String (Self.Ref) & "]",
            Header => "");
      end if;
   end On_Diff_Computed;

   ----------------------
   -- On_File_Computed --
   ----------------------

   overriding procedure On_File_Computed
     (Self     : not null access On_Diff_Visitor;
      Contents : String)
   is
      Tmp_File  : Virtual_File;
      W         : Writable_File;
      Dummy     : Boolean;
   begin
      --  We are using vdiff2 thus mimick the naming convention
      Tmp_File := Create_From_Dir
        (Get_Tmp_Directory, "ref$" & Self.File.Base_Name);

      if Tmp_File.Is_Regular_File then
         --  If file still exists ensure we can rewrite it.
         Tmp_File.Set_Writable (True);
      end if;

      W := Write_File (Tmp_File, Append => False);
      Write (W, Contents);
      Close (W);

      Vdiff2_Module.Utils.Visual_Diff
        (Diff_Mode.Get_Pref, Tmp_File, Self.File, Ref_File => 1);

      Vdiff2_Module.Utils.Setup_Ref
        (Kernel   => Self.Kernel,
         Base     => Self.File,
         Ref_File => Tmp_File,
         Vcs_File => No_File,  --  VCS1 parameter only
         Title    =>
           Self.File.Display_Base_Name & " [" & To_String (Self.Ref) & "]");
   end On_File_Computed;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Diff_Head_For_File;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      File   : constant Virtual_File  := File_Information (Context.Context);
      VCS    : VCS_Engine_Access;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         VCS.Queue_Diff
           (new On_Diff_Visitor'(
               Task_Visitor with
               Kernel => Kernel,
               File   => File,
               Ref     => To_Unbounded_String ("HEAD")),
            Ref   => "HEAD",
            File  => File);
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Diff_Head_For_File_In_Editor;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      File   : constant Virtual_File := File_Information (Context.Context);
      VCS    : VCS_Engine_Access;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         VCS.Queue_View_File
           (new On_Diff_Visitor'
              (Task_Visitor with
               Kernel => Kernel,
               Ref    => To_Unbounded_String ("HEAD"),
               File   => File),
            Ref   => "HEAD",
            File  => File);
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Diff_Head;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      VCS    : constant VCS_Engine_Access := Active_VCS (Kernel);
   begin
      VCS.Queue_Diff
        (new On_Diff_Visitor'(
            Task_Visitor with
            Kernel => Kernel,
            File   => No_File,
            Ref    => To_Unbounded_String ("HEAD")),
         Ref   => "HEAD");
      return Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      File_Action : constant String := "diff against head for file";
      Head_Action : constant String := "diff against head for file in editor";
   begin
      Register_Action
        (Kernel, File_Action,
         Description =>
           "Display the local changes for the current file",
         Command     => new Diff_Head_For_File,
         Filter      => Kernel.Lookup_Filter ("File"),
         Icon_Name   => "vcs-diff-symbolic",
         Category    => "VCS2");

      Register_Contextual_Menu
        (Kernel,
         Action   => File_Action,
         Label    => "Version Control/Show local changes for %f",
         Group    => VCS_Contextual_Group);

      Register_Action
        (Kernel, Head_Action,
         Description =>
           "Display the local changes for the current file in an editor",
         Command  => new Diff_Head_For_File_In_Editor,
         Filter   => Kernel.Lookup_Filter ("File"),
         Category => "VCS2");

      Register_Contextual_Menu
        (Kernel,
         Action   => Head_Action,
         Label    => "Version Control/Show local changes for %f (in editor)",
         Group    => VCS_Contextual_Group);

      Register_Action
        (Kernel, "diff all against head",
         Description =>
           ("Display all the local changes for the current version control"
            & " system"),
         Command  => new Diff_Head,
         Category => "VCS2");
   end Register_Module;

end VCS2.Diff;
