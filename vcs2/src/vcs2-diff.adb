------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Gdk.RGBA;                 use Gdk.RGBA;
with Generic_Views;            use Generic_Views;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Intl;                 use GPS.Intl;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Scripts;         use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Files;   use GNATCOLL.Scripts.Files;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Widget;               use Gtk.Widget;
with Pango.Enums;              use Pango.Enums;
with VCS2.Engines;             use VCS2.Engines;
with VCS2.Views;               use VCS2.Views;

package body VCS2.Diff is

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

   type Diff_View_Record is new View_Record with record
      Patch : Diff_Viewer;
   end record;

   function Initialize
     (Self : access Diff_View_Record'Class) return Gtk_Widget;

   package Diff_Views is new Generic_Views.Simple_Views
     (Module_Name        => "VCS_Diff",
      View_Name          => "Diff",
      Formal_View_Record => Diff_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Local_Toolbar      => False,
      Local_Config       => False,
      Areas              => Gtkada.MDI.Central_Only,
      Position           => Position_Automatic,
      Initialize         => Initialize);
   subtype Diff_View is Diff_Views.View_Access;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Diff_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Self.Patch);
      Scrolled.Add (Self.Patch);

      return Gtk_Widget (Self.Patch);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Diff_Viewer) is
   begin
      --  ??? Should be implemented as an editor with a special language for
      --  syntax highlighting, which provides folding for chunks.

      Self := new Diff_Viewer_Record;
      Gtk.Text_View.Initialize (Self);
      Self.Set_Editable (False);

      Set_Font_And_Colors (Self, Fixed_Font => True, Pref => null);
   end Gtk_New;

   --------------
   -- Add_Diff --
   --------------

   procedure Add_Diff
     (Self  : not null access Diff_Viewer_Record;
      Patch : String)
   is
      Buffer : constant Gtk_Text_Buffer := Self.Get_Buffer;
      Iter   : Gtk_Text_Iter;
      Diff, Block, Added, Removed  : Gtk_Text_Tag;
      List   : String_List_Access;

   begin
      Diff := Buffer.Create_Tag;
      Set_Property (Diff, Gtk.Text_Tag.Weight_Property, Pango_Weight_Bold);
      Gdk.RGBA.Set_Property
        (Diff, Gtk.Text_Tag.Foreground_Rgba_Property, Emblem_Color);

      Block := Buffer.Create_Tag;
      Gdk.RGBA.Set_Property
        (Block, Gtk.Text_Tag.Foreground_Rgba_Property, (0.2, 0.8, 0.7, 1.0));

      Added := Buffer.Create_Tag;
      Gdk.RGBA.Set_Property
        (Added, Gtk.Text_Tag.Foreground_Rgba_Property, (0.2, 0.6, 0.0, 1.0));

      Removed := Buffer.Create_Tag;
      Gdk.RGBA.Set_Property
        (Removed, Gtk.Text_Tag.Foreground_Rgba_Property,
         (1.0, 0.29, 0.32, 1.0));

      Buffer.Get_End_Iter (Iter);

      List := Split (Patch, ASCII.LF, Omit_Empty_Lines => False);

      for L of List.all loop
         if L'Length > 5
           and then L (L'First .. L'First + 4) = "diff "
         then
            Buffer.Insert_With_Tags (Iter, L.all, Diff);

         elsif L'Length > 2
           and then L (L'First .. L'First + 2) = "@@ "
         then
            Buffer.Insert_With_Tags (Iter, L.all, Block);

         elsif L'Length >= 1 and then L (L'First) = '-' then
            Buffer.Insert_With_Tags (Iter, L.all, Removed);

         elsif L'Length >= 1 and then L (L'First) = '+' then
            Buffer.Insert_With_Tags (Iter, L.all, Added);

         else
            Buffer.Insert (Iter, L.all);
         end if;

         Buffer.Insert (Iter, (1 .. 1 => ASCII.LF));
      end loop;

      Free (List);
      Buffer.Insert (Iter, (1 .. 1 => ASCII.LF));
   end Add_Diff;

   ----------------------
   -- On_Diff_Computed --
   ----------------------

   overriding procedure On_Diff_Computed
     (Self   : not null access On_Diff_Visitor;
      Diff   : String)
   is
      View  : Diff_View;
      Child : MDI_Child;
   begin
      if Diff = "" then
         if Self.File = No_File then
            Insert (Self.Kernel, -"No difference found");
         else
            Insert
              (Self.Kernel, -"No difference found for "
               & Self.File.Display_Full_Name);
         end if;

      else
         View  := Diff_Views.Get_Or_Create_View (Self.Kernel);
         Child := MDI_Child (Diff_Views.Child_From_View (View));
         Child.Set_Title ("Diff " & Self.File.Display_Base_Name
                          & " [" & To_String (Self.Ref) & "]");
         View.Patch.Add_Diff (Diff);
      end if;
   end On_Diff_Computed;

   ----------------------
   -- On_File_Computed --
   ----------------------

   overriding procedure On_File_Computed
     (Self     : not null access On_Diff_Visitor;
      Contents : String)
   is
      Script : constant Scripting_Language :=
        Lookup_Scripting_Language (Self.Kernel.Scripts, "Python");
      Args : Callback_Data'Class := Create (Script, 3);
      Dummy    : Boolean;
      Tmp_File, Diff_File : Virtual_File;
      W        : Writable_File;
   begin
      --  ??? Due to limitations in the vdiff module, this must be the same
      --  name as returned by Vdiff2_Module.Callback.Get_Ref_Filename
      Tmp_File := Create_From_Dir
        (Get_Tmp_Directory,
         "ref$" & Self.File.Base_Name);

      if Tmp_File.Is_Regular_File then
         --  If file still exists ensure we can rewrite it.
         Tmp_File.Set_Writable (True);
      end if;

      W := Write_File (Tmp_File, Append => False);
      Write (W, Contents);
      Close (W);

      --  ??? Generate a diff file that GPS can read. Not sure why we actually
      --  need this file when we have both versions of the file already.
      --  Also, vcs systems are in general pretty good at generating diff (git
      --  is much better than "diff") so we should take advantage of that.

      Diff_File := Create_From_Dir
        (Get_Tmp_Directory, Self.File.Base_Name & ".diff");

      GNATCOLL.Scripts.Files.Set_Nth_Arg (Args, 1, Tmp_File);
      GNATCOLL.Scripts.Files.Set_Nth_Arg (Args, 2, Self.File);
      GNATCOLL.Scripts.Files.Set_Nth_Arg (Args, 3, Diff_File);
      Args.Execute_Command ("GPS.VCS2._diff");

      Dummy := Diff_Action_Hook.Run
        (Kernel    => Self.Kernel,
         Vcs_File  => Self.File,
         Orig_File => Tmp_File,
         New_File  => Self.File,
         Diff_File => Diff_File,
         Title     =>
           Self.File.Display_Base_Name
           & " [" & To_String (Self.Ref) & "]");

      Tmp_File.Delete (Success => Dummy);
      Diff_File.Delete (Success => Dummy);
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
   begin
      Diff_Views.Register_Module (Kernel);

      Register_Action
        (Kernel, "diff against head for file",
         Description =>
           -("Display the local changes for the current file"),
         Command     => new Diff_Head_For_File,
         Filter      => Kernel.Lookup_Filter ("File"),
         Icon_Name   => "vcs-diff-symbolic",
         Category    => "VCS2");

      Register_Contextual_Menu
        (Kernel,
         Action   => "diff against head for file",
         Label    => "Version Control/Show local changes for %f");

      Register_Action
        (Kernel, "diff against head for file in editor",
         Description =>
           -("Display the local changes for the current file in an editor"),
         Command  => new Diff_Head_For_File_In_Editor,
         Filter   => Kernel.Lookup_Filter ("File"),
         Category => "VCS2");

      Register_Contextual_Menu
        (Kernel,
         Action   => "diff against head for file in editor",
         Label    => "Version Control/Show local changes for %f (in editor)");

      Register_Action
        (Kernel, "diff all against head",
         Description =>
           -("Display all the local changes for the current version control"
             & " system"),
         Command  => new Diff_Head,
         Category => "VCS2");
   end Register_Module;

end VCS2.Diff;
