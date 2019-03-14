------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GPS.Editors;             use GPS.Editors;
with GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;
with GPS.Kernel.Xref;
with LAL.Core_Module;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body LAL.Module is

   type Highlight_Hook is new Highlight_Hooks_Function with null record;
   overriding procedure Execute
     (Self      : Highlight_Hook;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Phase     : Integer;
      File      : GNATCOLL.VFS.Virtual_File;
      From_Line : Integer;
      To_Line   : Integer);
   --  Highlight piece of code between From_Line and To_Line in a buffer
   --  corresponding to given File.
   --  If Phase = 1 do fastest highlighting, take only token information into
   --  account, due to this phase runs immediate after each keystroke.
   --  If Phase = 2 do most accurate highlighting. This phase runs when LAL
   --  tree is ready.

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Callback for the "file_edited" hook

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);

   procedure Highlight_For_Phase
     (Buffer    : GPS.Editors.Editor_Buffer'Class;
      Phase     : Integer;
      From_Line : Integer;
      To_Line   : Integer);
   --  Highlight the given editor, for the given phase.

   type LAL_UI_Module_Id_Record is new GPS.Kernel.Modules.Module_ID_Record with
   record
      Hook : aliased Highlight_Hook;
      Core : LAL.Core_Module.LAL_Module_Id;
   end record;

   type LAL_UI_Module_Id is access all LAL_UI_Module_Id_Record'Class;

   Module : LAL_UI_Module_Id;

   -------------------------
   -- Highlight_For_Phase --
   -------------------------

   procedure Highlight_For_Phase
     (Buffer    : GPS.Editors.Editor_Buffer'Class;
      Phase     : Integer;
      From_Line : Integer;
      To_Line   : Integer) is
   begin
      if Buffer = GPS.Editors.Nil_Editor_Buffer
        or else To_Lower (Buffer.Get_Language.Get_Name) /= "ada"
      then
         return;
      end if;

      if Phase = 1 then
         Module.Core.Highlighter.Highlight_Fast (Buffer, From_Line, To_Line);
      else
         Module.Core.Highlighter.Highlight_Using_Tree
           (Buffer, From_Line, To_Line);
      end if;
   end Highlight_For_Phase;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : Highlight_Hook;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Phase     : Integer;
      File      : GNATCOLL.VFS.Virtual_File;
      From_Line : Integer;
      To_Line   : Integer)
   is
      pragma Unreferenced (Self);
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File, Open_Buffer => False, Open_View => False);
   begin
      Highlight_For_Phase (Buffer, Phase, From_Line, To_Line);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Self);
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File);
   begin
      Highlight_For_Phase
        (Buffer,
         Phase => 1,
         From_Line => 1,
         To_Line => Buffer.End_Of_Buffer.Line);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self, Kernel);
      Default_Charset : constant String := GPS.Kernel.Charsets.Get_File_Charset
        (GNATCOLL.VFS.No_File);
   begin
      Module.Core.Reset_Context (Default_Charset);
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Config     : Use_LAL_Configuration;
      Doc_Before : Boolean;
      Legacy     : Language.Tree.Database.Tree_Language_Access)
   is
      Default_Charset : constant String := GPS.Kernel.Charsets.Get_File_Charset
        (GNATCOLL.VFS.No_File);
   begin
      Module := new LAL_UI_Module_Id_Record;

      LAL.Core_Module.Register_Module
        (Kernel     => Kernel,
         Config     => Config,
         Doc_Before => Doc_Before,
         Legacy     => Legacy,
         Charset    => Default_Charset,
         Formater   => GPS.Kernel.Xref.Get_HTML_Profile_Formater'Access,
         Result     => Module.Core);

      if Config (Use_LAL_In_Highlight) then
         Highlight_Range_Hook.Add (Module.Hook'Access);
         File_Edited_Hook.Add (new On_File_Edited);
      end if;
   end Register_Module;

end LAL.Module;
