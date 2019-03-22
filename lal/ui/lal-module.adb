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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Basic_Types;             use Basic_Types;
with GPS.Editors;             use GPS.Editors;
with GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;
with GPS.Kernel.Xref;
with LAL.Highlighters;
with Libadalang.Analysis;
with Libadalang.Common;
with Langkit_Support.Slocs;

package body LAL.Module is

   type Highlight_Hook is new Highlight_Hooks_Function with null record;
   overriding procedure Execute
     (Self      : Highlight_Hook;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File      : GNATCOLL.VFS.Virtual_File;
      From_Line : Integer;
      To_Line   : Integer);
   --  Highlight piece of code between From_Line and To_Line in a buffer
   --  corresponding to given File.

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

   type Highlightable_Editor_Buffer_Type is
     new LAL.Highlighters.Highlightable_Interface with record
      Buffer : GPS.Editors.Editor_Buffer_Holders.Holder;
   end record;
   --  Wrapper aroung a source editor buffer allowing it to be highlighted
   --  using Libadalang.

   overriding procedure Highlight_Token
     (Self  : in out Highlightable_Editor_Buffer_Type;
      Token : Libadalang.Common.Token_Reference;
      Style : String);

   overriding procedure Remove_Highlighting
     (Self  : in out Highlightable_Editor_Buffer_Type;
      Style : String;
      From  : Integer;
      To    : Integer);

   procedure Highlight_Buffer
     (Buffer    : GPS.Editors.Editor_Buffer'Class;
      From_Line : Integer;
      To_Line   : Integer);
   --  Highlight the given editor

   type LAL_UI_Module_Id_Record is new GPS.Kernel.Modules.Module_ID_Record with
   record
      Hook : aliased Highlight_Hook;
      Core : LAL.Core_Module.LAL_Module_Id;
   end record;

   type LAL_UI_Module_Id is access all LAL_UI_Module_Id_Record'Class;

   Module : LAL_UI_Module_Id;

   ---------------------
   -- Highlight_Token --
   ---------------------

   overriding procedure Highlight_Token
     (Self  : in out Highlightable_Editor_Buffer_Type;
      Token : Libadalang.Common.Token_Reference;
      Style : String)
   is
      use Libadalang.Common;
      use Langkit_Support.Slocs;

      Loc   : constant Source_Location_Range := Sloc_Range (Data (Token));
      From  : constant Positive := Positive (Loc.Start_Line);
      To    : constant Positive := Positive (Loc.End_Line);
      Start : constant Visible_Column_Type :=
                Visible_Column_Type (Loc.Start_Column);
      Stop  : constant Visible_Column_Type :=
                Visible_Column_Type (Loc.End_Column);
      Buffer : constant GPS.Editors.Editor_Buffer'Class := Self.Buffer.Element;
   begin
      if Style = "" then
         return;
      end if;

      if From = To then
         Buffer.Apply_Style (Style, From, Start, Stop);
      else
         for J in From + 1 .. To - 1 loop
            Buffer.Apply_Style (Style, J, 1);
         end loop;

         Buffer.Apply_Style (Style, To, 1, Stop);
      end if;
   end Highlight_Token;

   -------------------------
   -- Remove_Highlighting --
   -------------------------

   overriding procedure Remove_Highlighting
     (Self  : in out Highlightable_Editor_Buffer_Type;
      Style : String;
      From  : Integer;
      To    : Integer) is
   begin
      Self.Buffer.Element.Remove_Style_On_Lines
        (Style,
         Editable_Line_Type (From),
         Editable_Line_Type (To));
   end Remove_Highlighting;
   ----------------------
   -- Highlight_Buffer --
   ----------------------

   procedure Highlight_Buffer
     (Buffer    : GPS.Editors.Editor_Buffer'Class;
      From_Line : Integer;
      To_Line   : Integer)
   is
   begin
      if Buffer = GPS.Editors.Nil_Editor_Buffer
        or else To_Lower (Buffer.Get_Language.Get_Name) /= "ada"
      then
         return;
      end if;

      declare
         Highlightable_Buffer : Highlightable_Editor_Buffer_Type :=
                                  Highlightable_Editor_Buffer_Type'
                                    (Buffer => Editor_Buffer_Holders.To_Holder
                                       (Buffer));
         Unit                 : constant Libadalang.Analysis.Analysis_Unit :=
                                  Libadalang.Analysis.Get_From_Buffer
                                    (Context  => Module.Core.Context,
                                     Filename => Buffer.File.Display_Full_Name,
                                     Buffer   => Buffer.Get_Chars);
      begin
         Highlightable_Buffer.Highlight_Using_Tree
           (Unit => Unit,
            From => From_Line,
            To   => To_Line);
      end;
   end Highlight_Buffer;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : Highlight_Hook;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File      : GNATCOLL.VFS.Virtual_File;
      From_Line : Integer;
      To_Line   : Integer)
   is
      pragma Unreferenced (Self);
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File, Open_Buffer => False, Open_View => False);
   begin
      Highlight_Buffer (Buffer, From_Line, To_Line);
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
        Kernel.Get_Buffer_Factory.Get
          (File, Open_Buffer => False, Open_View => False);
   begin
      Highlight_Buffer
        (Buffer,
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

   -------------------------
   -- Get_LAL_Core_Module --
   -------------------------

   function Get_LAL_Core_Module return LAL.Core_Module.LAL_Module_Id is
   begin
      return Module.Core;
   end Get_LAL_Core_Module;

end LAL.Module;
