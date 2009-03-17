-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008-2009, AdaCore               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with GPS.Editors; use GPS.Editors;

package Src_Editor_Module.Editors is

   type Src_Editor_Buffer_Factory is new GPS.Editors.Editor_Buffer_Factory
   with private;

   function Create (Kernel : Kernel_Handle) return Src_Editor_Buffer_Factory;
   --  Constructor

   procedure Destroy (X : in out Src_Editor_Buffer_Factory);
   --  Destructor

   overriding function Get
     (This        : Src_Editor_Buffer_Factory;
      File        : Virtual_File;
      Force       : Boolean := False;
      Open_Buffer : Boolean := False;
      Open_View   : Boolean := True) return Editor_Buffer'Class;

   overriding function New_Mark
     (This   : Src_Editor_Buffer_Factory;
      File   : Virtual_File := No_File;
      Line   : Integer;
      Column : Integer) return Editor_Mark'Class;

private

   type Element is record
      Box : Src_Editor_Box.Source_Editor_Box;
   end record;

   procedure Free (X : in out Element) is null;

   No_Element : constant Element := (Box => null);

   package Pure_Editors_Hash is new HTables.Simple_HTable
     (Header_Num, Element, Free, No_Element, Virtual_File, Hash, Equal);

   type Table_Access is access Pure_Editors_Hash.HTable;

   type Src_Editor_Buffer_Factory is new GPS.Editors.Editor_Buffer_Factory
   with record
      Kernel : Kernel_Handle;

      Pure_Buffers : Table_Access;
      --  Pure_Buffers are editors which are not realized to an MDI widget.
      --  This is used to support editor APIs without having to create
      --  corresponding widgets.
   end record;

end Src_Editor_Module.Editors;
