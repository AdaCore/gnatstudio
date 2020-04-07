------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

--  Integration with GNAT Studio's completion engine.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.VFS;

with Basic_Types;           use Basic_Types;
with Completion;            use Completion;
with GPS.Kernel;            use GPS.Kernel;
with Language;              use Language;
with LSP.Messages;          use LSP.Messages;
with LSP.Types;             use LSP.Types;
with Xref;                  use Xref;

package GPS.LSP_Client.Completion is

   procedure Register (Kernel : Kernel_Handle);
   --  Register requests

   ----------------------------
   -- LSP Completion Manager --
   ----------------------------

   type LSP_Completion_Manager is
     new Asynchronous_Completion_Manager with private;
   type LSP_Completion_Manager_Access is
     access all LSP_Completion_Manager'Class;

   overriding function Get_Initial_Completion_List
     (Manager : access LSP_Completion_Manager;
      Context : Completion_Context) return Completion_List;

   overriding procedure Query_Completion_List
     (Manager : access LSP_Completion_Manager;
      Context : Completion_Context;
      Win     : access Completion_Display_Interface'Class);

   -----------------------------
   -- LSP Completion Resolver --
   -----------------------------

   type LSP_Completion_Resolver is new Completion_Resolver with private;
   type LSP_Completion_Resolver_Access is
     access all LSP_Completion_Resolver'Class;

   overriding procedure Get_Completion_Root
     (Resolver   : access LSP_Completion_Resolver;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List);
   --  See inherited documentation

   overriding function Get_Id
     (Resolver : LSP_Completion_Resolver) return String;
   --  See inherited documentation

   overriding procedure Free (Resolver : in out LSP_Completion_Resolver)
   is null;
   --  See inherited documentation

   -----------------------------
   -- LSP Completion Proposal --
   -----------------------------

   type LSP_Completion_Proposal is new Completion_Proposal with private;

   overriding function Get_Completion
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String;
   --  See inherited documentation

   overriding function Get_Label
     (Proposal : LSP_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String;
   --  See inherited documentation

   overriding function Get_Category
     (Proposal : LSP_Completion_Proposal) return Language_Category;
   --  See inherited documentation

   overriding function Get_Visibility
     (Proposal : LSP_Completion_Proposal) return Construct_Visibility;
   --  See inherited documentation

   overriding function Get_Documentation
     (Proposal : LSP_Completion_Proposal)
      return String;
   --  See inherited documentation

   overriding function Insert_Text_On_Selected
     (Proposal : LSP_Completion_Proposal) return Boolean;
   --  See inherited documentation

   overriding procedure On_Selected
     (Proposal : LSP_Completion_Proposal;
      Kernel   : not null Kernel_Handle);
   --  See inherited documentation

   overriding function Match
     (Proposal   : LSP_Completion_Proposal;
      Context    : Completion_Context;
      Offset     : String_Index_Type) return Boolean;
   --  See inherited documentation

   overriding procedure Free (Proposal : in out LSP_Completion_Proposal)
   is
   null;
   --  See inherited documentation

   overriding function Deep_Copy
     (Proposal : LSP_Completion_Proposal)
      return Completion_Proposal'Class
   is
     (Proposal);
   --  See inherited documentation

   overriding function To_Completion_Id
     (Proposal : LSP_Completion_Proposal) return Completion_Id;
   --  See inherited documentation

   No_Proposal : constant LSP_Completion_Proposal;

private

   type LSP_Completion_Manager is new Asynchronous_Completion_Manager
     with record
      Kernel : Kernel_Handle;
   end record;

   type LSP_Completion_Resolver is new Completion_Resolver with record
      Kernel      : Kernel_Handle;
      Lang_Name   : Unbounded_String;
      Completions : LSP.Messages.CompletionList;
   end record;

   type LSP_Completion_Proposal is new Completion_Proposal with record
      Text                     : LSP_String;
      --  The text that will replace the completion prefix if this proposal
      --  gets selected.

      Label                    : LSP_String;
      --  The label displayed in the completion window.

      Detail                   : Unbounded_String;
      --  The detail displayed in the completion window notes.
      --  In the LSP world, this field is commonly used to display the
      --  profile of subprograms for instance.

      Highlightable_Detail     : Boolean := False;
      --  True if the detail can be highlighted.
      --  ??? This field is only set to True for Ada since we don't have
      --  a generic API to highlight code in markup format.

      Documentation            : LSP_String;
      --  The documentation associated to this proposal, if any.

      Category                 : Language_Category;
      --  The language category associated to this proposal.

      Is_Snippet               : Boolean := False;
      --  True when the proposal is a snippet.
   end record;

   No_Proposal : constant LSP_Completion_Proposal := (others => <>);

   function LSP_Completion_Manager_Factory
     (Kernel : not null GPS.Kernel.Kernel_Handle;
      File   : GNATCOLL.VFS.Virtual_File;
      Lang   : Language.Language_Access) return Completion_Manager_Access;
   --  The LSP completion manager factory.
   --  Return null if there is no LSP server for the given language.

end GPS.LSP_Client.Completion;
