------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package provides base tagged types for tag handlers and tag handlers
--  registry.

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

with GNATdoc.Customization.Markup_Generators;
use GNATdoc.Customization.Markup_Generators;

package GNATdoc.Customization.Tag_Handlers is

   type Abstract_Tag_Handler is abstract tagged limited private;

   type Tag_Handler_Access is access all Abstract_Tag_Handler'Class;

   not overriding function Name
     (Self : Abstract_Tag_Handler) return String is abstract;
   --  Returns name of the tag

   type Abstract_Inline_Tag_Handler is
     abstract new Abstract_Tag_Handler with private;

   type Inline_Tag_Handler_Access is
     access all Abstract_Inline_Tag_Handler'Class;

   not overriding function Has_Parameter
     (Self : Abstract_Inline_Tag_Handler) return Boolean is abstract;
   --  Returns True when first word after tag should be processed as tag's
   --  argument.

   not overriding procedure To_Markup
     (Self      : in out Abstract_Inline_Tag_Handler;
      Parameter : String;
      Writer    : in out Markup_Generator) is abstract;
   --  Called to process tag

   procedure Register (Handler : Tag_Handler_Access);
   --  Registers tag handler

   package Unbounded_String_Sets is
     new Ada.Containers.Hashed_Sets
       (Unbounded_String, Ada.Strings.Unbounded.Hash, "=");

   function Get_Inline_Tags return Unbounded_String_Sets.Set;
   --  Returns list of registered tags

   function Get_Inline_Tag_Handler
     (Name : String) return Inline_Tag_Handler_Access;
   --  Returns inline tag handler

private

   type Abstract_Tag_Handler is abstract tagged limited null record;

   type Abstract_Inline_Tag_Handler is
     abstract new Abstract_Tag_Handler with null record;

end GNATdoc.Customization.Tag_Handlers;
