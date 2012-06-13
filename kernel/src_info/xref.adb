------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with Entities.Queries; use Entities.Queries;

package body Xref is

   ---------------------------
   --  Note for development --
   ---------------------------

   --  A lot of functions defined here are first attempting to use the
   --  new system (GNATCOLL.Xref) and fallback on the legacy database
   --  (Entities.*).
   --
   --  The plan is to transition all user calls to Entities made in GPS
   --  to use this API, and then to get rid of the legacy database.

   use type Entities.Entity_Information;
   use type Entities.File_Location;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Ref : General_Entity_Reference) return General_Entity
   is
      E : General_Entity;
   begin
      --  Attempt to use the sqlite system

      if Active (Entities.SQLITE)
        and then Ref.Ref /= No_Entity_Reference
      then
         E.Entity := Ref.Ref.Entity;
      end if;

      --  Fall back on the old system

      E.Old_Entity := Entities.Get_Entity (Ref.Old_Ref);

      return E;
   end Get_Entity;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Db     : General_Xref_Database;
      Entity : General_Entity) return String is
   begin
      if Active (Entities.SQLITE)
        and then Entity.Entity /= No_Entity
      then
         return To_String
           (Declaration (Db.Xref.all, Entity.Entity).Name);
      end if;

      if Entity.Old_Entity /= null then
         return Get (Entities.Get_Name (Entity.Old_Entity)).all;
      end if;

      return "";
   end Get_Name;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Ref : General_Entity_Reference) return General_Location is
   begin
      if Active (Entities.SQLITE)
        and then Ref.Ref /= No_Entity_Reference
      then
         return (File => Ref.Ref.File,
                 Line => Ref.Ref.Line,
                 Column => Visible_Column_Type (Ref.Ref.Column));
      end if;

      declare
         Loc : constant Entities.File_Location :=
           Entities.Get_Location (Ref.Old_Ref);
      begin
         return (File => Entities.Get_Filename (Loc.File),
                 Line => Loc.Line,
                 Column => Loc.Column);
      end;
   end Get_Location;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location is
   begin
      if Active (Entities.SQLITE)
        and then Entity.Entity /= No_Entity
      then
         declare
            Ref : Entity_Reference;
         begin
            Ref := Declaration
              (Db.Xref.all,
               Entity.Entity).Location;

            if Ref /= No_Entity_Reference then
               return (File => Ref.File,
                       Line => Ref.Line,
                       Column => Visible_Column_Type (Ref.Column));
            end if;
         end;
      end if;

      if Entity.Old_Entity /= null then
         declare
            Loc : constant Entities.File_Location :=
              Entities.Get_Declaration_Of (Entity.Old_Entity);
         begin
            return (File => Entities.Get_Filename (Loc.File),
                    Line => Loc.Line,
                    Column => Loc.Column);
         end;
      end if;

      return No_Location;
   end Get_Declaration;

   --------------
   -- Get_Body --
   --------------

   function Get_Body
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location is
   begin
      if Active (Entities.SQLITE)
        and then Entity.Entity /= No_Entity
      then
         declare
            C   : References_Cursor;
            Ref : Entity_Reference;
         begin
            Bodies (Db.Xref.all, Entity.Entity, Cursor => C);
            if Has_Element (C) then
               Ref := Element (C);

               if Ref /= No_Entity_Reference then
                  return (File => Ref.File,
                          Line => Ref.Line,
                          Column => Visible_Column_Type (Ref.Column));
               end if;
            end if;
         end;
      end if;

      if Entity.Old_Entity /= null then
         declare
            Loc : Entities.File_Location;
         begin
            Find_Next_Body
              (Entity           => Entity.Old_Entity,
               Current_Location => Entities.No_File_Location,
               Location         => Loc);

            if Loc = Entities.No_File_Location then
               Loc := Entities.Get_Declaration_Of (Entity.Old_Entity);
            end if;

            if Loc /= Entities.No_File_Location then
               return (File => Entities.Get_Filename (Loc.File),
                       Line => Loc.Line,
                       Column => Loc.Column);
            end if;
         end;
      end if;

      return No_Location;
   end Get_Body;

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : General_Entity) is
   begin
      Entities.Ref (Entity.Old_Entity);
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out General_Entity) is
   begin
      Entities.Unref (Entity.Old_Entity);
   end Unref;

end Xref;
