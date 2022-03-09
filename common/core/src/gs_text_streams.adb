------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2022, AdaCore                   --
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

with VSS.Implementation.UTF8_Encoding;
with VSS.Unicode;

package body GS_Text_Streams is

   use type GNATCOLL.VFS.Writable_File;

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out File_UTF8_Output_Stream) is
   begin
      if Self.Writable /= GNATCOLL.VFS.Invalid_File then
         if Self.Last >= Self.Buffer'First then
            GNATCOLL.VFS.Write
              (Self.Writable, Self.Buffer (Self.Buffer'First .. Self.Last));
            Self.Last := Self.Buffer'First - 1;
         end if;

         GNATCOLL.VFS.Close (Self.Writable);
      end if;
   end Close;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self : in out File_UTF8_Output_Stream'Class;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Writable :=  File.Write_File;
   end Open;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out File_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      Code : constant VSS.Unicode.Code_Point :=
        VSS.Characters.Virtual_Character'Pos (Item);
      L    : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      U1   : VSS.Unicode.UTF8_Code_Unit;
      U2   : VSS.Unicode.UTF8_Code_Unit;
      U3   : VSS.Unicode.UTF8_Code_Unit;
      U4   : VSS.Unicode.UTF8_Code_Unit;

   begin
      VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

      if Self.Writable /= GNATCOLL.VFS.Invalid_File then
         declare
            Aux : constant String (1 .. 4) :=
              (Character'Val (U1),
               Character'Val (U2),
               Character'Val (U3),
               Character'Val (U4));

         begin
            if Self.Last + Natural (L) > Self.Buffer'Last then
               begin
                  GNATCOLL.VFS.Write
                    (Self.Writable,
                     Self.Buffer (Self.Buffer'First .. Self.Last));

               exception
                  when others =>
                     Success := False;

                     return;
               end;

               Self.Last := Self.Buffer'First - 1;
            end if;

            Self.Buffer (Self.Last + 1 .. Self.Last + Natural (L)) :=
              Aux (1 .. Natural (L));
            Self.Last := Self.Last + Natural (L);

         exception
            when others =>
               Success := False;
         end;

      else
         Success := False;
      end if;
   end Put;

end GS_Text_Streams;
