-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2005-2006 AdaCore                  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Glib.Unicode is

   procedure UTF8_Compute
     (Char : Guint8;
      Mask : out Guint32;
      Len  : out Integer);
   --  Compute Mask and Len needed to compute a given UTF8 character

   function UTF8_Get
     (Str  : UTF8_String;
      Mask : Guint32) return Gunichar;
   --  Return a unichar given a UTF8 string

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Char : Gunichar) return Gunichar is
   begin
      if Char <= Character'Pos (Character'Last) then
         return Character'Pos (To_Lower (Character'Val (Char)));
      else
         return Char;
      end if;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Char : Gunichar) return Gunichar is
   begin
      if Char <= Character'Pos (Character'Last) then
         return Character'Pos (To_Upper (Character'Val (Char)));
      else
         return Char;
      end if;
   end To_Upper;

   ------------------
   -- UTF8_Strdown --
   ------------------

   function UTF8_Strdown (Str : UTF8_String) return UTF8_String is
      S     : UTF8_String := Str;
      Index : Natural := S'First;
      Last  : Natural;
   begin
      loop
         Unichar_To_UTF8
           (To_Lower (UTF8_Get_Char (S (Index .. S'Last))),
            S (Index .. S'Last), Last);
         Index := UTF8_Next_Char (S, Index);
         exit when Index > S'Last;
      end loop;
      return S;
   end UTF8_Strdown;

   ----------------
   -- UTF8_Strup --
   ----------------

   function UTF8_Strup (Str : UTF8_String) return UTF8_String is
      S     : UTF8_String := Str;
      Index : Natural := S'First;
      Last  : Natural;
   begin
      loop
         Unichar_To_UTF8
           (To_Upper (UTF8_Get_Char (S (Index .. S'Last))),
            S (Index .. S'Last), Last);
         Index := UTF8_Next_Char (S, Index);
         exit when Index > S'Last;
      end loop;
      return S;
   end UTF8_Strup;

   ------------------
   -- Unichar_Type --
   ------------------

   function Unichar_Type (Char : Gunichar) return G_Unicode_Type is
   begin
      case Char is
         when 16#61# .. 16#7A#
           | 16#AA#
           | 16#B5#
           | 16#BA#
           | 16#DF# .. 16#F6#
           | 16#F8# .. 16#FF#
           | 16#101#
           | 16#103#
           | 16#105#
           | 16#107#
           | 16#109#
           | 16#10B#
           | 16#10D#
           | 16#10F#
           | 16#111#
           | 16#113#
           | 16#115#
           | 16#117#
           | 16#119#
           | 16#11B#
           | 16#11D#
           | 16#11F#
           | 16#121#
           | 16#123#
           | 16#125#
           | 16#127#
           | 16#129#
           | 16#12B#
           | 16#12D#
           | 16#12F#
           | 16#131#
           | 16#133#
           | 16#135#
           | 16#137# .. 16#138#
           | 16#13A#
           | 16#13C#
           | 16#13E#
           | 16#140#
           | 16#142#
           | 16#144#
           | 16#146#
           | 16#148# .. 16#149#
           | 16#14B#
           | 16#14D#
           | 16#14F#
           | 16#151#
           | 16#153#
           | 16#155#
           | 16#157#
           | 16#159#
           | 16#15B#
           | 16#15D#
           | 16#15F#
           | 16#161#
           | 16#163#
           | 16#165#
           | 16#167#
           | 16#169#
           | 16#16B#
           | 16#16D#
           | 16#16F#
           | 16#171#
           | 16#173#
           | 16#175#
           | 16#177#
           | 16#17A#
           | 16#17C#
           | 16#17E# .. 16#180#
           | 16#183#
           | 16#185#
           | 16#188#
           | 16#18C# .. 16#18D#
           | 16#192#
           | 16#195#
           | 16#199# .. 16#19B#
           | 16#19E#
           | 16#1A1#
           | 16#1A3#
           | 16#1A5#
           | 16#1A8#
           | 16#1AA# .. 16#1AB#
           | 16#1AD#
           | 16#1B0#
           | 16#1B4#
           | 16#1B6#
           | 16#1B9# .. 16#1BA#
           | 16#1BD# .. 16#1BF#
           | 16#1C6#
           | 16#1C9#
           | 16#1CC#
           | 16#1CE#
           | 16#1D0#
           | 16#1D2#
           | 16#1D4#
           | 16#1D6#
           | 16#1D8#
           | 16#1DA#
           | 16#1DC# .. 16#1DD#
           | 16#1DF#
           | 16#1E1#
           | 16#1E3#
           | 16#1E5#
           | 16#1E7#
           | 16#1E9#
           | 16#1EB#
           | 16#1ED#
           | 16#1EF# .. 16#1F0#
           | 16#1F3#
           | 16#1F5#
           | 16#1F9#
           | 16#1FB#
           | 16#1FD#
           | 16#1FF#
           | 16#201#
           | 16#203#
           | 16#205#
           | 16#207#
           | 16#209#
           | 16#20B#
           | 16#20D#
           | 16#20F#
           | 16#211#
           | 16#213#
           | 16#215#
           | 16#217#
           | 16#219#
           | 16#21B#
           | 16#21D#
           | 16#21F#
           | 16#221#
           | 16#223#
           | 16#225#
           | 16#227#
           | 16#229#
           | 16#22B#
           | 16#22D#
           | 16#22F#
           | 16#231#
           | 16#233# .. 16#239#
           | 16#23C#
           | 16#23F# .. 16#240#
           | 16#250# .. 16#2AF#
           | 16#390#
           | 16#3AC# .. 16#3CE#
           | 16#3D0# .. 16#3D1#
           | 16#3D5# .. 16#3D7#
           | 16#3D9#
           | 16#3DB#
           | 16#3DD#
           | 16#3DF#
           | 16#3E1#
           | 16#3E3#
           | 16#3E5#
           | 16#3E7#
           | 16#3E9#
           | 16#3EB#
           | 16#3ED#
           | 16#3EF# .. 16#3F3#
           | 16#3F5#
           | 16#3F8#
           | 16#3FB# .. 16#3FC#
           | 16#430# .. 16#45F#
           | 16#461#
           | 16#463#
           | 16#465#
           | 16#467#
           | 16#469#
           | 16#46B#
           | 16#46D#
           | 16#46F#
           | 16#471#
           | 16#473#
           | 16#475#
           | 16#477#
           | 16#479#
           | 16#47B#
           | 16#47D#
           | 16#47F#
           | 16#481#
           | 16#48B#
           | 16#48D#
           | 16#48F#
           | 16#491#
           | 16#493#
           | 16#495#
           | 16#497#
           | 16#499#
           | 16#49B#
           | 16#49D#
           | 16#49F#
           | 16#4A1#
           | 16#4A3#
           | 16#4A5#
           | 16#4A7#
           | 16#4A9#
           | 16#4AB#
           | 16#4AD#
           | 16#4AF#
           | 16#4B1#
           | 16#4B3#
           | 16#4B5#
           | 16#4B7#
           | 16#4B9#
           | 16#4BB#
           | 16#4BD#
           | 16#4BF#
           | 16#4C2#
           | 16#4C4#
           | 16#4C6#
           | 16#4C8#
           | 16#4CA#
           | 16#4CC#
           | 16#4CE#
           | 16#4D1#
           | 16#4D3#
           | 16#4D5#
           | 16#4D7#
           | 16#4D9#
           | 16#4DB#
           | 16#4DD#
           | 16#4DF#
           | 16#4E1#
           | 16#4E3#
           | 16#4E5#
           | 16#4E7#
           | 16#4E9#
           | 16#4EB#
           | 16#4ED#
           | 16#4EF#
           | 16#4F1#
           | 16#4F3#
           | 16#4F5#
           | 16#4F7#
           | 16#4F9#
           | 16#501#
           | 16#503#
           | 16#505#
           | 16#507#
           | 16#509#
           | 16#50B#
           | 16#50D#
           | 16#50F#
           | 16#561# .. 16#587#
           | 16#1D00# .. 16#1D2B#
           | 16#1D62# .. 16#1D77#
           | 16#1D79# .. 16#1D9A#
           | 16#1E01#
           | 16#1E03#
           | 16#1E05#
           | 16#1E07#
           | 16#1E09#
           | 16#1E0B#
           | 16#1E0D#
           | 16#1E0F#
           | 16#1E11#
           | 16#1E13#
           | 16#1E15#
           | 16#1E17#
           | 16#1E19#
           | 16#1E1B#
           | 16#1E1D#
           | 16#1E1F#
           | 16#1E21#
           | 16#1E23#
           | 16#1E25#
           | 16#1E27#
           | 16#1E29#
           | 16#1E2B#
           | 16#1E2D#
           | 16#1E2F#
           | 16#1E31#
           | 16#1E33#
           | 16#1E35#
           | 16#1E37#
           | 16#1E39#
           | 16#1E3B#
           | 16#1E3D#
           | 16#1E3F#
           | 16#1E41#
           | 16#1E43#
           | 16#1E45#
           | 16#1E47#
           | 16#1E49#
           | 16#1E4B#
           | 16#1E4D#
           | 16#1E4F#
           | 16#1E51#
           | 16#1E53#
           | 16#1E55#
           | 16#1E57#
           | 16#1E59#
           | 16#1E5B#
           | 16#1E5D#
           | 16#1E5F#
           | 16#1E61#
           | 16#1E63#
           | 16#1E65#
           | 16#1E67#
           | 16#1E69#
           | 16#1E6B#
           | 16#1E6D#
           | 16#1E6F#
           | 16#1E71#
           | 16#1E73#
           | 16#1E75#
           | 16#1E77#
           | 16#1E79#
           | 16#1E7B#
           | 16#1E7D#
           | 16#1E7F#
           | 16#1E81#
           | 16#1E83#
           | 16#1E85#
           | 16#1E87#
           | 16#1E89#
           | 16#1E8B#
           | 16#1E8D#
           | 16#1E8F#
           | 16#1E91#
           | 16#1E93#
           | 16#1E95# .. 16#1E9B#
           | 16#1EA1#
           | 16#1EA3#
           | 16#1EA5#
           | 16#1EA7#
           | 16#1EA9#
           | 16#1EAB#
           | 16#1EAD#
           | 16#1EAF#
           | 16#1EB1#
           | 16#1EB3#
           | 16#1EB5#
           | 16#1EB7#
           | 16#1EB9#
           | 16#1EBB#
           | 16#1EBD#
           | 16#1EBF#
           | 16#1EC1#
           | 16#1EC3#
           | 16#1EC5#
           | 16#1EC7#
           | 16#1EC9#
           | 16#1ECB#
           | 16#1ECD#
           | 16#1ECF#
           | 16#1ED1#
           | 16#1ED3#
           | 16#1ED5#
           | 16#1ED7#
           | 16#1ED9#
           | 16#1EDB#
           | 16#1EDD#
           | 16#1EDF#
           | 16#1EE1#
           | 16#1EE3#
           | 16#1EE5#
           | 16#1EE7#
           | 16#1EE9#
           | 16#1EEB#
           | 16#1EED#
           | 16#1EEF#
           | 16#1EF1#
           | 16#1EF3#
           | 16#1EF5#
           | 16#1EF7#
           | 16#1EF9#
           | 16#1F00# .. 16#1F07#
           | 16#1F10# .. 16#1F15#
           | 16#1F20# .. 16#1F27#
           | 16#1F30# .. 16#1F37#
           | 16#1F40# .. 16#1F45#
           | 16#1F50# .. 16#1F57#
           | 16#1F60# .. 16#1F67#
           | 16#1F70# .. 16#1F7D#
           | 16#1F80# .. 16#1F87#
           | 16#1F90# .. 16#1F97#
           | 16#1FA0# .. 16#1FA7#
           | 16#1FB0# .. 16#1FB4#
           | 16#1FB6# .. 16#1FB7#
           | 16#1FBE#
           | 16#1FC2# .. 16#1FC4#
           | 16#1FC6# .. 16#1FC7#
           | 16#1FD0# .. 16#1FD3#
           | 16#1FD6# .. 16#1FD7#
           | 16#1FE0# .. 16#1FE7#
           | 16#1FF2# .. 16#1FF4#
           | 16#1FF6# .. 16#1FF7#
           | 16#2071#
           | 16#207F#
           | 16#210A#
           | 16#210E# .. 16#210F#
           | 16#2113#
           | 16#212F#
           | 16#2134#
           | 16#2139#
           | 16#213C# .. 16#213D#
           | 16#2146# .. 16#2149#
           | 16#2C30# .. 16#2C5E#
           | 16#2C81#
           | 16#2C83#
           | 16#2C85#
           | 16#2C87#
           | 16#2C89#
           | 16#2C8B#
           | 16#2C8D#
           | 16#2C8F#
           | 16#2C91#
           | 16#2C93#
           | 16#2C95#
           | 16#2C97#
           | 16#2C99#
           | 16#2C9B#
           | 16#2C9D#
           | 16#2C9F#
           | 16#2CA1#
           | 16#2CA3#
           | 16#2CA5#
           | 16#2CA7#
           | 16#2CA9#
           | 16#2CAB#
           | 16#2CAD#
           | 16#2CAF#
           | 16#2CB1#
           | 16#2CB3#
           | 16#2CB5#
           | 16#2CB7#
           | 16#2CB9#
           | 16#2CBB#
           | 16#2CBD#
           | 16#2CBF#
           | 16#2CC1#
           | 16#2CC3#
           | 16#2CC5#
           | 16#2CC7#
           | 16#2CC9#
           | 16#2CCB#
           | 16#2CCD#
           | 16#2CCF#
           | 16#2CD1#
           | 16#2CD3#
           | 16#2CD5#
           | 16#2CD7#
           | 16#2CD9#
           | 16#2CDB#
           | 16#2CDD#
           | 16#2CDF#
           | 16#2CE1#
           | 16#2CE3# .. 16#2CE4#
           | 16#2D00# .. 16#2D25#
           | 16#FB00# .. 16#FB06#
           | 16#FB13# .. 16#FB17#
           | 16#FF41# .. 16#FF5A#
           | 16#10428# .. 16#1044F#
           | 16#1D41A# .. 16#1D433#
           | 16#1D44E# .. 16#1D454#
           | 16#1D456# .. 16#1D467#
           | 16#1D482# .. 16#1D49B#
           | 16#1D4B6# .. 16#1D4B9#
           | 16#1D4BB#
           | 16#1D4BD# .. 16#1D4C3#
           | 16#1D4C5# .. 16#1D4CF#
           | 16#1D4EA# .. 16#1D503#
           | 16#1D51E# .. 16#1D537#
           | 16#1D552# .. 16#1D56B#
           | 16#1D586# .. 16#1D59F#
           | 16#1D5BA# .. 16#1D5D3#
           | 16#1D5EE# .. 16#1D607#
           | 16#1D622# .. 16#1D63B#
           | 16#1D656# .. 16#1D66F#
           | 16#1D68A# .. 16#1D6A5#
           | 16#1D6C2# .. 16#1D6DA#
           | 16#1D6DC# .. 16#1D6E1#
           | 16#1D6FC# .. 16#1D714#
           | 16#1D716# .. 16#1D71B#
           | 16#1D736# .. 16#1D74E#
           | 16#1D750# .. 16#1D755#
           | 16#1D770# .. 16#1D788#
           | 16#1D78A# .. 16#1D78F#
           | 16#1D7AA# .. 16#1D7C2#
           =>
            return Unicode_Lowercase_Letter;

         when 16#41# .. 16#5A#
           | 16#C0# .. 16#D6#
           | 16#D8# .. 16#DE#
           | 16#100#
           | 16#102#
           | 16#104#
           | 16#106#
           | 16#108#
           | 16#10A#
           | 16#10C#
           | 16#10E#
           | 16#110#
           | 16#112#
           | 16#114#
           | 16#116#
           | 16#118#
           | 16#11A#
           | 16#11C#
           | 16#11E#
           | 16#120#
           | 16#122#
           | 16#124#
           | 16#126#
           | 16#128#
           | 16#12A#
           | 16#12C#
           | 16#12E#
           | 16#130#
           | 16#132#
           | 16#134#
           | 16#136#
           | 16#139#
           | 16#13B#
           | 16#13D#
           | 16#13F#
           | 16#141#
           | 16#143#
           | 16#145#
           | 16#147#
           | 16#14A#
           | 16#14C#
           | 16#14E#
           | 16#150#
           | 16#152#
           | 16#154#
           | 16#156#
           | 16#158#
           | 16#15A#
           | 16#15C#
           | 16#15E#
           | 16#160#
           | 16#162#
           | 16#164#
           | 16#166#
           | 16#168#
           | 16#16A#
           | 16#16C#
           | 16#16E#
           | 16#170#
           | 16#172#
           | 16#174#
           | 16#176#
           | 16#178# .. 16#179#
           | 16#17B#
           | 16#17D#
           | 16#181# .. 16#182#
           | 16#184#
           | 16#186# .. 16#187#
           | 16#189# .. 16#18B#
           | 16#18E# .. 16#191#
           | 16#193# .. 16#194#
           | 16#196# .. 16#198#
           | 16#19C# .. 16#19D#
           | 16#19F# .. 16#1A0#
           | 16#1A2#
           | 16#1A4#
           | 16#1A6# .. 16#1A7#
           | 16#1A9#
           | 16#1AC#
           | 16#1AE# .. 16#1AF#
           | 16#1B1# .. 16#1B3#
           | 16#1B5#
           | 16#1B7# .. 16#1B8#
           | 16#1BC#
           | 16#1C4#
           | 16#1C7#
           | 16#1CA#
           | 16#1CD#
           | 16#1CF#
           | 16#1D1#
           | 16#1D3#
           | 16#1D5#
           | 16#1D7#
           | 16#1D9#
           | 16#1DB#
           | 16#1DE#
           | 16#1E0#
           | 16#1E2#
           | 16#1E4#
           | 16#1E6#
           | 16#1E8#
           | 16#1EA#
           | 16#1EC#
           | 16#1EE#
           | 16#1F1#
           | 16#1F4#
           | 16#1F6# .. 16#1F8#
           | 16#1FA#
           | 16#1FC#
           | 16#1FE#
           | 16#200#
           | 16#202#
           | 16#204#
           | 16#206#
           | 16#208#
           | 16#20A#
           | 16#20C#
           | 16#20E#
           | 16#210#
           | 16#212#
           | 16#214#
           | 16#216#
           | 16#218#
           | 16#21A#
           | 16#21C#
           | 16#21E#
           | 16#220#
           | 16#222#
           | 16#224#
           | 16#226#
           | 16#228#
           | 16#22A#
           | 16#22C#
           | 16#22E#
           | 16#230#
           | 16#232#
           | 16#23A# .. 16#23B#
           | 16#23D# .. 16#23E#
           | 16#241#
           | 16#386#
           | 16#388# .. 16#38A#
           | 16#38C#
           | 16#38E# .. 16#38F#
           | 16#391# .. 16#3A1#
           | 16#3A3# .. 16#3AB#
           | 16#3D2# .. 16#3D4#
           | 16#3D8#
           | 16#3DA#
           | 16#3DC#
           | 16#3DE#
           | 16#3E0#
           | 16#3E2#
           | 16#3E4#
           | 16#3E6#
           | 16#3E8#
           | 16#3EA#
           | 16#3EC#
           | 16#3EE#
           | 16#3F4#
           | 16#3F7#
           | 16#3F9# .. 16#3FA#
           | 16#3FD# .. 16#42F#
           | 16#460#
           | 16#462#
           | 16#464#
           | 16#466#
           | 16#468#
           | 16#46A#
           | 16#46C#
           | 16#46E#
           | 16#470#
           | 16#472#
           | 16#474#
           | 16#476#
           | 16#478#
           | 16#47A#
           | 16#47C#
           | 16#47E#
           | 16#480#
           | 16#48A#
           | 16#48C#
           | 16#48E#
           | 16#490#
           | 16#492#
           | 16#494#
           | 16#496#
           | 16#498#
           | 16#49A#
           | 16#49C#
           | 16#49E#
           | 16#4A0#
           | 16#4A2#
           | 16#4A4#
           | 16#4A6#
           | 16#4A8#
           | 16#4AA#
           | 16#4AC#
           | 16#4AE#
           | 16#4B0#
           | 16#4B2#
           | 16#4B4#
           | 16#4B6#
           | 16#4B8#
           | 16#4BA#
           | 16#4BC#
           | 16#4BE#
           | 16#4C0# .. 16#4C1#
           | 16#4C3#
           | 16#4C5#
           | 16#4C7#
           | 16#4C9#
           | 16#4CB#
           | 16#4CD#
           | 16#4D0#
           | 16#4D2#
           | 16#4D4#
           | 16#4D6#
           | 16#4D8#
           | 16#4DA#
           | 16#4DC#
           | 16#4DE#
           | 16#4E0#
           | 16#4E2#
           | 16#4E4#
           | 16#4E6#
           | 16#4E8#
           | 16#4EA#
           | 16#4EC#
           | 16#4EE#
           | 16#4F0#
           | 16#4F2#
           | 16#4F4#
           | 16#4F6#
           | 16#4F8#
           | 16#500#
           | 16#502#
           | 16#504#
           | 16#506#
           | 16#508#
           | 16#50A#
           | 16#50C#
           | 16#50E#
           | 16#531# .. 16#556#
           | 16#10A0# .. 16#10C5#
           | 16#1E00#
           | 16#1E02#
           | 16#1E04#
           | 16#1E06#
           | 16#1E08#
           | 16#1E0A#
           | 16#1E0C#
           | 16#1E0E#
           | 16#1E10#
           | 16#1E12#
           | 16#1E14#
           | 16#1E16#
           | 16#1E18#
           | 16#1E1A#
           | 16#1E1C#
           | 16#1E1E#
           | 16#1E20#
           | 16#1E22#
           | 16#1E24#
           | 16#1E26#
           | 16#1E28#
           | 16#1E2A#
           | 16#1E2C#
           | 16#1E2E#
           | 16#1E30#
           | 16#1E32#
           | 16#1E34#
           | 16#1E36#
           | 16#1E38#
           | 16#1E3A#
           | 16#1E3C#
           | 16#1E3E#
           | 16#1E40#
           | 16#1E42#
           | 16#1E44#
           | 16#1E46#
           | 16#1E48#
           | 16#1E4A#
           | 16#1E4C#
           | 16#1E4E#
           | 16#1E50#
           | 16#1E52#
           | 16#1E54#
           | 16#1E56#
           | 16#1E58#
           | 16#1E5A#
           | 16#1E5C#
           | 16#1E5E#
           | 16#1E60#
           | 16#1E62#
           | 16#1E64#
           | 16#1E66#
           | 16#1E68#
           | 16#1E6A#
           | 16#1E6C#
           | 16#1E6E#
           | 16#1E70#
           | 16#1E72#
           | 16#1E74#
           | 16#1E76#
           | 16#1E78#
           | 16#1E7A#
           | 16#1E7C#
           | 16#1E7E#
           | 16#1E80#
           | 16#1E82#
           | 16#1E84#
           | 16#1E86#
           | 16#1E88#
           | 16#1E8A#
           | 16#1E8C#
           | 16#1E8E#
           | 16#1E90#
           | 16#1E92#
           | 16#1E94#
           | 16#1EA0#
           | 16#1EA2#
           | 16#1EA4#
           | 16#1EA6#
           | 16#1EA8#
           | 16#1EAA#
           | 16#1EAC#
           | 16#1EAE#
           | 16#1EB0#
           | 16#1EB2#
           | 16#1EB4#
           | 16#1EB6#
           | 16#1EB8#
           | 16#1EBA#
           | 16#1EBC#
           | 16#1EBE#
           | 16#1EC0#
           | 16#1EC2#
           | 16#1EC4#
           | 16#1EC6#
           | 16#1EC8#
           | 16#1ECA#
           | 16#1ECC#
           | 16#1ECE#
           | 16#1ED0#
           | 16#1ED2#
           | 16#1ED4#
           | 16#1ED6#
           | 16#1ED8#
           | 16#1EDA#
           | 16#1EDC#
           | 16#1EDE#
           | 16#1EE0#
           | 16#1EE2#
           | 16#1EE4#
           | 16#1EE6#
           | 16#1EE8#
           | 16#1EEA#
           | 16#1EEC#
           | 16#1EEE#
           | 16#1EF0#
           | 16#1EF2#
           | 16#1EF4#
           | 16#1EF6#
           | 16#1EF8#
           | 16#1F08# .. 16#1F0F#
           | 16#1F18# .. 16#1F1D#
           | 16#1F28# .. 16#1F2F#
           | 16#1F38# .. 16#1F3F#
           | 16#1F48# .. 16#1F4D#
           | 16#1F59#
           | 16#1F5B#
           | 16#1F5D#
           | 16#1F5F#
           | 16#1F68# .. 16#1F6F#
           | 16#1FB8# .. 16#1FBB#
           | 16#1FC8# .. 16#1FCB#
           | 16#1FD8# .. 16#1FDB#
           | 16#1FE8# .. 16#1FEC#
           | 16#1FF8# .. 16#1FFB#
           | 16#2102#
           | 16#2107#
           | 16#210B# .. 16#210D#
           | 16#2110# .. 16#2112#
           | 16#2115#
           | 16#2119# .. 16#211D#
           | 16#2124#
           | 16#2126#
           | 16#2128#
           | 16#212A# .. 16#212D#
           | 16#2130# .. 16#2131#
           | 16#2133#
           | 16#213E# .. 16#213F#
           | 16#2145#
           | 16#2C00# .. 16#2C2E#
           | 16#2C80#
           | 16#2C82#
           | 16#2C84#
           | 16#2C86#
           | 16#2C88#
           | 16#2C8A#
           | 16#2C8C#
           | 16#2C8E#
           | 16#2C90#
           | 16#2C92#
           | 16#2C94#
           | 16#2C96#
           | 16#2C98#
           | 16#2C9A#
           | 16#2C9C#
           | 16#2C9E#
           | 16#2CA0#
           | 16#2CA2#
           | 16#2CA4#
           | 16#2CA6#
           | 16#2CA8#
           | 16#2CAA#
           | 16#2CAC#
           | 16#2CAE#
           | 16#2CB0#
           | 16#2CB2#
           | 16#2CB4#
           | 16#2CB6#
           | 16#2CB8#
           | 16#2CBA#
           | 16#2CBC#
           | 16#2CBE#
           | 16#2CC0#
           | 16#2CC2#
           | 16#2CC4#
           | 16#2CC6#
           | 16#2CC8#
           | 16#2CCA#
           | 16#2CCC#
           | 16#2CCE#
           | 16#2CD0#
           | 16#2CD2#
           | 16#2CD4#
           | 16#2CD6#
           | 16#2CD8#
           | 16#2CDA#
           | 16#2CDC#
           | 16#2CDE#
           | 16#2CE0#
           | 16#2CE2#
           | 16#FF21# .. 16#FF3A#
           | 16#10400# .. 16#10427#
           | 16#1D400# .. 16#1D419#
           | 16#1D434# .. 16#1D44D#
           | 16#1D468# .. 16#1D481#
           | 16#1D49C#
           | 16#1D49E# .. 16#1D49F#
           | 16#1D4A2#
           | 16#1D4A5# .. 16#1D4A6#
           | 16#1D4A9# .. 16#1D4AC#
           | 16#1D4AE# .. 16#1D4B5#
           | 16#1D4D0# .. 16#1D4E9#
           | 16#1D504# .. 16#1D505#
           | 16#1D507# .. 16#1D50A#
           | 16#1D50D# .. 16#1D514#
           | 16#1D516# .. 16#1D51C#
           | 16#1D538# .. 16#1D539#
           | 16#1D53B# .. 16#1D53E#
           | 16#1D540# .. 16#1D544#
           | 16#1D546#
           | 16#1D54A# .. 16#1D550#
           | 16#1D56C# .. 16#1D585#
           | 16#1D5A0# .. 16#1D5B9#
           | 16#1D5D4# .. 16#1D5ED#
           | 16#1D608# .. 16#1D621#
           | 16#1D63C# .. 16#1D655#
           | 16#1D670# .. 16#1D689#
           | 16#1D6A8# .. 16#1D6C0#
           | 16#1D6E2# .. 16#1D6FA#
           | 16#1D71C# .. 16#1D734#
           | 16#1D756# .. 16#1D76E#
           =>
            return Unicode_Uppercase_Letter;

         when others =>
            return Unicode_Unassigned;
      end case;
   end Unichar_Type;

   ------------------
   -- UTF8_Compute --
   ------------------

   procedure UTF8_Compute
     (Char : Guint8;
      Mask : out Guint32;
      Len  : out Integer) is
   begin
      if Char < 128 then
         Len := 1;
         Mask := 16#7F#;

      elsif (Char and 16#E0#) = 16#C0# then
         Len := 2;
         Mask := 16#1F#;

      elsif (Char and 16#F0#) = 16#E0# then
         Len := 3;
         Mask := 16#0F#;

      elsif (Char and 16#F8#) = 16#F0# then
         Len := 4;
         Mask := 16#07#;

      elsif (Char and 16#FC#) = 16#F8# then
         Len := 5;
         Mask := 16#03#;

      elsif (Char and 16#FE#) = 16#FC# then
         Len := 6;
         Mask := 16#01#;

      else
         Len := -1;
      end if;
   end UTF8_Compute;

   --------------
   -- UTF8_Get --
   --------------

   function UTF8_Get
     (Str  : UTF8_String;
      Mask : Guint32) return Gunichar
   is
      Result : Gunichar;
   begin
      Result := Character'Pos (Str (Str'First)) and Gunichar (Mask);

      for J in Str'First + 1 .. Str'Last loop
         if (Guint8'(Character'Pos (Str (J))) and 16#C0#) /= 16#80# then
            return -1;
         end if;

         Result := Result * (2 ** 6);
         Result := Result or (Character'Pos (Str (J)) and 16#3F#);
      end loop;

      return Result;
   end UTF8_Get;

   --------------------
   -- UTF8_Next_Char --
   --------------------

   type Byte is range 1 .. 6;
   type Byte_Array is array (Character) of Byte;

   UTF8_Skip_Data : constant Byte_Array :=
     (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 1, 1);

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural is
   begin
      return Index + Natural (UTF8_Skip_Data (Str (Index)));
   end UTF8_Next_Char;

   -------------------------
   -- UTF8_Find_Prev_Char --
   -------------------------

   function UTF8_Find_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural is
   begin
      for P in Index - 1 .. Str'First loop
         if (Guint8'(Character'Pos (Str (P))) and 16#C0#) /= 16#80# then
            return P;
         end if;
      end loop;

      return Str'First - 1;
   end UTF8_Find_Prev_Char;

   -------------------
   -- UTF8_Get_Char --
   -------------------

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar is
      Mask : Guint32 := 0;
      Len  : Integer;
   begin
      UTF8_Compute (Character'Pos (Str (Str'First)), Mask, Len);

      if Len = -1 then
         return -1;
      end if;

      return UTF8_Get (Str (Str'First .. Str'First + Len - 1), Mask);
   end UTF8_Get_Char;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Char : Gunichar) return Boolean is
   begin
      return Char = Character'Pos (' ')
        or else Char = Character'Pos (ASCII.HT);
   end Is_Space;

   --------------
   -- Is_Alnum --
   --------------

   function Is_Alnum (Char : Gunichar) return Boolean is
   begin
      return Char in Character'Pos ('0') .. Character'Pos ('9')
        or else Char in Character'Pos ('A') .. Character'Pos ('Z')
        or else Char in Character'Pos ('a') .. Character'Pos ('z');
   end Is_Alnum;

   ---------------------
   -- Unichar_To_UTF8 --
   ---------------------

   procedure Unichar_To_UTF8
     (Char : Gunichar;
      Str  : out UTF8_String;
      Last : out Natural)
   is
      First : Gunichar;
      C     : Gunichar := Char;
   begin
      if C < 16#80# then
         First := 0;
         Last  := Str'First;

      elsif C < 16#800# then
         First := 16#C0#;
         Last  := Str'First + 1;

      elsif C < 16#10000# then
         First := 16#E0#;
         Last  := Str'First + 2;

      elsif C < 16#200000# then
         First := 16#F0#;
         Last  := Str'First + 3;

      elsif C < 16#4000000# then
         First := 16#F8#;
         Last  := Str'First + 4;

      else
         First := 16#FC#;
         Last  := Str'First + 5;
      end if;

      for J in reverse Str'First + 1 .. Last loop
         Str (J) := Character'Val ((C and 16#3F#) or 16#80#);
         C := C / (2 ** 6);
      end loop;

      Str (Str'First) := Character'Val (C or First);
   end Unichar_To_UTF8;

end Glib.Unicode;
