------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;

package HAL is
   pragma Pure;

   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt8 is new Interfaces.Unsigned_8;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt16 is new Interfaces.Unsigned_16;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;
   type UInt32 is new Interfaces.Unsigned_32;
   type UInt33 is mod 2**33
     with Size => 33;
   type UInt34 is mod 2**34
     with Size => 34;
   type UInt35 is mod 2**35
     with Size => 35;
   type UInt36 is mod 2**36
     with Size => 36;
   type UInt37 is mod 2**37
     with Size => 37;
   type UInt38 is mod 2**38
     with Size => 38;
   type UInt39 is mod 2**39
     with Size => 39;
   type UInt40 is mod 2**40
     with Size => 40;
   type UInt41 is mod 2**41
     with Size => 41;
   type UInt42 is mod 2**42
     with Size => 42;
   type UInt43 is mod 2**43
     with Size => 43;
   type UInt44 is mod 2**44
     with Size => 44;
   type UInt45 is mod 2**45
     with Size => 45;
   type UInt46 is mod 2**46
     with Size => 46;
   type UInt47 is mod 2**47
     with Size => 47;
   type UInt48 is mod 2**48
     with Size => 48;
   type UInt49 is mod 2**49
     with Size => 49;
   type UInt50 is mod 2**50
     with Size => 50;
   type UInt51 is mod 2**51
     with Size => 51;
   type UInt52 is mod 2**52
     with Size => 52;
   type UInt53 is mod 2**53
     with Size => 53;
   type UInt54 is mod 2**54
     with Size => 54;
   type UInt55 is mod 2**55
     with Size => 55;
   type UInt56 is mod 2**56
     with Size => 56;
   type UInt57 is mod 2**57
     with Size => 57;
   type UInt58 is mod 2**58
     with Size => 58;
   type UInt59 is mod 2**59
     with Size => 59;
   type UInt60 is mod 2**60
     with Size => 60;
   type UInt61 is mod 2**61
     with Size => 61;
   type UInt62 is mod 2**62
     with Size => 62;
   type UInt63 is mod 2**63
     with Size => 63;
   type UInt64 is new Interfaces.Unsigned_64;

   type UInt8_Array is array (Natural range <>) of UInt8;
   type UInt16_Array is array (Natural range <>) of UInt16;
   type UInt32_Array is array (Natural range <>) of UInt32;

end HAL;
