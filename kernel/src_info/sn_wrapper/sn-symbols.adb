-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

package body SN.Symbols is

   ------------------
   -- Parse_Symbol --
   ------------------

   function Parse_Symbol (Symbol_String : String) return Symbol_Type is
      S : constant String  := Symbol_String & ASCII.NUL;
      P : constant Integer := S'First;
   begin
      case S (P) is
         when 'c' =>
            case S (P + 1) is
               when 'l' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return CL;
                     when others =>
                        return Undef;
                  end case;
               when 'o' =>
                  case S (P + 2) is
                     when 'm' =>
                        case S (P + 3) is
                           when ASCII.NUL =>
                              return COM;
                           when others =>
                              return Undef;
                        end case;
                     when 'n' =>
                        case S (P + 3) is
                           when ASCII.NUL =>
                              return CON;
                           when others =>
                              return Undef;
                        end case;
                     when 'v' =>
                        case S (P + 3) is
                           when ASCII.NUL =>
                              return COV;
                           when others =>
                              return Undef;
                        end case;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 'e' =>
            case S (P + 1) is
               when ASCII.NUL =>
                  return E;
               when 'c' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return EC;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 'f' =>
            case S (P + 1) is
               when 'd' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return FD;
                     when others =>
                        return Undef;
                  end case;
               when 'r' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return FR;
                     when others =>
                        return Undef;
                  end case;
               when 'u' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return FU;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 'g' =>
            case S (P + 1) is
               when 'v' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return GV;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 'i' =>
            case S (P + 1) is
               when 'n' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return SN_IN;
                     when others =>
                        return Undef;
                  end case;
               when 'u' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return IU;
                     when others =>
                        return Undef;
                  end case;
               when 'v' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return IV;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 'l' =>
            case S (P + 1) is
               when 'v' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return LV;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 'm' =>
            case S (P + 1) is
               when 'a' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return MA;
                     when others =>
                        return Undef;
                  end case;
               when 'd' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return MD;
                     when others =>
                        return Undef;
                  end case;
               when 'i' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return MI;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 's' =>
            case S (P + 1) is
               when 'u' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return SU;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when 't' =>
            case S (P + 1) is
               when ASCII.NUL =>
                  return T;
               when others =>
                  return Undef;
            end case;
         when 'u' =>
            case S (P + 1) is
               when 'n' =>
                  case S (P + 2) is
                     when ASCII.NUL =>
                        return UN;
                     when others =>
                        return Undef;
                  end case;
               when others =>
                  return Undef;
            end case;
         when others =>
            return Undef;
      end case;
   end Parse_Symbol;
end SN.Symbols;
