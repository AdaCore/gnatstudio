-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2003                              --
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

with Glib;   use Glib;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;
with GNAT.Regpat; use GNAT.Regpat;
with String_Hash;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Traces;      use Traces;

package body Remote_Connections is

   Me : constant Debug_Handle := Create ("Connections");

   Localhost_Name : constant String := "localhost";

   Passwd_Re : constant Pattern_Matcher_Access := new Pattern_Matcher'
     (Compile ("^.*(password|passphrase.*): *$",
               Case_Insensitive or Multiple_Lines));
   Wrong_Passwd_Re : constant Pattern_Matcher_Access :=
     new Pattern_Matcher'
       (Compile
            ("^.*(Permission denied.|Login incorrect|"
             & "Received signal [0-9]+|Connection (refused|closed)|"
             & "Sorry, try again.).*$",
             Case_Insensitive or Multiple_Lines));
   Login_Re : constant Pattern_Matcher_Access := new Pattern_Matcher'
     (Compile ("(.*ogin|Name \(\w+:\w+\)): *$",
               Case_Insensitive or Multiple_Lines));
   Unknown_Host_Re : constant Pattern_Matcher_Access :=
     new Pattern_Matcher'
       (Compile
            ("Name or service not known", Case_Insensitive or Multiple_Lines));
   Shell_Prompt_Re : constant Pattern_Matcher_Access :=
     new Pattern_Matcher'
       (Compile ("^[^#$%>\n]*[#$%>] *$", Case_Insensitive or Multiple_Lines));
   --  Regexps used in various part of this package. These are inspired from
   --  Emacs's tramp.el mode


   procedure Do_Nothing (Factory : in out Remote_Connection);
   package Factory_Hash is new String_Hash
     (Remote_Connection, Do_Nothing, null);
   use Factory_Hash.String_Hash_Table;

   type Connection_List_Record;
   type Connection_List is access Connection_List_Record;
   type Connection_List_Record is record
      Connection : Remote_Connection;
      Next       : Connection_List;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Connection_List_Record, Connection_List);


   Factories : Factory_Hash.String_Hash_Table.HTable;
   Open_Connections : Connection_List;
   --  ??? Should be stored in a module somewhere

   Longest_Protocol : Natural := 0;
   --  Length of the longest registered protocol

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Factory : in out Remote_Connection) is
      pragma Unreferenced (Factory);
   begin
      null;
   end Do_Nothing;

   --------------
   -- Get_User --
   --------------

   function Get_User
     (Connection : access Remote_Connection_Record) return String is
   begin
      if Connection.Remote_User = null then
         return User_Login_Name;
      else
         return Connection.Remote_User.all;
      end if;
   end Get_User;

   --------------
   -- Get_Host --
   --------------

   function Get_Host
     (Connection : access Remote_Connection_Record) return String is
   begin
      if Connection.Remote_Host = null then
         return Localhost_Name;
      else
         return Connection.Remote_Host.all;
      end if;
   end Get_Host;

   ----------------
   -- Get_Passwd --
   ----------------

   function Get_Passwd
     (Connection : access Remote_Connection_Record) return String is
   begin
      if Connection.Passwd = null then
         return "";
      else
         return Connection.Passwd.all;
      end if;
   end Get_Passwd;

   ----------------
   -- Set_Passwd --
   ----------------

   procedure Set_Passwd
     (Connection : access Remote_Connection_Record; Passwd : String) is
   begin
      Free (Connection.Passwd);
      Connection.Passwd := new String'(Passwd);
   end Set_Passwd;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Connection : access Remote_Connection_Record;
      User, Host : String;
      Passwd     : String := "") is
   begin
      Free (Connection.Remote_Host);
      Connection.Remote_Host := new String'(Host);

      Free (Connection.Remote_User);
      Connection.Remote_User := new String'(User);

      if Passwd /= "" then
         Free (Connection.Passwd);
         Connection.Passwd := new String'(Passwd);
      end if;
   end Initialize;

   -----------------------
   -- Register_Protocol --
   -----------------------

   procedure Register_Protocol
     (Protocol : access Remote_Connection_Record'Class)
   is
      Prot : constant String := Get_Protocol (Protocol);
   begin
      Set (Factories, Prot, Remote_Connection (Protocol));
      if Prot'Length > Longest_Protocol then
         Longest_Protocol := Prot'Length;
      end if;
   end Register_Protocol;

   -----------
   -- Close --
   -----------

   procedure Close
     (Connection : access Remote_Connection_Record;
      GPS_Termination : Boolean)
   is
      pragma Unreferenced (GPS_Termination);
      Tmp : Connection_List := Open_Connections;
      Previous : Connection_List;
   begin
      while Tmp.Connection /= Remote_Connection (Connection) loop
         Previous := Tmp;
         Tmp := Tmp.Next;
      end loop;

      if Tmp /= null then
         if Tmp = Open_Connections then
            Open_Connections := Tmp.Next;
         else
            Previous.Next := Tmp.Next;
         end if;
         Unchecked_Free (Tmp);
      end if;
   end Close;

   ---------------------------
   -- Close_All_Connections --
   ---------------------------

   procedure Close_All_Connections is
      Tmp : Connection_List := Open_Connections;
      Next : Connection_List;
   begin
      while Tmp /= null loop
         Next := Tmp.Next;
         Close (Tmp.Connection, GPS_Termination => True);
         Tmp := Next;
      end loop;
   end Close_All_Connections;

   --------------------
   -- Get_Connection --
   --------------------

   function Get_Connection
     (Protocol, User, Host : String;
      Force_New : Boolean := False) return Remote_Connection
   is
      Tmp  : Connection_List := Open_Connections;
      Fact : Remote_Connection;
      C    : Remote_Connection;
   begin
      Trace (Me, "Get_Connection " & Protocol
             & ' ' & User & ' ' & Host & ' ' & Force_New'Img);

      --  Check open connections first, in case we can reuse
      while Tmp /= null loop
         exit when Get_Protocol (Tmp.Connection) = Protocol
           and then Get_User (Tmp.Connection) = User
           and then Get_Host (Tmp.Connection) = Host;
         Tmp := Tmp.Next;
      end loop;

      if Tmp /= null and then not Force_New then
         C := Factory (Tmp.Connection, User, Host, Get_Passwd (Tmp.Connection),
                       Reuse => True);
      else
         Fact := Get (Factories, Protocol);

         if Fact = null then
            return null;
         end if;

         if Tmp /= null then
            C := Factory (Fact, User, Host, Get_Passwd (Tmp.Connection));
         else
            C := Factory (Fact, User, Host);
         end if;
      end if;

      if Tmp = null or else C /= Tmp.Connection then
         Open_Connections := new Connection_List_Record'
           (Connection => C, Next => Open_Connections);
      end if;

      return C;
   end Get_Connection;

   ---------------
   -- Parse_URL --
   ---------------

   procedure Parse_URL
     (URL         : String;
      Protocol    : out GNAT.OS_Lib.String_Access;
      Remote_User : out GNAT.OS_Lib.String_Access;
      Remote_Host : out GNAT.OS_Lib.String_Access;
      Remote_Path : out GNAT.OS_Lib.String_Access)
   is
      Index   : Natural := URL'First;
      Protocol_End : Natural;
      At_Sign : Natural;
      Max : constant Natural :=
        Integer'Min (URL'Last, URL'First + Longest_Protocol);
   begin
      while Index <= Max
        and then URL (Index) /= ':'
      loop
         Index := Index + 1;
      end loop;

      if Index + 2 > URL'Last
        or else URL (Index .. Index + 2) /= "://"
      then
         Protocol := null;
         Remote_User := null;
         Remote_Host := null;
         Remote_Path := null;
         return;
      end if;

      Protocol := new String'(URL (URL'First .. Index - 1));

      Index := Index + 3;
      Protocol_End := Index;
      At_Sign := Index;

      while Index <= URL'Last
        and then URL (Index) /= '/'
        and then URL (Index) /= Directory_Separator
      loop
         Index := Index + 1;
      end loop;

      while At_Sign < Index and then URL (At_Sign) /= '@' loop
         At_Sign := At_Sign + 1;
      end loop;

      if At_Sign = Protocol_End
        or else At_Sign >= Index
      then
         Remote_User := new String'(User_Login_Name);
      else
         Remote_User := new String'(URL (Protocol_End .. At_Sign - 1));
      end if;

      if At_Sign >= Index then
         At_Sign := Protocol_End - 1;
      end if;

      if At_Sign = Index - 1 then
         Remote_Host := new String'("localhost");
      else
         Remote_Host := new String'(URL (At_Sign + 1 .. Index - 1));
      end if;

      if Index <= URL'Last then
         Remote_Path := new String'(URL (Index .. URL'Last));
      else
         Remote_Path := new String'("/");
      end if;
   end Parse_URL;

   ---------------------
   -- User_Login_Name --
   ---------------------

   function User_Login_Name return String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "user_login_name");
      S : chars_ptr := Internal;
      Result : constant String := Value (S);
   begin
      Free (S);
      return Result;
   end User_Login_Name;

   ------------------
   -- Login_Regexp --
   ------------------

   function Login_Regexp return GNAT.Expect.Pattern_Matcher_Access is
   begin
      return Login_Re;
   end Login_Regexp;

   -------------------
   -- Passwd_Regexp --
   -------------------

   function Passwd_Regexp return GNAT.Expect.Pattern_Matcher_Access is
   begin
      return Passwd_Re;
   end Passwd_Regexp;

   -------------------------
   -- Wrong_Passwd_Regexp --
   -------------------------

   function Wrong_Passwd_Regexp return GNAT.Expect.Pattern_Matcher_Access is
   begin
      return Wrong_Passwd_Re;
   end Wrong_Passwd_Regexp;

   -------------------------
   -- Unknown_Host_Regexp --
   -------------------------

   function Unknown_Host_Regexp return GNAT.Expect.Pattern_Matcher_Access is
   begin
      return Unknown_Host_Re;
   end Unknown_Host_Regexp;

   -------------------------
   -- Shell_Prompt_Regexp --
   -------------------------

   function Shell_Prompt_Regexp return GNAT.Expect.Pattern_Matcher_Access is
   begin
      return Shell_Prompt_Re;
   end Shell_Prompt_Regexp;

end Remote_Connections;
