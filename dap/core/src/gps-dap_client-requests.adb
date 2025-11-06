------------------------------------------------------------------------------
--  GPS.DAP_Client.Requests                                                --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with VSS.Strings.Conversions;

package body GPS.DAP_Client.Requests is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Request_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Request'Class, Request_Access);
   begin
      if Item /= null then
         Item.Finalize;
         Free (Item);
         Item := null;
      end if;
   end Destroy;

   -----------------
   -- On_Rejected --
   -----------------

   procedure On_Rejected (Self : in out Request) is
   begin
      declare
         Method_Name : constant String :=
           Request'Class (Self).Method;
      begin
         Self.Callbacks.Trace
           ("Request rejected: " & Method_Name,
            GPS.DAP_Client.Callbacks.Trace_Warning);
      end;
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   procedure On_Error_Message
     (Self    : in out Request;
      Message : VSS.Strings.Virtual_String) is
      UTF8 : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Message);
      Method_Name : constant String :=
        Request'Class (Self).Method;
   begin
      Self.Callbacks.Trace
        ("Request error: " & Method_Name & " -> " & UTF8,
         GPS.DAP_Client.Callbacks.Trace_Error);
      Self.Callbacks.On_Request_Error
        (Method  => Method_Name,
         Message => UTF8);
   end On_Error_Message;

end GPS.DAP_Client.Requests;
