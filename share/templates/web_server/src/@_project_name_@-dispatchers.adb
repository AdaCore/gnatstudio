
with Ada.Directories;
with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.MIME;
with AWS.Templates;

package body @_Project_Name_@.Dispatchers is

   use Ada;
   use Ada.Strings.Unbounded;

   Web_Root : Unbounded_String;

   ----------------------
   -- Default Dispatch --
   ----------------------

   overriding function Dispatch
     (Dispatcher : in Default; Request : in Status.Data) return Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI          : constant String := Status.URI (Request);
      Translations : Templates.Translate_Set;
   begin
      if URI = "/" then
         Templates.Insert
           (Translations,
            Templates.Assoc ("MESSAGE", "This is the main page"));

         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              (Filename     => To_String (Web_Root) & "tmplt/main.thtml",
               Translations => Translations)));

      else
         return Response.Acknowledge (Messages.S404, "Unknown page");
      end if;
   end Dispatch;

   ------------------
   -- CSS Dispatch --
   ------------------

   overriding function Dispatch
     (Dispatcher : in CSS;
      Request    : in Status.Data) return Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               To_String (Web_Root) & URI (URI'First + 1 .. URI'Last);
   begin
      if Directories.Exists (File) then
         return Response.File
           (Content_Type => MIME.Text_CSS,
            Filename     => File);
      else
         return Response.Acknowledge (Messages.S404);
      end if;
   end Dispatch;

   --------------------
   -- Image Dispatch --
   --------------------

   overriding function Dispatch
     (Dispatcher : in Image;
      Request    : in Status.Data) return Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               To_String (Web_Root) & URI (URI'First + 1 .. URI'Last);
   begin
      if Directories.Exists (File) then
         return Response.File
           (Content_Type => MIME.Content_Type (File),
            Filename     => File);
      else
         return Response.Acknowledge (Messages.S404);
      end if;
   end Dispatch;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Web_Config : in Config.Object) is
   begin
      Web_Root := To_Unbounded_String (Config.WWW_Root (Web_Config));
   end Initialize;

end @_Project_Name_@.Dispatchers;
