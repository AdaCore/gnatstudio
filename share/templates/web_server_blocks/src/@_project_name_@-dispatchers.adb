
with Ada.Directories;
with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.MIME;
with AWS.Services.Web_Block.Registry;
with AWS.Templates;

package body @_Project_Name_@.Dispatchers is

   use Ada;
   use Ada.Strings.Unbounded;

   Web_Root : Unbounded_String;

   ----------------------
   -- Default Dispatch --
   ----------------------

   overriding function Dispatch
     (Dispatcher : in Default; Request : Status.Data) return Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI : constant String := Status.URI (Request);
   begin
      --  Default dispatcher needs to dispatch to Web Block
      return Services.Web_Block.Registry.Build
	(Key          => URI,
	 Request      => Request,
	 Translations => Templates.Null_Set);
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

   -----------------
   -- JS Dispatch --
   -----------------

   overriding function Dispatch
     (Dispatcher : in JS;
      Request    : in Status.Data) return Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               To_String (Web_Root) & URI (URI'First + 1 .. URI'Last);
   begin
      if Directories.Exists (File) then
	 if Directories.Extension (File) = "tjs" then
	    --  This is a template Javascript file, parse it
	    return Response.Build
	      (MIME.Text_Javascript,
	       String'(Templates.Parse
			 (Filename     => File,
			  Translations => Templates.Null_Set)));
	 else
	    return Response.File
	      (Content_Type => MIME.Text_Javascript,
	       Filename     => File);
	 end if;

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
