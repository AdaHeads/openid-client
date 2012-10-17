--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Ada.Exceptions,
  Ada.Strings.Unbounded;
with
  AWS.Messages,
  AWS.OpenID.Manual_Dispatching,
  AWS.Status;
with
  Yolk.Log;

package body OpenID_Handler is
   package OpenID is
      new AWS.OpenID.Manual_Dispatching (Host_Name => "jaws.adaheads.com");

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      Yolk.Log.Trace
        (Handle  => Yolk.Log.Info,
         Message => "Request URI: <" & AWS.Status.URI (Request) & ">");

      if OpenID.Is_Authenticated (Request) then
         return AWS.Response.Moved ("http://www.jacob-sparre.dk/?" &
                                    OpenID.Authenticated_As (Request));
      elsif AWS.Status.URI (Request) = OpenID.Log_In.URI then
         return OpenID.Log_In.Service (Request);
      elsif AWS.Status.URI (Request) = OpenID.Validate.URI then
         return OpenID.Validate.Service (Request);
      elsif AWS.Status.URI (Request) = "/" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "index.html");
      elsif AWS.Status.URI (Request) = "/error" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "error.html");
      elsif AWS.Status.URI (Request) = "/style" then
         return AWS.Response.File (Content_Type  => "text/css",
                                   Filename      => "style.css");
      elsif AWS.Status.URI (Request) = "/favicon.ico" then
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Redirecting to <http://www.jacob-sparre.dk/icon>");

         return AWS.Response.Moved ("http://www.jacob-sparre.dk/icon");
      else
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "not_authenticated.html");
      end if;
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "OpenID demo failed at URI <" &
                       AWS.Status.URI (Request) & "> with exception " &
                       Ada.Exceptions.Exception_Name (E) & ": " &
                       Ada.Exceptions.Exception_Message (E));
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Redirecting to </error>");

         return AWS.Response.Moved ("/error");
   end Service;
end OpenID_Handler;
