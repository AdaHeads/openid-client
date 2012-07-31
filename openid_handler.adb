with
  Ada.Exceptions;
with
  AWS.Messages,
  AWS.Status;
with
  Security.OpenID,
  Yolk.Log;

pragma Elaborate (Security.OpenID);

package body OpenID_Handler is
   protected Data is
      procedure Initialise (Name      : in     String;
                            Return_To : in     String);
      procedure Associate (Name   : in     String);
      function URL return String;
      procedure Validate (Response : in     AWS.Status.Data);
      function Authenticated return Boolean;
   private
      Realm          : Security.OpenID.Manager;
      End_Point      : Security.OpenID.End_Point;
      Association    : Security.OpenID.Association;
      Authentication : Security.OpenID.Authentication;
   end Data;

   protected body Data is
      procedure Initialise (Name      : in     String;
                            Return_To : in     String) is
      begin
         Security.OpenID.Initialize (Realm     => Realm,
                                     Name      => Name,
                                     Return_To => Return_To);
      end Initialise;

      procedure Associate (Name   : in     String) is
      begin
         Security.OpenID.Discover (Realm  => Realm,
                                   Name   => Name,
                                   Result => End_Point);
         Security.OpenID.Associate (Realm  => Realm,
                                    OP     => End_Point,
                                    Result => Association);
      end Associate;

      function URL return String is
      begin
         return Security.OpenID.Get_Authentication_URL (Realm => Realm,
                                                        OP    => End_Point,
                                                        Assoc => Association);
      end URL;

      procedure Validate (Response : in     AWS.Status.Data) is
      begin
         Security.OpenID.Verify (Realm   => Realm,
                                 Assoc   => Association,
                                 Request => Response,
                                 Result  => Authentication);
      end Validate;

      function Authenticated return Boolean is
         use type Security.OpenID.Auth_Result;
      begin
         return Security.OpenID.Get_Status (Authentication) = Security.OpenID.Authenticated;
      end Authenticated;
   end Data;

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      Yolk.Log.Trace
        (Handle  => Yolk.Log.Info,
         Message => "OpenID demo request URI: <" & AWS.Status.URI (Request) &
                    ">");

      if AWS.Status.URI (Request) = "/" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "index.html");
      elsif AWS.Status.URI (Request) = "/error" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "error.html");
      elsif AWS.Status.URI (Request) = "/login" then
         Data.Associate
           (Name => AWS.Status.Parameters (Request).Get ("openid"));

         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Redirecting to <" & Data.URL & ">");

         return AWS.Response.Moved (Data.URL);
      elsif AWS.Status.URI (Request) = "/return_to" then
         Data.Validate (Response => Request);

         if Data.Authenticated then
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Info,
               Message => "Authenticated");
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Info,
               Message => "Redirecting to <http://www.jacob-sparre.dk/>");

            return AWS.Response.Moved ("http://www.jacob-sparre.dk/");
         else
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Info,
               Message => "Not authenticated");
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Info,
               Message => "Redirecting to <https://jaws.adaheads.com/>");

            return AWS.Response.Moved ("https://jaws.adaheads.com/");
         end if;
      elsif AWS.Status.URI (Request) = "/favicon.ico" then
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Redirecting to <http://www.jacob-sparre.dk/icon>");

         return AWS.Response.Moved ("http://www.jacob-sparre.dk/icon");
      else
         Yolk.Log.Trace (Handle  => Yolk.Log.Error,
                         Message => "Unknown document.");

         return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S404);
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
            Message => "Redirecting to <https://jaws.adaheads.com/error>");

         return AWS.Response.Moved ("https://jaws.adaheads.com/error");
   end Service;
begin
   Data.Initialise (Name      => "https://jaws.adaheads.com/",
                    Return_To => "https://jaws.adaheads.com/return_to");
end OpenID_Handler;
