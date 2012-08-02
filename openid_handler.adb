with
  Ada.Exceptions,
  Ada.Strings.Unbounded;
with
  AWS.Messages,
  AWS.Status;
with
  Association_Database,
  Security.OpenID,
  Yolk.Log;

pragma Elaborate (Security.OpenID);

package body OpenID_Handler is
   Domain : constant String := "https://jaws.adaheads.com/";

   Realm : Security.OpenID.Manager;

   function Log_In (Provider : in     String) return AWS.Response.Data is
      End_Point   : Security.OpenID.End_Point;
      Association : Security.OpenID.Association;
   begin
      Security.OpenID.Discover (Realm  => Realm,
                                Name   => Provider,
                                Result => End_Point);
      Security.OpenID.Associate (Realm  => Realm,
                                 OP     => End_Point,
                                 Result => Association);
      Association_Database.Insert (Item => Association);

      declare
         URL : constant String := Security.OpenID.Get_Authentication_URL
                                    (Realm => Realm,
                                     OP    => End_Point,
                                     Assoc => Association);
      begin
         Yolk.Log.Trace (Handle  => Yolk.Log.Info,
                         Message => "Redirecting to <" & URL & ">");

         return AWS.Response.Moved (URL);
      end;
   end Log_In;

   function Validate (Response       : in     AWS.Status.Data)
     return Security.OpenID.Authentication is
      Handle         : Ada.Strings.Unbounded.Unbounded_String;
      Association    : Security.OpenID.Association;
      Authentication : Security.OpenID.Authentication;
   begin
      Handle := Security.OpenID.Handle (Response => Response);
      Association := Association_Database.Look_Up (Handle => Handle);
      Authentication := Security.OpenID.Verify (Realm   => Realm,
                                                Assoc   => Association,
                                                Request => Response);
      return Authentication;
   end Validate;

   procedure Log (Authentication : in     Security.OpenID.Authentication) is
   begin
      Yolk.Log.Trace (Yolk.Log.Info,
                      "ID:         " &
                      Security.OpenID.Identity (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "Claimed ID: " &
                      Security.OpenID.Claimed_ID (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "E-mail:     " &
                      Security.OpenID.Email (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "First name: " &
                      Security.OpenID.First_Name (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "Last name:  " &
                      Security.OpenID.Last_Name (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "Name:       " &
                      Security.OpenID.Full_Name (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "Language:   " &
                      Security.OpenID.Language (Authentication));
      Yolk.Log.Trace (Yolk.Log.Info,
                      "Country:    " &
                      Security.OpenID.Country (Authentication));
   end Log;

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
         return
           Log_In (Provider => AWS.Status.Parameters (Request).Get ("openid"));
      elsif AWS.Status.URI (Request) = "/return_to" then
         declare
            Authentication : Security.OpenID.Authentication;
         begin
            Authentication := Validate (Response => Request);

            if Security.OpenID.Authenticated (Authentication) then
               Yolk.Log.Trace
                 (Handle  => Yolk.Log.Info,
                  Message => "Authenticated");
               Log (Authentication);
               Yolk.Log.Trace
                 (Handle  => Yolk.Log.Info,
                  Message => "Redirecting to <http://www.jacob-sparre.dk/>");

               return AWS.Response.Moved
                        ("http://www.jacob-sparre.dk/?" &
                         Security.OpenID.Identity (Authentication));
            else
               Yolk.Log.Trace
                 (Handle  => Yolk.Log.Info,
                  Message => "Not authenticated.  Try again.");

               return AWS.Response.Moved (Domain);
            end if;
         end;
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
            Message => "Redirecting to <" & Domain & "error>");

         return AWS.Response.Moved (Domain & "error");
   end Service;
begin
   Security.OpenID.Initialize (Realm  => Realm,
                               Domain => Domain);
end OpenID_Handler;
