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
   private
      Realm       : Security.OpenID.Manager;
      End_Point   : Security.OpenID.End_Point;
      Association : Security.OpenID.Association;
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
   end Data;

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      if AWS.Status.URI (Request) = "/" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "index.html");
      elsif AWS.Status.URI (Request) = "/login" then
         Data.Associate
           (Name => AWS.Status.Parameters (Request).Get ("openid"));
         return AWS.Response.Moved (Data.URL);
      elsif AWS.Status.URI (Request) = "/favicon.ico" then
         return AWS.Response.Moved ("http://www.jacob-sparre.dk/icon");
      else
         return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S404);
      end if;
   exception
      when others =>
         Yolk.Log.Trace (Yolk.Log.Error, "OpenID demo failed.");
         return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S500);
   end Service;
begin
   Data.Initialise (Name      => "AdaHeads K/S",
                    Return_To => "https://localhost/return_to");
end OpenID_Handler;
