--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Ada.Strings.Unbounded;
with
  AWS.Cookie;
with
  Association_Database,
  Authentication_Database;

package body AWS.OpenID.Manual_Dispatching is
   Realm : Security.OpenID.Manager;

   package body Log_In is
      function Service (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
         Provider    : String := AWS.Status.Parameters
                                   (Request).Get (Provider_Parameter_Name);
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
            return AWS.Response.URL (URL);
         end;
      end Service;
   end Log_In;

   package body Validate is
      function Service (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
         Handle         : Ada.Strings.Unbounded.Unbounded_String;
         Association    : Security.OpenID.Association;
         Authentication : Security.OpenID.Authentication;
      begin
         Handle := Security.OpenID.Handle (Response => Request);
         Association := Association_Database.Look_Up (Handle => Handle);
         Authentication := Security.OpenID.Verify (Realm   => Realm,
                                                   Assoc   => Association,
                                                   Request => Request);

         return Result : AWS.Response.Data do
            Result :=
              AWS.Response.URL ("https://" & Host_Name & Logged_In.URI);

            if Security.OpenID.Authenticated (Authentication) then
               Authentication_Database.Register_Identity
                 (Source   => Authentication,
                  Request  => Request,
                  Response => Result);
            end if;
         end return;
      end Service;
   end Validate;

   package body Log_Out is
      function Service (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
         Response : AWS.Response.Data;
      begin
         Response :=
           AWS.Response.URL ("https://" & Host_Name & Logged_Out.URI);

         Authentication_Database.Delete_Identity (Request  => Request,
                                                  Response => Response);
         return Response;
      end Service;
   end Log_Out;

   function Is_Authenticated (Request : in AWS.Status.Data) return Boolean is
   begin
      return Authentication_Database.Is_Authenticated (Request => Request);
   end Is_Authenticated;

   function Authenticated_As (Request : in AWS.Status.Data) return String is
   begin
      return Authentication_Database.Identity (Request => Request);
   end Authenticated_As;
begin
   Security.OpenID.Initialize (Realm     => Realm,
                               Domain    => "https://" & Host_Name & "/",
                               Return_To => Return_To_Page);
end AWS.OpenID.Manual_Dispatching;
