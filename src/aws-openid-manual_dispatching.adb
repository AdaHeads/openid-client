-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Association_Database;
with Authentication_Database;

with AWS.OpenID.Error_Messages;
private with AWS.OpenID.Security;

package body AWS.OpenID.Manual_Dispatching is

   Realm : AWS.OpenID.Security.Manager;

   package body Log_In is

      ---------------
      --  Service  --
      ---------------

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data
      is
         End_Point   : AWS.OpenID.Security.End_Point;
         Association : AWS.OpenID.Security.Association;
      begin
         declare
            Provider : constant String
              := AWS.Status.Parameters (Request).Get (Provider_Parameter_Name);
         begin
            AWS.OpenID.Security.Discover (Realm  => Realm,
                                          Name   => Provider,
                                          Result => End_Point);
         exception
            when Constraint_Error =>
               return AWS.OpenID.Error_Messages.Invalid_URL (Provider);
            when AWS.OpenID.Security.Invalid_End_Point =>
               return AWS.OpenID.Error_Messages.Invalid_End_Point (Provider);
            when AWS.OpenID.Security.Service_Error =>
               return AWS.OpenID.Error_Messages.Provider_Off_Line (Provider);
         end;

         AWS.OpenID.Security.Associate (Realm  => Realm,
                                        OP     => End_Point,
                                        Result => Association);
         Association_Database.Insert (Item => Association);

         declare
            URL : constant String := AWS.OpenID.Security.Get_Authentication_URL
              (Realm => Realm,
               OP    => End_Point,
               Assoc => Association);
         begin
            return AWS.Response.URL (URL);
         end;
      end Service;

   end Log_In;

   package body Validate is

      ---------------
      --  Service  --
      ---------------

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data
      is
         Handle         : Ada.Strings.Unbounded.Unbounded_String;
         Association    : AWS.OpenID.Security.Association;
         Authentication : AWS.OpenID.Security.Authentication;
      begin
         Handle := AWS.OpenID.Security.Handle (Response => Request);
         Association := Association_Database.Look_Up (Handle => Handle);
         Authentication := AWS.OpenID.Security.Verify (Realm   => Realm,
                                                       Assoc   => Association,
                                                       Request => Request);

         if AWS.OpenID.Security.Authenticated (Authentication) then
            return Result : AWS.Response.Data do
               Result :=
                 AWS.Response.URL (Protocol & Host_Name & Logged_In.URI);

               Authentication_Database.Register_Identity
                 (Source   => Authentication,
                  Request  => Request,
                  Response => Result);
            end return;
         else
            return AWS.OpenID.Error_Messages.Authentication_Failed;
         end if;
      end Service;

   end Validate;

   package body Log_Out is

      ---------------
      --  Service  --
      ---------------

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data
      is
         Response : AWS.Response.Data;
      begin
         Response :=
           AWS.Response.URL (Protocol & Host_Name & Logged_Out.URI);

         Authentication_Database.Delete_Identity (Request  => Request,
                                                  Response => Response);
         return Response;
      end Service;

   end Log_Out;

   ------------------------
   --  Authenticated_As  --
   ------------------------

   function Authenticated_As
     (Request : in AWS.Status.Data)
      return String
   is
   begin
      return Authentication_Database.Identity (Request => Request);
   end Authenticated_As;

   ------------------------
   --  Is_Authenticated  --
   ------------------------

   function Is_Authenticated
     (Request : in AWS.Status.Data)
      return Boolean
   is
   begin
      return Authentication_Database.Is_Authenticated (Request => Request);
   end Is_Authenticated;

begin

   AWS.OpenID.Security.Initialize (Realm     => Realm,
                                   Domain    => Protocol & Host_Name,
                                   Return_To => Return_To_Page);

end AWS.OpenID.Manual_Dispatching;
