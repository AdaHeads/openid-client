-------------------------------------------------------------------------------
--  The contents of this file originates from the Stephane Carrez project
--  Ada Server Faces (security-openid).
--
--  The contents has been altered by AdaHeads K/S. The changes are primarily
--  stylistic plus removal of code that isn't directly used to complete an
--  OpenID authentication process. Changes to the actual code (logic) are not
--  marked specifically. AdaHeads K/S does NOT claim copyright on this file.
--
--  The header from the original file is included here for copyright and
--  license information:
--
--  -----------------------------------------------------------------------
--  --  security-openid -- Open ID 2.0 Support
--  --  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  --  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  --
--  --  Licensed under the Apache License, Version 2.0 (the "License");
--  --  you may not use this file except in compliance with the License.
--  --  You may obtain a copy of the License at
--  --
--  --      http://www.apache.org/licenses/LICENSE-2.0
--  --
--  --  Unless required by applicable law or agreed to in writing, software
--  --  distributed under the License is distributed on an "AS IS" BASIS,
--  --  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  --  See the License for the specific language governing permissions and
--  --  limitations under the License.
--  -----------------------------------------------------------------------

with Ada.Calendar;
with Ada.Finalization;
with Ada.Strings.Unbounded;

with AWS.Status;

package AWS.OpenID.Security is

   Invalid_End_Point : exception;
   Service_Error     : exception;

   type Auth_Result is (Authenticated,
                        Cancel,
                        Setup_Needed,
                        Association_Not_Found,
                        Unknown,
                        Invalid_Signature);

   type End_Point is private;
   --  An End_Point represents the OpenID provider that will authenticate the
   --  user.

   type Association is private;
   subtype Association_Handle is Ada.Strings.Unbounded.Unbounded_String;
   --  The OpenID association contains the shared secret between the relying
   --  party and the OpenID provider. The association can be cached and reused
   --  to authenticate different users using the same OpenID provider. The
   --  association also has an expiration date.

   function Handle
     (Item : in Association)
      return Association_Handle;

   function Handle
     (Response : in AWS.Status.Data)
      return Association_Handle;

   function Is_Expired
     (Item : in Association)
      return Boolean;
   --  Return True if Item is Expired.

   function To_String
     (Assoc : Association)
      return String;
   --  Dump the association as a string (for debugging purposes)

   type Authentication is private;

   function Authenticated
     (Auth : in Authentication)
      return Boolean;

   function Status
     (Auth : in Authentication)
      return Auth_Result;

   function Identity
     (Auth : in Authentication)
      return String;

   type Manager is tagged limited private;

   procedure Initialize
     (Realm     : in out Manager;
      Domain    : in     String;
      Return_To : in     String);
   --  Initialize is called to configure the OpenID realm and set the OpenID
   --  return callback.
   --  Discover is called to retrieve from the OpenID provider the XRDS stream
   --  and identify the provider. An End_Point is returned.
   --  Associate is called to make the association with the End_Point. The
   --  Association record holds the session and authentication.
   --  Get_Authentication_URL builds the provider OpenID authentication URL for
   --  the association.
   --  The user should be redirected to the authentication URL.
   --  The OpenID provider authenticate the user and redirects the user to the
   --  callback.
   --  The association is decoded from the callback parameter.
   --  Verify is called with the association to check the result and obtain the
   --  authentication results.

   procedure Discover
     (Realm  : in out Manager;
      Name   : in     String;
      Result :    out End_Point);
   --  Discover the OpenID provider that is used to authenticate the user.
   --  Name can be an URL or an alias that identifies the provider. A cached
   --  OpenID provider can be returned. (See OpenID Section 7.3 Discovery)

   procedure Associate
     (Realm  : in out Manager;
      OP     : in     End_Point;
      Result :    out Association);
   --  Associate the application (relying party) with the OpenID provider.
   --  The association can be cached.
   --  (See OpenID Section 8 Establishing Associations)

   function Get_Authentication_URL
     (Realm : in Manager;
      OP    : in End_Point;
      Assoc : in Association)
      return String;

   function Verify
     (Realm   : in Manager;
      Assoc   : in Association;
      Request : in AWS.Status.Data)
      return Authentication;

   procedure Verify_Discovered
     (Realm   : in     Manager;
      Assoc   : in     Association;
      Request : in     AWS.Status.Data;
      Result  :    out Authentication);

   procedure Verify_Signature
     (Realm   : in     Manager;
      Assoc   : in     Association;
      Request : in     AWS.Status.Data;
      Result  : in out Authentication);

   procedure Discover_XRDS
     (Realm  : in out Manager;
      URI    : in     String;
      Result :    out End_Point);
   --  Read the XRDS document from the URI and initialize the OpenID provider
   --  end point.

   procedure Extract_XRDS
     (Realm   : in out Manager;
      Content : in     String;
      Result  :    out End_Point);
   --  Extract the OpenID provider URI from the XRDS content.
   --  The default implementation is very basic as it returns the first <URI>
   --  available in the stream without validating the XRDS document.
   --  Raises the Invalid_End_Point exception if the URI cannot be found.

private

   use Ada.Strings.Unbounded;

   type Association is
      record
         Session_Type : Unbounded_String;
         Assoc_Type   : Unbounded_String;
         Assoc_Handle : Association_Handle;
         Mac_Key      : Unbounded_String;
         Expired      : Ada.Calendar.Time;
      end record;

   type Authentication (Status : Auth_Result := Unknown) is
      record
         case Status is
            when Authenticated =>
               Identity   : Unbounded_String;
               Claimed_ID : Unbounded_String;
               Email      : Unbounded_String;
               Full_Name  : Unbounded_String;
               First_Name : Unbounded_String;
               Last_Name  : Unbounded_String;
               Language   : Unbounded_String;
               Country    : Unbounded_String;
               Gender     : Unbounded_String;
               Timezone   : Unbounded_String;
               Nickname   : Unbounded_String;
            when others =>
               null;
         end case;
      end record;

   type End_Point is
      record
         URL     : Unbounded_String;
         Alias   : Unbounded_String;
         Expired : Ada.Calendar.Time;
      end record;

   type Manager is new Ada.Finalization.Limited_Controlled with
      record
         Realm     : Unbounded_String;
         Return_To : Unbounded_String;
      end record;

end AWS.OpenID.Security;
