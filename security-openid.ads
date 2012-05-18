-----------------------------------------------------------------------
--  security-openid -- Open ID 2.0 Support
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Finalization;

with AWS.Status;
with Security.Permissions;

--  The <b>Security.Openid</b> package implements an authentication framework based
--  on OpenID 2.0.
--
--  See OpenID Authentication 2.0 - Final
--  http://openid.net/specs/openid-authentication-2_0.html
package Security.Openid is

   Invalid_End_Point : exception;

   Service_Error     : exception;

   --  ------------------------------
   --  OpenID provider
   --  ------------------------------
   --  The <b>End_Point</b> represents the OpenID provider that will authenticate
   --  the user.
   type End_Point is private;

   function To_String (OP : End_Point) return String;

   --  ------------------------------
   --  Association
   --  ------------------------------
   --  The OpenID association contains the shared secret between the relying party
   --  and the OpenID provider.  The association can be cached and reused to authenticate
   --  different users using the same OpenID provider.  The association also has an
   --  expiration date.
   type Association is private;

   --  Dump the association as a string (for debugging purposes)
   function To_String (Assoc : Association) return String;

   type Auth_Result is (AUTHENTICATED, CANCEL, SETUP_NEEDED, UNKNOWN, INVALID_SIGNATURE);

   --  ------------------------------
   --  OpenID provider
   --  ------------------------------
   --
   type Authentication is private;

   --  Get the email address
   function Get_Email (Auth : in Authentication) return String;

   --  Get the user first name.
   function Get_First_Name (Auth : in Authentication) return String;

   --  Get the user last name.
   function Get_Last_Name (Auth : in Authentication) return String;

   --  Get the user full name.
   function Get_Full_Name (Auth : in Authentication) return String;

   --  Get the user identity.
   function Get_Identity (Auth : in Authentication) return String;

   --  Get the user claimed identity.
   function Get_Claimed_Id (Auth : in Authentication) return String;

   --  Get the user language.
   function Get_Language (Auth : in Authentication) return String;

   --  Get the user country.
   function Get_Country (Auth : in Authentication) return String;

   --  Get the result of the authentication.
   function Get_Status (Auth : in Authentication) return Auth_Result;

   --  ------------------------------
   --  OpenID Default principal
   --  ------------------------------
   type Principal is new Security.Permissions.Principal with private;
   type Principal_Access is access all Principal'Class;

   --  Returns true if the given role is stored in the user principal.
   overriding
   function Has_Role (User : in Principal;
                      Role : in Permissions.Role_Type) return Boolean;

   --  Get the principal name.
   overriding
   function Get_Name (From : in Principal) return String;

   --  Get the user email address.
   function Get_Email (From : in Principal) return String;

   --  Get the authentication data.
   function Get_Authentication (From : in Principal) return Authentication;

   --  ------------------------------
   --  OpenID Manager
   --  ------------------------------
   --  The process is the following:
   --
   --  o <b>Initialize</b> is called to configure the OpenID realm and set the
   --    OpenID return callback CB.
   --  o <b>Discover</b> is called to retrieve from the OpenID provider the XRDS
   --    stream and identify the provider.  An <b>End_Point</b> is returned.
   --  o <b>Associate</b> is called to make the association with the <b>End_Point</b>.
   --    The <b>Association</b> record holds session, and authentication.
   --  o <b>Get_Authentication_URL</b> builds the provider OpenID authentication
   --    URL for the association.
   --  o The user should be redirected to the authentication URL.
   --  o The OpenID provider authenticate the user and redirects the user to the callback CB.
   --  o The association is decoded from the callback parameter.
   --  o <b>Verify</b> is called with the association to check the result and
   --    obtain the authentication results.
   type Manager is tagged limited private;

   --  Initialize the OpenID realm.
   procedure Initialize (Realm     : in out Manager;
                         Name      : in String;
                         Return_To : in String);

   --  Discover the OpenID provider that must be used to authenticate the user.
   --  The <b>Name</b> can be an URL or an alias that identifies the provider.
   --  A cached OpenID provider can be returned.
   --  (See OpenID Section 7.3 Discovery)
   procedure Discover (Realm  : in out Manager;
                       Name   : in String;
                       Result : out End_Point);

   --  Associate the application (relying party) with the OpenID provider.
   --  The association can be cached.
   --  (See OpenID Section 8 Establishing Associations)
   procedure Associate (Realm  : in out Manager;
                        OP     : in End_Point;
                        Result : out Association);

   function Get_Authentication_URL (Realm : in Manager;
                                    OP    : in End_Point;
                                    Assoc : in Association) return String;

   --  Verify the authentication result
   procedure Verify (Realm   : in out Manager;
                     Assoc   : in Association;
                     Request : in AWS.Status.Data;
                     Result  : out Authentication);

   --  Verify the authentication result
   procedure Verify_Discovered (Realm   : in out Manager;
                                Assoc   : in Association;
                                Request : in AWS.Status.Data;
                                Result  : out Authentication);

   --  Verify the signature part of the result
   procedure Verify_Signature (Realm   : in Manager;
                               Assoc   : in Association;
                               Request : in AWS.Status.Data;
                               Result  : in out Authentication);

   --  Read the XRDS document from the URI and initialize the OpenID provider end point.
   procedure Discover_XRDS (Realm  : in out Manager;
                            URI    : in String;
                            Result : out End_Point);

   --  Extract from the XRDS content the OpenID provider URI.
   --  The default implementation is very basic as it returns the first <URI>
   --  available in the stream without validating the XRDS document.
   --  Raises the <b>Invalid_End_Point</b> exception if the URI cannot be found.
   procedure Extract_XRDS (Realm   : in out Manager;
                           Content : in String;
                           Result  : out End_Point);

private

   use Ada.Strings.Unbounded;

   type Association is record
      Session_Type : Unbounded_String;
      Assoc_Type   : Unbounded_String;
      Assoc_Handle : Unbounded_String;
      Mac_Key      : Unbounded_String;
      Expired      : Ada.Calendar.Time;
   end record;

   type Authentication is record
      Status     : Auth_Result;
      Identity   : Unbounded_String;
      Claimed_Id : Unbounded_String;
      Email      : Unbounded_String;
      Full_Name  : Unbounded_String;
      First_Name : Unbounded_String;
      Last_Name  : Unbounded_String;
      Language   : Unbounded_String;
      Country    : Unbounded_String;
      Gender     : Unbounded_String;
      Timezone   : Unbounded_String;
      Nickname   : Unbounded_String;
   end record;

   type End_Point is record
      URL        : Unbounded_String;
      Alias      : Unbounded_String;
      Expired    : Ada.Calendar.Time;
   end record;

   type Manager is new Ada.Finalization.Limited_Controlled with record
      Realm     : Unbounded_String;
      Return_To : Unbounded_String;
   end record;

   type Principal is new Security.Permissions.Principal with record
      Auth : Authentication;
   end record;
end Security.Openid;
