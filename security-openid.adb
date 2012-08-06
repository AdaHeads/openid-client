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
with Ada.Strings.Fixed;

with AWS.Client;
with AWS.Headers;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.Response;

with Util.Strings;
with Util.Encoders;
with Util.Encoders.SHA1;
with Util.Encoders.HMAC.SHA1;

with Yolk.Log;

package body Security.OpenID is
   use Ada.Strings.Fixed;

   procedure Extract_Profile (Prefix  : in String;
                              Request : in AWS.Status.Data;
                              Result  : in out Authentication);

   function Extract (From      : String;
                     Start_Tag : String;
                     End_Tag   : String) return String;

   procedure Extract_Value (Into    : in out Unbounded_String;
                            Request : in AWS.Status.Data;
                            Name    : in String);

   function Get_Association_Query return String;

   function Association_Not_Found return Authentication is
   begin
      return (Status => Association_Not_Found);
   end Association_Not_Found;

   function Authenticated (Auth : in Authentication) return Boolean is
   begin
      return Status (Auth) = Authenticated;
   end Authenticated;

   function Status (Auth : in Authentication) return Auth_Result is
   begin
      return Auth.Status;
   end Status;

   function Email (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Email);
   end Email;

   function First_Name (Auth : in Authentication) return String is
   begin
      return To_String (Auth.First_Name);
   end First_Name;

   function Last_Name (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Last_Name);
   end Last_Name;

   function Full_Name (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Full_Name);
   end Full_Name;

   function Identity (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Identity);
   end Identity;

   function Claimed_ID (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Claimed_ID);
   end Claimed_ID;

   function Language (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Language);
   end Language;

   function Country (Auth : in Authentication) return String is
   begin
      return To_String (Auth.Country);
   end Country;

   --  ------------------------------
   --  OpenID Default principal
   --  ------------------------------

   --  ------------------------------
   --  Returns true if the given permission is stored in the user principal.
   --  ------------------------------
   overriding
   function Has_Role (User : in Principal;
                      Role : in Permissions.Role_Type) return Boolean is
      pragma Unreferenced (User, Role);
   begin
      return False;
   end Has_Role;

   overriding
   function Name (From : in Principal) return String is
   begin
      return First_Name (From.Auth) & " " & Last_Name (From.Auth);
   end Name;

   function Email (From : in Principal) return String is
   begin
      return Email (From.Auth);
   end Email;

   function Get_Authentication (From : in Principal) return Authentication is
   begin
      return From.Auth;
   end Get_Authentication;

   procedure Initialize (Realm     : in out Manager;
                         Domain    : in     String;
                         Return_To : in     String := "return_to") is
   begin
      Realm.Realm     := To_Unbounded_String (Domain);
      Realm.Return_To := To_Unbounded_String (Domain & Return_To);
   end Initialize;

   --  ------------------------------
   --  Discover the OpenID provider that must be used to authenticate the user.
   --  The <b>Name</b> can be an URL or an alias that identifies the provider.
   --  A cached OpenID provider can be returned.
   --  (See OpenID Section 7.3 Discovery)
   --  ------------------------------
   procedure Discover (Realm  : in out Manager;
                       Name   : in String;
                       Result : out End_Point) is

   begin
      Manager'Class (Realm).Discover_XRDS (URI    => Name,
                                           Result => Result);
   end Discover;

   --  ------------------------------
   --  Read the XRDS document from the URI and initialize the OpenID provider end point.
   --  ------------------------------
   procedure Discover_XRDS (Realm  : in out Manager;
                            URI    : in String;
                            Result : out End_Point) is
      use AWS.Headers.Set;
      use Yolk.Log;
      use type AWS.Messages.Status_Code;
      Headers : AWS.Headers.List;
      Reply   : AWS.Response.Data;
   begin
      Trace (Info ,"Discover XRDS on " & URI);

      Add (Headers, "Accept", "application/xrds+xml");

      Reply := AWS.Client.Get (URL                => URI,
                               Follow_Redirection => True,
                               Headers            => Headers);

      if AWS.Response.Status_Code (Reply) /= AWS.Messages.S200 then
         Trace (Error,
                "Received error " &
                  AWS.Messages.Status_Code'Image (AWS.Response.Status_Code (Reply)) &
                  " when discovering XRDS on " &
                  URI);
         raise Service_Error with "Discovering XRDS of OpenID provider failed.";
      end if;

      Manager'Class (Realm).Extract_XRDS (Content => AWS.Response.Message_Body (Reply),
                                          Result  => Result);
   end Discover_XRDS;

   function Extract (From      : String;
                     Start_Tag : String;
                     End_Tag   : String) return String is
      Pos  : Natural := Index (From, Start_Tag);
      Last : Natural;
      URL_Pos : Natural;
   begin
      if Pos = 0 then
         Pos := Index (From, Start_Tag (Start_Tag'First .. Start_Tag'Last - 1));
         if Pos = 0 then
            return "";
         end if;
         Pos := Index (From, ">", Pos + 1);
         if Pos = 0 then
            return "";
         end if;
         URL_Pos := Pos + 1;
      else
         URL_Pos := Pos + Start_Tag'Length;
      end if;
      Last := Index (From, End_Tag, Pos);
      if Last <= Pos then
         return "";
      end if;
      return From (URL_Pos .. Last - 1);
   end Extract;

   --  ------------------------------
   --  Extract from the XRDS content the OpenID provider URI.
   --  The default implementation is very basic as it returns the first <URI>
   --  available in the stream without validating the XRDS document.
   --  Raises the <b>Invalid_End_Point</b> exception if the URI cannot be found.
   --  ------------------------------
   procedure Extract_XRDS (Realm   : in out Manager;
                           Content : in String;
                           Result  : out End_Point) is
      pragma Unreferenced (Realm);

      URI : constant String := Extract (Content, "<URI>", "</URI>");
   begin
      if URI'Length = 0 then
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Extract_XRDS: Content = """ & Content & """");
         raise Invalid_End_Point
           with "Cannot extract the <URI> from the XRDS document";
      end if;
      Result.URL := To_Unbounded_String (URI);
   end Extract_XRDS;

   function Get_Association_Query return String is
   begin
      return "openid.ns=http://specs.openid.net/auth/2.0&"
        & "openid.mode=associate&"
        & "openid.session_type=no-encryption&"
        & "openid.assoc_type=HMAC-SHA1";
   end Get_Association_Query;

   --  ------------------------------
   --  Associate the application (relying party) with the OpenID provider.
   --  The association can be cached.
   --  (See OpenID Section 8 Establishing Associations)
   --  ------------------------------
   procedure Associate (Realm  : in out Manager;
                        OP     : in End_Point;
                        Result : out Association) is
      pragma Unreferenced (Realm);

      use Yolk.Log;
      use type AWS.Messages.Status_Code;
      Output : Unbounded_String;
      URI    : constant String := To_String (OP.URL);
      Params : constant String := Get_Association_Query;
      Reply  : AWS.Response.Data;
      Pos, Last, N : Natural;
   begin
      Reply := AWS.Client.Post (URL  => URI,
                                Data => Params);
      if AWS.Response.Status_Code (Reply) /= AWS.Messages.S200 then
         Trace (Error,
                "Received error " &
                  AWS.Messages.Status_Code'Image (AWS.Response.Status_Code (Reply)) &
                  " when creating association with " &
                  URI);
         raise Service_Error with "Cannot create association with OpenID provider.";
      end if;
      Output := AWS.Response.Message_Body (Reply);
      Pos := 1;
      while Pos < Length (Output) loop
         N := Index (Output, ":", Pos);
         exit when N = 0;
         Last := Index (Output, "" & ASCII.LF, N);
         if Last = 0 then
            Last := Length (Output);
         else
            Last := Last - 1;
         end if;
         declare
            Key : constant String := Slice (Output, Pos, N - 1);
         begin
            if Key = "session_type" then
               Result.Session_Type := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "assoc_type" then
               Result.Assoc_Type := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "assoc_handle" then
Yolk.Log.Trace (Yolk.Log.Debug, "Extracting 'assoc_handle' from result...");
               Result.Assoc_Handle := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "mac_key" then
               Result.Mac_Key := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "expires_in" then
               declare
                  Val : constant String := Slice (Output, N + 1, Last);
                  --                    Expires : Integer := Integer'Value (Val);
               begin
                  Yolk.Log.Trace
                    (Handle  => Yolk.Log.Info,
                     Message => "Expires: |" & Val & "|");
                  Result.Expired := Ada.Calendar.Clock;
               end;
            elsif Key /= "ns" then
               Yolk.Log.Trace
                 (Handle  => Yolk.Log.Error,
                  Message => "Key not recognized: " & Key);
            end if;
         end;
         Pos := Last + 2;
      end loop;
      Trace (Debug, "Received end point " & To_String (Output));
   end Associate;

   function Get_Authentication_URL (Realm : in Manager;
                                    OP    : in End_Point;
                                    Assoc : in Association) return String is
      Result : Unbounded_String := OP.URL;
      Axa : constant String := "ax";
   begin
      if Index (Result, "?") > 0 then
         Append (Result, "&");
      else
         Append (Result, "?");
      end if;
      Append (Result, "openid.ns=http://specs.openid.net/auth/2.0");
      Append (Result, "&openid.claimed_id=http://specs.openid.net/auth/2.0/identifier_select");
      Append (Result, "&openid.identity=http://specs.openid.net/auth/2.0/identifier_select");
      Append (Result, "&openid.mode=checkid_setup");
      Append (Result, "&openid.ns." & Axa & "=http://openid.net/srv/ax/1.0");
      Append (Result, "&openid." & Axa & ".mode=fetch_request");
      Append (Result, "&openid." & Axa & ".type.email=http://axschema.org/contact/email");
      Append (Result, "&openid." & Axa & ".type.fullname=http://axschema.org/namePerson");
      Append (Result, "&openid." & Axa & ".type.language=http://axschema.org/pref/language");
      Append (Result, "&openid." & Axa & ".type.firstname=http://axschema.org/namePerson/first");
      Append (Result, "&openid." & Axa & ".type.lastname=http://axschema.org/namePerson/last");
      Append (Result, "&openid." & Axa & ".type.gender=http://axschema.org/person/gender");
      Append (Result, "&openid." & Axa & ".required=email,fullname,language,firstname,"
                & "lastname,gender");
      Append (Result, "&openid.ns.sreg=http://openid.net/extensions/sreg/1.1");
      Append (Result, "&openid.sreg.required=email,fullname,gender,country,nickname");
      Append (Result, "&openid.return_to=");
      Append (Result, Realm.Return_To);
      Append (Result, "&openid.assoc_handle=");
      Append (Result, Assoc.Assoc_Handle);
      Append (Result, "&openid.realm=");
      Append (Result, Realm.Realm);
      return To_String (Result);
   end Get_Authentication_URL;

   procedure Log_Verification (Succeeded : in     Boolean;
                               Message   : in     String) is
      use Yolk.Log;
   begin
      if Succeeded then
         Trace (Info,  "OpenID verification: "        & Message);
      else
         Trace (Error, "OpenID verification failed: " & Message);
      end if;
   end Log_Verification;

   procedure Extract_Value (Into    : in out Unbounded_String;
                            Request : in AWS.Status.Data;
                            Name    : in String) is
   begin
      if Length (Into) = 0 then
         Into := To_Unbounded_String (AWS.Status.Parameter (Request, Name));
      end if;
   end Extract_Value;

   procedure Extract_Profile (Prefix  : in String;
                              Request : in AWS.Status.Data;
                              Result  : in out Authentication) is
   begin
      Extract_Value (Result.Email,      Request, Prefix & ".email");
      Extract_Value (Result.Nickname,   Request, Prefix & ".nickname");
      Extract_Value (Result.Gender,     Request, Prefix & ".gender");
      Extract_Value (Result.Country,    Request, Prefix & ".country");
      Extract_Value (Result.Language,   Request, Prefix & ".language");
      Extract_Value (Result.Full_Name,  Request, Prefix & ".fullname");
      Extract_Value (Result.Timezone,   Request, Prefix & ".timezone");
      Extract_Value (Result.First_Name, Request, Prefix & ".firstname");
      Extract_Value (Result.Last_Name,  Request, Prefix & ".lastname");

      --  If the fullname is not specified, try to build one from the
      --  first_name and last_name.
      if Length (Result.Full_Name) = 0 then
         Append (Result.Full_Name, Result.First_Name);
         if Length (Result.First_Name) > 0 and Length (Result.Last_Name) > 0 then
            Append (Result.Full_Name, " ");
            Append (Result.Full_Name, Result.Last_Name);
         end if;
      end if;
   end Extract_Profile;

   --  ------------------------------
   --  Verify the authentication result
   --  ------------------------------
   function Verify (Realm   : in Manager;
                    Assoc   : in Association;
                    Request : in AWS.Status.Data) return Authentication is
      Mode : constant String := AWS.Status.Parameter (Request, "openid.mode");
   begin
      --  Step 1: verify the response status
      if Mode = "cancel" then
         Log_Verification (Succeeded => False,
                           Message   => "Authentication refused");
         return (Status => Cancel);
      end if;

      if Mode = "setup_needed" then
         Log_Verification (Succeeded => False,
                           Message   => "Setup is needed");
         return (Status => Setup_Needed);
      end if;

      if Mode /= "id_res" then
         Log_Verification (Succeeded => False,
                           Message   => "Setup is needed");
         return (Status => Unknown);
      end if;

      --  OpenID Section: 11.1.  Verifying the Return URL
      declare
         Value : constant String := AWS.Status.Parameter (Request,
                                                          "openid.return_to");
      begin
         if Value /= Realm.Return_To then
            Log_Verification
              (Succeeded => False,
               Message   => "openid.return_to URL does not match");
            return (Status => Unknown);
         end if;
      end;

      return Result : Authentication do
        --  OpenID Section: 11.2.  Verifying Discovered Information
        --  Manager'Class (Realm).Verify_Discovered (Assoc, Request, Result);
        --  /\_only copies (unchecked) information

        --  OpenID Section: 11.3.  Checking the Nonce
        declare
           Value : constant String := AWS.Status.Parameter
                                        (Request, "openid.response_nonce");
        begin
           if Value = "" then
              Log_Verification
                (Succeeded => False,
                 Message   => "openid.response_nonce is empty");
              Result := (Status => Unknown);
              return;
           end if;
        end;

        --  OpenID Section: 11.4.  Verifying Signatures
        Manager'Class (Realm).Verify_Signature (Assoc, Request, Result);

        declare
           Value : constant String := AWS.Status.Parameter (Request,
                                                            "openid.ns.sreg");
        begin
           --  Extract profile information
           if Value = "http://openid.net/extensions/sreg/1.1" then
              Extract_Profile ("openid.sreg", Request, Result);
           end if;
        end;

        declare
           Value : constant String := AWS.Status.Parameter (Request,
                                                            "openid.ns.ax");
        begin
           if Value = "http://openid.net/srv/ax/1.0" then
              Extract_Profile ("openid.ax.value", Request, Result);
           end if;
        end;

        declare
           Value : constant String := AWS.Status.Parameter (Request,
                                                            "openid.ns.ext1");
        begin
           if Value = "http://openid.net/srv/ax/1.0" then
              Extract_Profile ("openid.ext1.value", Request, Result);
           end if;
        end;

        if Result.Status = Authenticated then
           Manager'Class (Realm).Verify_Discovered (Assoc, Request, Result);
        end if;
      end return;
   end Verify;

   procedure Verify_Signature (Realm   : in Manager;
                               Assoc   : in Association;
                               Request : in AWS.Status.Data;
                               Result  : in out Authentication) is
      pragma Unreferenced (Realm);

      use Yolk.Log;
      use type Util.Encoders.SHA1.Digest;

      Signed : constant String := AWS.Status.Parameter (Request,
                                                        "openid.signed");
      Len    : constant Natural := Signed'Length;
      Sign   : Unbounded_String;
      Param  : Unbounded_String;
      Pos    : Natural := 1;
      Last   : Natural;
   begin
      while Pos < Len loop
         Last := Index (Signed, ",", Pos);
         if Last > 0 then
            Param := To_Unbounded_String (Signed (Pos .. Last - 1));
            Pos  := Last + 1;
         else
            Param := To_Unbounded_String (Signed (Pos .. Len));
            Pos  := Len + 1;
         end if;
         declare
            Name  : constant String := "openid." & To_String (Param);
            Value : constant String := AWS.Status.Parameter (Request, Name);
         begin
            Append (Sign, Param);
            Append (Sign, ':');
            Append (Sign, Value);
            Append (Sign, ASCII.LF);
         end;
      end loop;
      Trace (Info, "Signing: '" & To_String (Sign) & "'");

      declare
         Decoder : constant Util.Encoders.Encoder := Util.Encoders.Create (Util.Encoders.BASE_64);
         S       : constant String := AWS.Status.Parameter (Request, "openid.sig");
         Key     : constant String := Decoder.Decode (To_String (Assoc.Mac_Key));

         R : constant Util.Encoders.SHA1.Base64_Digest
           := Util.Encoders.HMAC.SHA1.Sign_Base64 (Key, To_String (Sign));
      begin
         Trace (Info, "Signature: " & S & " - " & R);
         if R = S then
            Log_Verification (Succeeded => True,
                              Message   => "Signatures match.  Authenticated.");
            Result := (Status => Authenticated,
                       others => <>);
         else
            Log_Verification (Succeeded => False,
                              Message   => "Invalid signature.");
            Result := (Status => Invalid_Signature);
         end if;
      end;
   end Verify_Signature;

   --  ------------------------------
   --  Verify the authentication result
   --  ------------------------------
   procedure Verify_Discovered (Realm   : in Manager;
                                Assoc   : in Association;
                                Request : in AWS.Status.Data;
                                Result  : out Authentication) is
      pragma Unreferenced (Realm, Assoc);
   begin
      Result.Claimed_ID := To_Unbounded_String (AWS.Status.Parameter (Request, "openid.claimed_id"));
      Result.Identity   := To_Unbounded_String (AWS.Status.Parameter (Request, "openid.identity"));
   end Verify_Discovered;

   function To_String (OP : End_Point) return String is
   begin
      return "openid://" & To_String (OP.URL);
   end To_String;

   function Handle (Item : in Association) return Association_Handle is
   begin
      return Item.Assoc_Handle;
   end Handle;

   function Handle (Response : in AWS.Status.Data) return Association_Handle is
   begin
      return To_Unbounded_String (AWS.Status.Parameter (Response,
                                                        "openid.assoc_handle"));
   end Handle;

   function To_String (Assoc : Association) return String is
   begin
      return "session_type=" & To_String (Assoc.Session_Type)
        & "&assoc_type=" & To_String (Assoc.Assoc_Type)
        & "&assoc_handle=" & To_String (Assoc.Assoc_Handle)
        & "&mac_key=" & To_String (Assoc.Mac_Key);
   end To_String;
end Security.Openid;
