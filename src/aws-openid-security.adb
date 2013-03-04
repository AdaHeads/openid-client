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

with Ada.Strings.Fixed;

with AWS.Client;
with AWS.Headers;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.Response;

with AWS.OpenID.Encoders;
with AWS.OpenID.Encoders.HMAC.SHA1;
with AWS.OpenID.Encoders.SHA1;
with AWS.OpenID.Log;

package body AWS.OpenID.Security is

   function Extract
     (From      : String;
      Start_Tag : String;
      End_Tag   : String)
      return String;
   --  TODO: write comment

   procedure Extract_Profile
     (Prefix  : in     String;
      Request : in     AWS.Status.Data;
      Result  : in out Authentication);
   --  TODO: write comment

   procedure Extract_Value
     (Into    : in out Unbounded_String;
      Request : in AWS.Status.Data;
      Name    : in String);
   --  TODO: write comment

   function Get_Association_Query
     return String;
   --  TODO: write comment

   procedure Log_Verification
     (Succeeded : in Boolean;
      Message   : in String);
   --  TODO: write comment

   -----------------
   --  Associate  --
   -----------------

   procedure Associate
     (Realm  : in out Manager;
      OP     : in     End_Point;
      Result :    out Association)
   is
      pragma Unreferenced (Realm);

      use AWS.OpenID;
      use AWS.Response;
      use type AWS.Messages.Status_Code;

      Last   : Natural;
      N      : Natural;
      Output : Unbounded_String;
      Params : constant String := Get_Association_Query;
      Pos    : Natural;
      Reply  : AWS.Response.Data;
      URI    : constant String := To_String (OP.URL);
   begin
      Reply := AWS.Client.Post (URL  => URI,
                                Data => Params);

      if Status_Code (Reply) /= AWS.Messages.S200 then
         Log.Error ("Received error " &
                      AWS.Messages.Status_Code'Image (Status_Code (Reply)) &
                      " when creating association with " &
                      URI);

         raise Service_Error with
           "Cannot create association with OpenID provider.";
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
               AWS.OpenID.Log.Debug
                 ("Extracting 'assoc_handle' from result...");
               Result.Assoc_Handle := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "mac_key" then
               Result.Mac_Key := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "expires_in" then
               declare
                  Val : constant String := Slice (Output, N + 1, Last);
                  --  Expires : Integer := Integer'Value (Val);
               begin
                  AWS.OpenID.Log.Info ("Expires: |" & Val & "|");
                  Result.Expired := Ada.Calendar.Clock;
               end;
            elsif Key /= "ns" then
               AWS.OpenID.Log.Error ("Key not recognized: " & Key);
            end if;
         end;

         Pos := Last + 2;
      end loop;

      Log.Debug ("Received end point " & To_String (Output));
   end Associate;

   ---------------------
   --  Authenticated  --
   ---------------------

   function Authenticated
     (Auth : in Authentication)
      return Boolean
   is
   begin
      return Status (Auth) = Authenticated;
   end Authenticated;

   ----------------
   --  Discover  --
   ----------------

   procedure Discover
     (Realm  : in out Manager;
      Name   : in     String;
      Result :    out End_Point)
   is
   begin
      Manager'Class (Realm).Discover_XRDS (URI    => Name,
                                           Result => Result);
   end Discover;

   ---------------------
   --  Discover_XRDS  --
   ---------------------

   procedure Discover_XRDS
     (Realm  : in out Manager;
      URI    : in     String;
      Result :    out End_Point)
   is
      use AWS.Headers.Set;
      use AWS.OpenID;
      use AWS.Response;
      use type AWS.Messages.Status_Code;

      Headers : AWS.Headers.List;
      Reply   : AWS.Response.Data;
   begin
      Log.Info ("Discover XRDS on " & URI);

      Add (Headers, "Accept", "application/xrds+xml");

      Reply := AWS.Client.Get (URL                => URI,
                               Follow_Redirection => True,
                               Headers            => Headers);

      if Status_Code (Reply) /= AWS.Messages.S200 then
         Log.Error ("Received error " &
                      AWS.Messages.Status_Code'Image (Status_Code (Reply)) &
                      " when discovering XRDS on " &
                      URI);

         raise Service_Error with
           "Discovering XRDS of OpenID provider failed.";
      end if;

      Manager'Class (Realm).Extract_XRDS (Content => Message_Body (Reply),
                                          Result  => Result);
   end Discover_XRDS;

   ---------------
   --  Extract  --
   ---------------

   function Extract
     (From      : String;
      Start_Tag : String;
      End_Tag   : String)
      return String
   is
      use Ada.Strings.Fixed;

      Pos     : Natural := Index (From, Start_Tag);
      Last    : Natural;
      URL_Pos : Natural;
   begin
      if Pos = 0 then
         Pos := Index (From,
                       Start_Tag (Start_Tag'First .. Start_Tag'Last - 1));
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

   -----------------------
   --  Extract_Profile  --
   -----------------------

   procedure Extract_Profile
     (Prefix  : in     String;
      Request : in     AWS.Status.Data;
      Result  : in out Authentication)
   is
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
         if Length (Result.First_Name) > 0
           and Length (Result.Last_Name) > 0
         then
            Append (Result.Full_Name, " ");
            Append (Result.Full_Name, Result.Last_Name);
         end if;
      end if;
   end Extract_Profile;

   ---------------------
   --  Extract_Value  --
   ---------------------

   procedure Extract_Value
     (Into    : in out Unbounded_String;
      Request : in     AWS.Status.Data;
      Name    : in     String)
   is
   begin
      if Length (Into) = 0 then
         Into := To_Unbounded_String (AWS.Status.Parameter (Request, Name));
      end if;
   end Extract_Value;

   --------------------
   --  Extract_XRDS  --
   --------------------

   procedure Extract_XRDS
     (Realm   : in out Manager;
      Content : in     String;
      Result  :    out End_Point)
   is
      pragma Unreferenced (Realm);

      URI : constant String := Extract (Content, "<URI>", "</URI>");
   begin
      if URI'Length = 0 then
         AWS.OpenID.Log.Error ("Extract_XRDS: Content = """ & Content & """");

         raise Invalid_End_Point
           with "Cannot extract the <URI> from the XRDS document";
      end if;

      Result.URL := To_Unbounded_String (URI);
   end Extract_XRDS;

   -----------------------------
   --  Get_Association_Query  --
   -----------------------------

   function Get_Association_Query
     return String
   is
   begin
      return "openid.ns=http://specs.openid.net/auth/2.0&"
        & "openid.mode=associate&"
        & "openid.session_type=no-encryption&"
        & "openid.assoc_type=HMAC-SHA1";
   end Get_Association_Query;

   --------------------------
   --  Get_Authentication  --
   --------------------------

   function Get_Authentication_URL
     (Realm : in Manager;
      OP    : in End_Point;
      Assoc : in Association)
      return String
   is
      Result : Unbounded_String := OP.URL;
      Axa    : constant String := "ax";
   begin
      if Index (Result, "?") > 0 then
         Append (Result, "&");
      else
         Append (Result, "?");
      end if;

      Append (Result, "openid.ns=http://specs.openid.net/auth/2.0");
      Append (Result, "&openid.claimed_id=" &
                "http://specs.openid.net/auth/2.0/identifier_select");
      Append (Result, "&openid.identity=" &
                "http://specs.openid.net/auth/2.0/identifier_select");
      Append (Result, "&openid.mode=checkid_setup");
      Append (Result, "&openid.ns." & Axa & "=http://openid.net/srv/ax/1.0");
      Append (Result, "&openid." & Axa & ".mode=fetch_request");
      Append (Result, "&openid." & Axa &
                ".type.email=http://axschema.org/contact/email");
      Append (Result, "&openid." & Axa &
                ".type.fullname=http://axschema.org/namePerson");
      Append (Result, "&openid." & Axa &
                ".type.language=http://axschema.org/pref/language");
      Append (Result, "&openid." & Axa &
                ".type.firstname=http://axschema.org/namePerson/first");
      Append (Result, "&openid." & Axa &
                ".type.lastname=http://axschema.org/namePerson/last");
      Append (Result, "&openid." & Axa &
                ".type.gender=http://axschema.org/person/gender");
      Append (Result, "&openid." & Axa &
                ".required=email,fullname,language,firstname,"
                & "lastname,gender");
      Append (Result, "&openid.ns.sreg=http://openid.net/extensions/sreg/1.1");
      Append (Result, "&openid.sreg.required=" &
                "email,fullname,gender,country,nickname");
      Append (Result, "&openid.return_to=");
      Append (Result, Realm.Return_To);
      Append (Result, "&openid.assoc_handle=");
      Append (Result, Assoc.Assoc_Handle);
      Append (Result, "&openid.realm=");
      Append (Result, Realm.Realm);

      return To_String (Result);
   end Get_Authentication_URL;

   --------------
   --  Handle  --
   --------------

   function Handle
     (Response : in AWS.Status.Data)
      return Association_Handle
   is
   begin
      return To_Unbounded_String
        (AWS.Status.Parameter (Response, "openid.assoc_handle"));
   end Handle;

   --------------
   --  Handle  --
   --------------

   function Handle
     (Item : in Association)
      return Association_Handle
   is
   begin
      return Item.Assoc_Handle;
   end Handle;

   ----------------
   --  Identity  --
   ----------------

   function Identity
     (Auth : in Authentication)
      return String
   is
   begin
      return To_String (Auth.Identity);
   end Identity;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
     (Realm     : in out Manager;
      Domain    : in     String;
      Return_To : in     String)
   is
   begin
      Realm.Realm := To_Unbounded_String (Domain);
      Realm.Return_To := To_Unbounded_String (Domain & Return_To);
   end Initialize;

   ------------------------
   --  Log_Verification  --
   ------------------------

   procedure Log_Verification
     (Succeeded : in Boolean;
      Message   : in String)
   is
      use AWS.OpenID;
   begin
      if Succeeded then
         Log.Info  ("OpenID verification: " & Message);
      else
         Log.Error ("OpenID verification failed: " & Message);
      end if;
   end Log_Verification;

   --------------
   --  Status  --
   --------------

   function Status
     (Auth : in Authentication)
      return Auth_Result
   is
   begin
      return Auth.Status;
   end Status;

   -----------------
   --  To_String  --
   -----------------

   function To_String
     (Assoc : Association)
      return String
   is
   begin
      return "session_type=" & To_String (Assoc.Session_Type)
        & "&assoc_type=" & To_String (Assoc.Assoc_Type)
        & "&assoc_handle=" & To_String (Assoc.Assoc_Handle)
        & "&mac_key=" & To_String (Assoc.Mac_Key);
   end To_String;

   --------------
   --  Verify  --
   --------------

   --  ------------------------------
   --  Verify the authentication result
   --  ------------------------------
   function Verify
     (Realm   : in Manager;
      Assoc   : in Association;
      Request : in AWS.Status.Data)
      return Authentication
   is
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
         Value : constant String :=
                   AWS.Status.Parameter (Request, "openid.return_to");
      begin
         if Value /= Realm.Return_To then
            Log_Verification
              (Succeeded => False,
               Message   => "openid.return_to URL does not match");
            return (Status => Unknown);
         end if;
      end;

      return Result : Authentication do
         --  OpenID Section: 11.3.  Checking the Nonce
         declare
            Value : constant String :=
                      AWS.Status.Parameter (Request, "openid.response_nonce");
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
            Value : constant String :=
                      AWS.Status.Parameter (Request, "openid.ns.sreg");
         begin
            --  Extract profile information
            if Value = "http://openid.net/extensions/sreg/1.1" then
               Extract_Profile ("openid.sreg", Request, Result);
            end if;
         end;

         declare
            Value : constant String :=
                      AWS.Status.Parameter (Request, "openid.ns.ax");
         begin
            if Value = "http://openid.net/srv/ax/1.0" then
               Extract_Profile ("openid.ax.value", Request, Result);
            end if;
         end;

         declare
            Value : constant String :=
                      AWS.Status.Parameter (Request, "openid.ns.ext1");
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

   -------------------------
   --  Verify_Discovered  --
   -------------------------

   procedure Verify_Discovered
     (Realm   : in Manager;
      Assoc   : in Association;
      Request : in AWS.Status.Data;
      Result  : out Authentication)
   is
      pragma Unreferenced (Realm, Assoc);
   begin
      Result.Claimed_ID := To_Unbounded_String
        (AWS.Status.Parameter (Request, "openid.claimed_id"));
      Result.Identity   := To_Unbounded_String
        (AWS.Status.Parameter (Request, "openid.identity"));
   end Verify_Discovered;

   ------------------------
   --  Verify_Signature  --
   ------------------------

   procedure Verify_Signature
     (Realm   : in Manager;
      Assoc   : in Association;
      Request : in AWS.Status.Data;
      Result  : in out Authentication)
   is
      pragma Unreferenced (Realm);

      use Ada.Strings.Fixed;
      use AWS.OpenID;
      use type AWS.OpenID.Encoders.SHA1.Digest;

      Last   : Natural;
      Param  : Unbounded_String;
      Pos    : Natural := 1;
      Sign   : Unbounded_String;
      Signed : constant String := AWS.Status.Parameter (Request,
                                                        "openid.signed");
      Len    : constant Natural := Signed'Length;
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

      Log.Info ("Signing: '" & To_String (Sign) & "'");

      declare
         Decoder : constant Encoders.Encoder :=
                     Encoders.Create (Encoders.Base64_Encoding);
         S       : constant String :=
                     AWS.Status.Parameter (Request, "openid.sig");
         Key     : constant String
           := Decoder.Decode (To_String (Assoc.Mac_Key));
         R       : constant Encoders.SHA1.Base64_Digest :=
                     Encoders.HMAC.SHA1.Sign_Base64 (Key, To_String (Sign));
      begin
         Log.Info ("Signature: " & S & " - " & R);

         if R = S then
            Log_Verification
              (Succeeded => True,
               Message   => "Signatures match.  Authenticated.");

            Result := (Status => Authenticated, others => <>);
         else
            Log_Verification (Succeeded => False,
                              Message   => "Invalid signature.");

            Result := (Status => Invalid_Signature);
         end if;
      end;
   end Verify_Signature;

end AWS.OpenID.Security;
