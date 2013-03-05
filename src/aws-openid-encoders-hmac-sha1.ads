-------------------------------------------------------------------------------
--  The contents of this file originates from the Stephane Carrez project
--  Ada Server Faces (util-encoders-hmac-sha1).
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
--  --  util-encoders-hmac-sha1 -- Compute HMAC-SHA1 authentication code
--  --  Copyright (C) 2011 Stephane Carrez
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
--  --  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
--  --  implied.
--  --  See the License for the specific language governing permissions and
--  --  limitations under the License.
--  -----------------------------------------------------------------------

with Ada.Finalization;
with Ada.Streams;

with AWS.OpenID.Encoders.SHA1;

package AWS.OpenID.Encoders.HMAC.SHA1 is

   type Context is limited private;

   procedure Finish
     (E    : in out Context;
      Hash :    out AWS.OpenID.Encoders.SHA1.Hash_Array);
   --  Computes the HMAC-SHA1 with the private key and the data collected by
   --  the Update procedures. Returns the raw binary hash in Hash.

   procedure Finish
     (E    : in out Context;
      Hash :    out AWS.OpenID.Encoders.SHA1.Digest);
   --  Computes the HMAC-SHA1 with the private key and the data collected by
   --  the Update procedures. Returns the hexadecimal hash in Hash.

   procedure Finish_Base64
     (E    : in out Context;
      Hash :    out AWS.OpenID.Encoders.SHA1.Base64_Digest);
   --  Computes the HMAC-SHA1 with the private key and the data collected by
   --  the Update procedures. Returns the base64 hash in Hash.

   procedure Set_Key
     (E   : in out Context;
      Key : in     String);
   --  Set the hmac private key. The key must be set before calling any
   --  Update procedure.

   procedure Set_Key
     (E   : in out Context;
      Key : in     Ada.Streams.Stream_Element_Array);
   --  Set the hmac private key. The key must be set before calling any
   --  Update procedure.

   function Sign
     (Key  : in String;
      Data : in String)
      return AWS.OpenID.Encoders.SHA1.Digest;
   --  Sign the data string with the key and return the HMAC-SHA1 code as
   --  hexadecimal string.

   function Sign_Base64
     (Key  : in String;
      Data : in String)
      return AWS.OpenID.Encoders.SHA1.Base64_Digest;
   --  Sign the data string with the key and return the HMAC-SHA1 code as
   --  base64 string.

   procedure Update
     (E : in out Context;
      S : in     String);
   --  Update the hash with the string.

   procedure Update
     (E : in out Context;
      S : in     Ada.Streams.Stream_Element_Array);
   --  Update the hash with the string.

private

   type Context is new Ada.Finalization.Limited_Controlled with record
      SHA     : AWS.OpenID.Encoders.SHA1.Context;
      Key     : Ada.Streams.Stream_Element_Array (0 .. 63);
      Key_Len : Ada.Streams.Stream_Element_Offset;
   end record;

   overriding
   procedure Initialize
     (E : in out Context)
   is null;
   --  Initialize the SHA-1 context.

end AWS.OpenID.Encoders.HMAC.SHA1;
