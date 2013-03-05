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

with AWS.OpenID.Encoders.Base16;
with AWS.OpenID.Encoders.Base64;

package body AWS.OpenID.Encoders.HMAC.SHA1 is

   IPAD : constant Ada.Streams.Stream_Element := 16#36#;
   OPAD : constant Ada.Streams.Stream_Element := 16#5c#;

   --------------
   --  Finish  --
   --------------

   procedure Finish
     (E    : in out Context;
      Hash :    out AWS.OpenID.Encoders.SHA1.Hash_Array)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      AWS.OpenID.Encoders.SHA1.Finish (E.SHA, Hash);

      --  Hash the key in the SHA1 context.
      declare
         Block : Ada.Streams.Stream_Element_Array (0 .. 63);
      begin
         for I in 0 .. E.Key_Len loop
            Block (I) := OPAD xor E.Key (I);
         end loop;

         if E.Key_Len < 63 then
            for I in E.Key_Len + 1 .. 63 loop
               Block (I) := OPAD;
            end loop;
         end if;

         AWS.OpenID.Encoders.SHA1.Update (E.SHA, Block);
      end;

      AWS.OpenID.Encoders.SHA1.Update (E.SHA, Hash);
      AWS.OpenID.Encoders.SHA1.Finish (E.SHA, Hash);
   end Finish;

   --------------
   --  Finish  --
   --------------

   procedure Finish
     (E    : in out Context;
      Hash :    out AWS.OpenID.Encoders.SHA1.Digest)
   is
      Buf : Ada.Streams.Stream_Element_Array (1 .. Hash'Length);
      for Buf'Address use Hash'Address;
      pragma Import (Ada, Buf);

      B       : AWS.OpenID.Encoders.Base16.Encoder;
      Encoded : Ada.Streams.Stream_Element_Offset;
      H       : AWS.OpenID.Encoders.SHA1.Hash_Array;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      Finish (E, H);
      B.Transform (Data => H, Into => Buf, Last => Last, Encoded => Encoded);
   end Finish;

   ---------------------
   --  Finish_Base64  --
   ---------------------

   procedure Finish_Base64
     (E    : in out Context;
      Hash :    out AWS.OpenID.Encoders.SHA1.Base64_Digest)
   is
      Buf : Ada.Streams.Stream_Element_Array (1 .. Hash'Length);
      for Buf'Address use Hash'Address;
      pragma Import (Ada, Buf);

      B       : AWS.OpenID.Encoders.Base64.Encoder;
      Encoded : Ada.Streams.Stream_Element_Offset;
      H       : AWS.OpenID.Encoders.SHA1.Hash_Array;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      Finish (E, H);
      B.Transform (Data => H, Into => Buf, Last => Last, Encoded => Encoded);
   end Finish_Base64;

   ---------------
   --  Set_Key  --
   ---------------

   procedure Set_Key
     (E   : in out Context;
      Key : in     String)
   is
      Buf : Ada.Streams.Stream_Element_Array (1 .. Key'Length);
      for Buf'Address use Key'Address;
      pragma Import (Ada, Buf);
   begin
      Set_Key (E, Buf);
   end Set_Key;

   ---------------
   --  Set_Key  --
   ---------------

   procedure Set_Key
     (E   : in out Context;
      Key : in     Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element;
   begin
      --  Reduce the key
      if Key'Length > 64 then
         AWS.OpenID.Encoders.SHA1.Update (E.SHA, Key);
         AWS.OpenID.Encoders.SHA1.Finish (E.SHA, E.Key (0 .. 19));
         E.Key_Len := 19;
      else
         E.Key_Len := Key'Length - 1;
         E.Key (0 .. E.Key_Len) := Key;
      end if;

      --  Hash the key in the SHA1 context.
      declare
         Block : Ada.Streams.Stream_Element_Array (0 .. 63);
      begin
         for I in 0 .. E.Key_Len loop
            Block (I) := IPAD xor E.Key (I);
         end loop;

         for I in E.Key_Len + 1 .. 63 loop
            Block (I) := IPAD;
         end loop;

         AWS.OpenID.Encoders.SHA1.Update (E.SHA, Block);
      end;
   end Set_Key;

   ------------
   --  Sign  --
   ------------

   function Sign
     (Key  : in String;
      Data : in String) return AWS.OpenID.Encoders.SHA1.Digest
   is
      Ctx    : Context;
      Result : AWS.OpenID.Encoders.SHA1.Digest;
   begin
      Set_Key (Ctx, Key);
      Update (Ctx, Data);
      Finish (Ctx, Result);

      return Result;
   end Sign;

   -------------------
   --  Sign_Base64  --
   -------------------

   function Sign_Base64
     (Key  : in String;
      Data : in String)
      return AWS.OpenID.Encoders.SHA1.Base64_Digest
   is
      Ctx    : Context;
      Result : AWS.OpenID.Encoders.SHA1.Base64_Digest;
   begin
      Set_Key (Ctx, Key);
      Update (Ctx, Data);
      Finish_Base64 (Ctx, Result);

      return Result;
   end Sign_Base64;

   --------------
   --  Update  --
   --------------

   procedure Update
     (E : in out Context;
      S : in     String)
   is
   begin
      AWS.OpenID.Encoders.SHA1.Update (E.SHA, S);
   end Update;

   --------------
   --  Update  --
   --------------

   procedure Update
     (E : in out Context;
      S : in Ada.Streams.Stream_Element_Array)
   is
   begin
      AWS.OpenID.Encoders.SHA1.Update (E.SHA, S);
   end Update;

end AWS.OpenID.Encoders.HMAC.SHA1;
