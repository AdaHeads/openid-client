-----------------------------------------------------------------------
--  asf.requests -- ASF Requests
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with ASF.Servlets;

with Util.Strings;

--  The <b>ASF.Requests</b> package is an Ada implementation of
--  the Java servlet request (JSR 315 3. The Request).
package body ASF.Requests is

   --  ------------------------------
   --  Get the size of the mime part.
   --  ------------------------------
   function Get_Size (Data : in Part) return Natural is
   begin
      return Data.Size;
   end Get_Size;

   --  ------------------------------
   --  Get the content name submitted in the mime part.
   --  ------------------------------
   function Get_Name (Data : in Part) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Data.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the content type of the part.
   --  ------------------------------
   function Get_Content_Type (Data : in Part) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Data.Content_Type);
   end Get_Content_Type;

   --  Get
   --     function Get_Stream (Data : in Part) return Util.Stream.Buffer;
   procedure Save (Data : in Part;
                   Path : in String) is
   begin
      null;
   end Save;

   --  ------------------------------
   --  Returns the value of the named attribute as an Object, or null if no attribute
   --  of the given name exists.
   --
   --  Attributes can be set two ways. The servlet container may set attributes to make
   --  available custom information about a request. For example, for requests made
   --  using HTTPS, the attribute javax.servlet.request.X509Certificate can be used
   --  to retrieve information on the certificate of the client. Attributes can also
   --  be set programatically using setAttribute(String, Object).
   --  This allows information to be embedded into a request before
   --  a RequestDispatcher call.
   --  ------------------------------
   function Get_Attribute (Req  : in Request;
                           Name : in String) return EL.Objects.Object is
      Pos : constant EL.Objects.Maps.Cursor := Req.Attributes.Find (Name);
   begin
      if EL.Objects.Maps.Has_Element (Pos) then
         return EL.Objects.Maps.Element (Pos);
      else
         return EL.Objects.Null_Object;
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Stores an attribute in this request. Attributes are reset between requests.
   --  This method is most often used in conjunction with RequestDispatcher.
   --
   --  If the object passed in is null, the effect is the same as calling
   --  removeAttribute(java.lang.String).  It is warned that when the request is
   --  dispatched from the servlet resides in a different web application by
   --  RequestDispatcher, the object set by this method may not be correctly
   --  retrieved in the caller servlet.
   --  ------------------------------
   procedure Set_Attribute (Req   : in out Request;
                            Name  : in String;
                            Value : in EL.Objects.Object) is
   begin
      if EL.Objects.Is_Null (Value) then
         Req.Attributes.Delete (Name);
      else
         Req.Attributes.Include (Name, Value);
      end if;
   end Set_Attribute;

   --  ------------------------------
   --  Stores a list of attributes in this request.
   --  ------------------------------
   procedure Set_Attributes (Req        : in out Request;
                             Attributes : in Util.Beans.Objects.Maps.Map) is

      procedure Set_Attribute (Name  : in String;
                               Value : in Util.Beans.Objects.Object) is
      begin
         if EL.Objects.Is_Null (Value) then
            Req.Attributes.Delete (Name);
         else
            Req.Attributes.Include (Name, Value);
         end if;
      end Set_Attribute;

      Iter : Util.Beans.Objects.Maps.Cursor := Attributes.First;
   begin
      while Util.Beans.Objects.Maps.Has_Element (Iter) loop
         Util.Beans.Objects.Maps.Query_Element (Iter, Set_Attribute'Access);
         Util.Beans.Objects.Maps.Next (Iter);
      end loop;
   end Set_Attributes;

   --  ------------------------------
   --  Removes an attribute from this request. This method is not generally needed
   --  as attributes only persist as long as the request is being handled.
   --  ------------------------------
   procedure Remove_Attribute (Req  : in out Request;
                               Name : in String) is
   begin
      Req.Set_Attribute (Name => Name, Value => EL.Objects.Null_Object);
   end Remove_Attribute;

   --  ------------------------------
   --  Iterate over the request attributes and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Attributes (Req     : in Request;
                                 Process : not null access
                                   procedure (Name  : in String;
                                              Value : in EL.Objects.Object)) is

      procedure Process_Wrapper (Position : in EL.Objects.Maps.Cursor);

      procedure Process_Wrapper (Position : in EL.Objects.Maps.Cursor) is
      begin
         Process.all (Name  => EL.Objects.Maps.Key (Position),
                      Value => EL.Objects.Maps.Element (Position));
      end Process_Wrapper;

   begin
      Req.Attributes.Iterate (Process => Process_Wrapper'Access);
   end Iterate_Attributes;

   --  Returns the name of the character encoding used in the body of this request.
   --  This method returns null if the request does not specify a character encoding
   function Get_Character_Encoding (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "UTF-8";
   end Get_Character_Encoding;

   --  Overrides the name of the character encoding used in the body of this request.
   --  This method must be called prior to reading request parameters or reading input
   --  using getReader(). Otherwise, it has no effect.
   procedure Set_Character_Encoding (Req : in out Request;
                                     Encoding : in String) is
   begin
      null;
   end Set_Character_Encoding;

   --  ------------------------------
   --  Returns the length, in bytes, of the request body and made available by the
   --  input stream, or -1 if the length is not known. For HTTP servlets,
   --  same as the value of the CGI variable CONTENT_LENGTH.
   --  ------------------------------
   function Get_Content_Length (Req : in Request) return Integer is
   begin
      return Request'Class (Req).Get_Int_Header ("Content-Length");
   end Get_Content_Length;

   --  ------------------------------
   --  Returns the MIME type of the body of the request, or null if the type is
   --  not known. For HTTP servlets, same as the value of the CGI variable CONTENT_TYPE.
   --  ------------------------------
   function Get_Content_Type (Req : in Request) return String is
   begin
      return Request'Class (Req).Get_Header ("Content-Type");
   end Get_Content_Type;

   --  Returns the value of a request parameter as a String, or null if the
   --  parameter does not exist. Request parameters are extra information sent with
   --  the request. For HTTP servlets, parameters are contained in the query string
   --  or posted form data.
   --
   --  You should only use this method when you are sure the parameter has only one
   --  value. If the parameter might have more than one value, use
   --  Get_Parameter_Values(String).
   --
   --  If you use this method with a multivalued parameter, the value returned is
   --  equal to the first value in the array returned by Get_Parameter_Values.
   --
   --  If the parameter data was sent in the request body, such as occurs with
   --  an HTTP POST request, then reading the body directly via getInputStream()
   --  or getReader() can interfere with the execution of this method.
   --
--     function Get_Parameter (Req  : in Request;
--                             Name : in String)
--                             return String is abstract;

   --  Returns an array of String objects containing all of the values the given
   --  request parameter has, or null if the parameter does not exist.
   --
   --  If the parameter has a single value, the array has a length of 1.
   function Get_Parameter_Values (Req  : in Request;
                                  Name : in String) return String is
      pragma Unreferenced (Req, Name);
   begin
      return "";
   end Get_Parameter_Values;

   --  ------------------------------
   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   --  ------------------------------
   function Get_Protocol (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "HTTP/1.1";
   end Get_Protocol;

   --  Returns the name of the scheme used to make this request, for example, http,
   --  https, or ftp. Different schemes have different rules for constructing URLs,
   --  as noted in RFC 1738.
   function Get_Scheme (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "http";
   end Get_Scheme;

   --  Returns the host name of the server to which the request was sent.
   --  It is the value of the part before ":" in the Host  header value, if any,
   --  or the resolved server name, or the server IP address.
   function Get_Server_Name (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Server_Name;

   --  Returns the port number to which the request was sent. It is the value of the
   --  part after ":" in the Host  header value, if any, or the server port where the
   --  client connection was accepted on.
   function Get_Server_Port (Req : in Request) return Natural is
      pragma Unreferenced (Req);
   begin
      return 0;
   end Get_Server_Port;

   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   function Get_Remote_Addr (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Remote_Addr;

   --  Returns the fully qualified name of the client or the last proxy that sent
   --  the request. If the engine cannot or chooses not to resolve the hostname
   --  (to improve performance), this method returns the dotted-string form of the
   --  IP address. For HTTP servlets, same as the value of the CGI variable REMOTE_HOST.
   function Get_Remote_Host (Req : in Request) return String is
   begin
      return Request'Class (Req).Get_Remote_Addr;
   end Get_Remote_Host;
--
--     --
--     procedure Compute_Locale (Req : in Request) is
--        H : constant String := Req.Get_Header ("Accept-Language");
--        P : Natural;
--        I : Positive := H'First;
--     begin
--        while I < H'Last loop
--           P := Util.Strings.Index (Source => H,
--                                    Char   => ';',
--                                    From   => I);
--           if P = 0 then
--              P := H'Last + 1;
--           end if;
--           Check_Locale (Req, H (I .. P - 1));
--           I := P + 1;
--        end loop;
--     end Compute_Locale;

   --
   procedure Compute_Locale (Req : in Request) is
      H    : constant String := Req.Get_Header ("Accept-Language");
      P    : Natural;
      I    : Positive := H'First;
      Last : Natural := Util.Strings.Index (H, ';');
   begin
      --  Limited calculation of the locale.
      if Last = 0 then
         Last := H'Last;
      else
         Last := Last - 1;
      end if;
      while I <= Last loop
         P := Util.Strings.Index (Source => H,
                                  Char   => ',',
                                  From   => I);
         if P = 0 or P > Last then
            P := Last + 1;
         end if;
--           Check_Locale (Req, H (I .. P - 1));
         I := P + 1;
      end loop;
   end Compute_Locale;

   --  Returns the preferred Locale that the client will accept content in, based
   --  on the Accept-Language header. If the client request doesn't provide an
   --  Accept-Language header, this method returns the default locale for the server.
   function Get_Locale (Req : in Request) return Util.Locales.Locale is
      pragma Unreferenced (Req);
   begin
      return Util.Locales.ENGLISH;
   end Get_Locale;

   --  Returns an Enumeration of Locale objects indicating, in decreasing order
   --  starting with the preferred locale, the locales that are acceptable to the
   --  client based on the Accept-Language header. If the client request doesn't
   --  provide an Accept-Language header, this method returns an Enumeration containing
   --  one Locale, the default locale for the server.
   function Get_Locales (Req : in Request) return Util.Locales.Locale is
      pragma Unreferenced (Req);
   begin
      return Util.Locales.ENGLISH;
   end Get_Locales;

   --  Returns a boolean indicating whether this request was made using a secure
   --  channel, such as HTTPS.
   function Is_Secure (Req : in Request) return Boolean is
      pragma Unreferenced (Req);
   begin
      return False;
   end Is_Secure;

   --  Returns the Internet Protocol (IP) source port of the client or last proxy
   --  that sent the request.
   function Get_Remote_Port (Req : in Request) return Natural is
      pragma Unreferenced (Req);
   begin
      return 0;
   end Get_Remote_Port;

   --  Returns the host name of the Internet Protocol (IP) interface on which
   --  the request was received.
   function Get_Local_Name (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Local_Name;

   --  Returns the Internet Protocol (IP) address of the interface on which the
   --  request was received.
   function Get_Local_Addr (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Local_Addr;

   --  Returns the Internet Protocol (IP) port number of the interface on which
   --  the request was received.
   function Get_Local_Port (Req : in Request) return Natural is
      pragma Unreferenced (Req);
   begin
      return 0;
   end Get_Local_Port;

   --  Returns the name of the authentication scheme used to protect the servlet.
   --  All servlet containers support basic, form and client certificate authentication,
   --  and may additionally support digest authentication. If the servlet is not
   --  authenticated null is returned.
   function Get_Auth_Type (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Auth_Type;

   --  ------------------------------
   --  Make sure the cookies are loaded in the request object.
   --  ------------------------------
   procedure Load_Cookies (Req : in Request'Class) is
      use type ASF.Cookies.Cookie_Array_Access;
   begin
      if Req.Info.Cookies = null then
         Req.Info.Cookies := ASF.Cookies.Get_Cookies (Req.Get_Header ("Cookie"));
      end if;
   end Load_Cookies;

   --  ------------------------------
   --  Returns an array containing all of the Cookie  objects the client sent with
   --  this request. This method returns null if no cookies were sent.
   --  ------------------------------
   function Get_Cookies (Req : in Request) return ASF.Cookies.Cookie_Array is
   begin
      Req.Load_Cookies;
      return Req.Info.Cookies.all;
   end Get_Cookies;

   --  ------------------------------
   --  Iterate over the request cookies and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Cookies (Req     : in Request;
                              Process : not null access
                                procedure (Cookie : in ASF.Cookies.Cookie)) is
      use type ASF.Cookies.Cookie_Array_Access;
   begin
      Req.Load_Cookies;
      for I in Req.Info.Cookies'Range loop
         Process (Cookie => Req.Info.Cookies (I));
      end loop;
   end Iterate_Cookies;

   --  Returns the value of the specified request header as a long value that
   --  represents a Date object. Use this method with headers that contain dates,
   --  such as If-Modified-Since.
   --
   --  The date is returned as the number of milliseconds since January 1, 1970 GMT.
   --  The header name is case insensitive.
   --
   --  If the request did not have a header of the specified name, this method
   --  returns -1. If the header can't be converted to a date, the method throws
   --  an IllegalArgumentException.
   function Get_Date_Header (Req  : in Request;
                             Name : in String) return Ada.Calendar.Time is
      Header : constant String := Request'Class (Req).Get_Header (Name);
   begin
      return Ada.Calendar.Clock;
   end Get_Date_Header;


   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   function Get_Header (Req  : in Request;
                        Name : in String) return String is
      pragma Unreferenced (Req, Name);
   begin
      return "";
   end Get_Header;

   --  Returns all the values of the specified request header as an Enumeration
   --  of String objects.
   --
   --  Some headers, such as Accept-Language can be sent by clients as several headers
   --  each with a different value rather than sending the header as a comma
   --  separated list.
   --
   --  If the request did not include any headers of the specified name, this method
   --  returns an empty Enumeration. The header name is case insensitive. You can use
   --  this method with any request header.
   function Get_Headers (Req  : in Request;
                         Name : in String) return String is
      pragma Unreferenced (Req, Name);
   begin
      return "";
   end Get_Headers;

   --  Returns the value of the specified request header as an int. If the request
   --  does not have a header of the specified name, this method returns -1.
   --  If the header cannot be converted to an integer, this method throws
   --  a NumberFormatException.
   --
   --  The header name is case insensitive.
   function Get_Int_Header (Req  : in Request;
                            Name : in String) return Integer is
      Value : constant String := Req.Get_Header (Name);
   begin
      return Integer'Value (Value);
   end Get_Int_Header;

   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   function Get_Method (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "GET";
   end Get_Method;

   --  Returns any extra path information associated with the URL the client sent when
   --  it made this request. The extra path information follows the servlet path but
   --  precedes the query string and will start with a "/" character.
   function Get_Path_Info (Req : in Request) return String is
   begin
      return To_String (Req.Path_Info);
   end Get_Path_Info;

   --  ------------------------------
   --  Returns the portion of the request URI that indicates the context of the
   --  request. The context path always comes first in a request URI. The path
   --  starts with a "/" character but does not end with a "/" character.
   --  For servlets in the default (root) context, this method returns "".
   --  The container does not decode this string.
   --  ------------------------------
   function Get_Context_Path (Req : in Request) return String is
   begin
      if Req.Servlet = null then
         return "/";
      else
         return Req.Servlet.Get_Servlet_Context.Get_Context_Path;
      end if;
   end Get_Context_Path;

   --  Returns the query string that is contained in the request URL after the path.
   --  This method returns null  if the URL does not have a query string. Same as the
   --  value of the CGI variable QUERY_STRING.
   function Get_Query_String (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Query_String;

   --  ------------------------------
   --  Returns the login of the user making this request, if the user has been
   --  authenticated, or null if the user has not been authenticated. Whether
   --  the user name is sent with each subsequent request depends on the browser
   --  and type of authentication. Same as the value of the CGI variable REMOTE_USER.
   --  ------------------------------
   function Get_Remote_User (Req : in Request) return String is
      use type Security.Permissions.Principal_Access;
      Principal : constant Security.Permissions.Principal_Access := Req.Get_User_Principal;
   begin
      if Principal = null then
         return "";
      else
         return Principal.Get_Name;
      end if;
   end Get_Remote_User;

   --  ------------------------------
   --  Returns a Principal object containing the name of the current
   --  authenticated user. If the user has not been authenticated, the method returns null.
   --  ------------------------------
   function Get_User_Principal (Req : in Request) return Security.Permissions.Principal_Access is
   begin
      if not Req.Info.Session_Initialized then
         --  Look if the session exist
         if not Req.Has_Session then
            return null;
         end if;
         Req.Info.Session_Initialized := True;
      end if;
      if not Req.Info.Session.Is_Valid then
         return null;
      end if;
      return Req.Info.Session.Get_Principal;
   end Get_User_Principal;

   --  ------------------------------
   --  Returns the session ID specified by the client. This may not be the same as
   --  the ID of the current valid session for this request. If the client did not
   --  specify a session ID, this method returns null.
   --  ------------------------------
   function Get_Request_Session_Id (Req : in Request) return String is
   begin
      Req.Load_Cookies;
      for I in Req.Info.Cookies'Range loop
         if ASF.Cookies.Get_Name (Req.Info.Cookies (I)) = "SID" then
            return ASF.Cookies.Get_Value (Req.Info.Cookies (I));
         end if;
      end loop;
      return "";
   end Get_Request_Session_Id;

   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   function Get_Request_URI (Req : in Request) return String is
      pragma Unreferenced (Req);
   begin
      return "";
   end Get_Request_URI;

   --  Reconstructs the URL the client used to make the request. The returned URL
   --  contains a protocol, server name, port number, and server path, but it does
   --  not include query string parameters.
   --
   --  If this request has been forwarded using RequestDispatcher.forward(Request, Response),
   --  the server path in the reconstructed URL must reflect the path used to
   --  obtain the RequestDispatcher, and not the server path specified by the client.
   --
   --  Because this method returns a StringBuffer, not a string, you can modify the
   --  URL easily, for example, to append query parameters.
   --
   --  This method is useful for creating redirect messages and for reporting errors.
   function Get_Request_URL (Req : in Request) return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Req);
      Res : Ada.Strings.Unbounded.Unbounded_String;
   begin
      return Res;
   end Get_Request_URL;

   --  ------------------------------
   --  Returns the part of this request's URL that calls the servlet. This path starts
   --  with a "/" character and includes either the servlet name or a path to the
   --  servlet, but does not include any extra path information or a query string.
   --  Same as the value of the CGI variable SCRIPT_NAME.
   --
   --  This method will return an empty string ("") if the servlet used to process
   --  this request was matched using the "/*" pattern.
   --  ------------------------------
   function Get_Servlet_Path (Req : in Request) return String is
   begin
      return Req.Servlet.Get_Name;
   end Get_Servlet_Path;


   --  ------------------------------
   --  Get and check the request session
   --  ------------------------------
   function Has_Session (Req : in Request'Class) return Boolean is
   begin
      Req.Load_Cookies;
      for I in Req.Info.Cookies'Range loop
         if ASF.Cookies.Get_Name (Req.Info.Cookies (I)) = "SID" then
            declare
               SID : constant String := ASF.Cookies.Get_Value (Req.Info.Cookies (I));
               Ctx : constant Servlets.Servlet_Registry_Access := Req.Servlet.Get_Servlet_Context;
            begin
               Ctx.Find_Session (Id     => SID,
                                 Result => Req.Info.Session);
               return Req.Info.Session.Is_Valid;
            end;
         end if;
      end loop;
      return False;
   end Has_Session;

   --  ------------------------------
   --  Returns the current HttpSession  associated with this request or, if there
   --  is no current session and create is true, returns a new session.
   --
   --  If create is false  and the request has no valid HttpSession, this method
   --  returns null.
   --
   --  To make sure the session is properly maintained, you must call this method
   --  before the response is committed. If the container is using cookies to maintain
   --  session integrity and is asked to create a new session when the response is
   --  committed, an IllegalStateException is thrown.
   --  ------------------------------
   function Get_Session (Req    : in Request;
                         Create : in Boolean := False) return ASF.Sessions.Session is
   begin
      if not Req.Info.Session_Initialized then
         --  Look if the session exist
         if not Req.Has_Session then
            null;
         end if;
         Req.Info.Session_Initialized := True;
      end if;

      --  Create the session if necessary.
      if Create and not Req.Info.Session.Is_Valid then
         declare
            Ctx : constant ASF.Servlets.Servlet_Registry_Access
              := Req.Servlet.Get_Servlet_Context;
         begin
            Ctx.Create_Session (Req.Info.Session);
            declare
               C : ASF.Cookies.Cookie
                 := ASF.Cookies.Create ("SID", Req.Info.Session.Get_Id);
            begin
               ASF.Cookies.Set_Path (C, Req.Get_Context_Path);
               Req.Info.Response.Add_Cookie (Cookie => C);
            end;
         end;
      end if;
      return Req.Info.Session;
   end Get_Session;

   --  ------------------------------
   --  Set the path info
   --  ------------------------------
   procedure Set_Path_Info (Req  : in out Request;
                            Path : in String) is
   begin
      Req.Path_Info := To_Unbounded_String (Path);
   end Set_Path_Info;

   --  ------------------------------
   --  Returns True if the request is an AJAX request.
   --  ------------------------------
   function Is_Ajax_Request (Req : in Request) return Boolean is
      Header : constant String := Request'Class (Req).Get_Header ("X-Requested-With");
   begin
      return Header'Length > 0;
   end Is_Ajax_Request;

   --  ------------------------------
   --  Returns the absolute path of the resource identified by the given relative path.
   --  The resource is searched in a list of directories configured by the application.
   --  The path must begin with a "/" and is interpreted as relative to the current
   --  context root.
   --
   --  This method allows the servlet container to make a resource available to
   --  servlets from any source.
   --
   --  This method returns an empty string if the resource could not be localized.
   --  ------------------------------
   function Get_Resource (Req  : in Request;
                          Path : in String) return String is
   begin
      return Req.Servlet.Get_Servlet_Context.Get_Resource (Path);
   end Get_Resource;

   --  ------------------------------
   --  Initialize the request object.
   --  ------------------------------
   overriding
   procedure Initialize (Req : in out Request) is
   begin
      Req.Info := new Request_Data;
   end Initialize;

   --  ------------------------------
   --  Finalize the request object.
   --  ------------------------------
   overriding
   procedure Finalize (Req : in out Request) is
      procedure Free is new Ada.Unchecked_Deallocation (Request_Data, Request_Data_Access);
      procedure Free is new Ada.Unchecked_Deallocation (ASF.Cookies.Cookie_Array,
                                                        ASF.Cookies.Cookie_Array_Access);
   begin
      Free (Req.Info.Cookies);
      Free (Req.Info);
   end Finalize;

end ASF.Requests;
