--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package AWS.OpenID.State is
   procedure Save (File_Name : in     String);
   --  Remember to call Save before stopping the last HTTP server.
   --  If you compile the AWS.OpenID packages with
   --     AUTHENTICATION_DATABASE_IMPLEMENTATION=session_cookies
   --  Save depends on AWS.Session, which drops all state after the last HTTP
   --  server in the application is stopped.

   procedure Load (File_Name : in     String);
end AWS.OpenID.State;
