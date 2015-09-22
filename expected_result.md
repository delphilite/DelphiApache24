If you compile and deploy this example project as directed by the [Configuration](Configuration.md) page of this wiki, if you enter into your browser a URL like ...
```
https://bla.com/hello/somePage?someParam=someValue
```

where
  * bla.com is the domain that you configured your instance of Apache for,

then the web request should be intercepted by the Delphi module and a table of server variables and assorted data should be returned.

This demonstration application was tested with:
  * XAMPP v3.2.1
  * which includes Apache 2.4.7 (Win32)
  * Serving https on port 8443
  * The installer was ...
    * xampp-win32-1.8.2-4-VC9-installer.exe
  * ... which can be located ..
    * Page = http://sourceforge.net/projects/xampp/files/XAMPP%20Windows/1.8.2/
    * Select Windows 32
    * Select either stand-alone or service