library mod_HelloWorld;

uses
  ActiveX,
  ComObj,
  WebBroker,
  WebFact,
  HTTPApp,
  SBD.AP24.httpd in 'SBD.AP24.httpd.pas',
  SBD.AP24.ApacheHTTP in 'SBD.AP24.ApacheHTTP.pas',
  SBD.AP24.ApacheApp in 'SBD.AP24.ApacheApp.pas',
  SBD.HTTPDCustomDirective in 'SBD.HTTPDCustomDirective.pas',
  uHelloWorldApacheApp in 'uHelloWorldApacheApp.pas',
  wmHelloWorld in 'wmHelloWorld.pas' {PageProducerPage1: TWebAppPageModule} {*.html};

{$E .so}

{$R *.res}

const
  ModuleName  = 'helloworld';
  HandlerName = 'mod_helloworld-handler';

exports
  apache_module name ModuleName;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application := THelloWorldApacheApp.CreateApache2App( nil, @apache_module, ModuleName, HandlerName);

  // Add one of these lines for each additional handler you have defined in httpd.conf
  (Application as THelloWorldApacheApp).RegisterHandler( HandlerName);

  // Add your custom directives like this example ...
  (Application as THelloWorldApacheApp).AddDirective( TTestDirective.CreateDirective);

  // Register all web modules like this ...
  Application.AddWebModuleFactory(
    TWebAppPageModuleFactory.Create(
      TPageProducerPage1,
      TWebPageInfo.Create( [wpPublished], '.html'), caCache));

  Application.Initialize;
  Application.Run
end.

