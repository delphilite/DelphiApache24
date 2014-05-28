unit wmHelloWorld;

interface

uses
  SysUtils, Classes, HTTPApp, WebModu, HTTPProd, ReqMulti, WebDisp;

type
  TPageProducerPage1 = class(TWebAppPageModule)
    PageProducer: TPageProducer;
    WebAppComponents: TWebAppComponents;
    WebDispatcher: TWebDispatcher;
    procedure PageProducerHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: string; TagParams: TStrings; var ReplaceText: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation













uses WebBroker, uHelloWorldApacheApp;
{$R *.dfm}  {*.html}



procedure TPageProducerPage1.PageProducerHTMLTag(
  Sender: TObject; Tag: TTag; const TagString: string; TagParams: TStrings;
  var ReplaceText: string);

  function DateVariableAsHtml( Value: TDateTime): string;
  begin
  if Value <= 0.0 then
      result := '(null)'
    else
      result := FormatDateTime( 'dd-mmm-yyyy hh:nn:ss', Value)
  end;

begin
try
  if TagString = 'Referer' then
    ReplaceText := Request.Referer

  else if TagString = 'UserAgent' then
    ReplaceText := Request.UserAgent

  else if TagString = 'URL' then
    ReplaceText := Request.URL

  else if TagString = 'Query' then
    ReplaceText := Request.Query

  else if TagString = 'SERVER_SOFTWARE' then
    ReplaceText := Request.GetFieldByName( 'SERVER_SOFTWARE')

  else if TagString = 'Date' then
    ReplaceText := DateVariableAsHtml( Request.Date)

  else if TagString = 'Expires' then
    ReplaceText := DateVariableAsHtml( Request.Expires)

  else if TagString = 'Method' then
    ReplaceText := Request.Method

  else if TagString = 'ProtocolVersion' then
    ReplaceText := Request.ProtocolVersion

  else if TagString = 'PathInfo' then
    ReplaceText := Request.PathInfo

  else if TagString = 'PathTranslated' then
    ReplaceText := Request.PathTranslated

  else if TagString = 'Authorization' then
    ReplaceText := Request.Authorization

  else if TagString = 'CacheControl' then
    ReplaceText := Request.CacheControl

  else if TagString = 'Accept' then
    ReplaceText := Request.Accept

  else if TagString = 'From' then
    ReplaceText := Request.From

  else if TagString = 'Host' then
    ReplaceText := Request.Host

  else if TagString = 'IfModifiedSince' then
    ReplaceText := DateVariableAsHtml( Request.IfModifiedSince)

  else if TagString = 'ContentEncoding' then
    ReplaceText := Request.ContentEncoding

  else if TagString = 'ContentVersion' then
    ReplaceText := Request.ContentVersion

  else if TagString = 'Connection' then
    ReplaceText := Request.Connection

  else if TagString = 'DerivedFrom' then
    ReplaceText := Request.DerivedFrom

  else if TagString = 'Title' then
    ReplaceText := Request.Title

  else if TagString = 'RemoteAddr' then
    ReplaceText := Request.RemoteAddr

  else if TagString = 'RemoteHost' then
    ReplaceText := Request.RemoteHost

  else if TagString = 'ScriptName' then
    ReplaceText := Request.ScriptName

  else if TagString = 'ServerPort' then
    ReplaceText := IntToStr( Request.ServerPort)

  else if TagString = 'InternalScriptName' then
    ReplaceText := Request.InternalScriptName

  else if TagString = 'Handler' then
    ReplaceText := Request.GetFieldByName('HANDLER')

  else if TagString = 'pascali_setconfigroot' then
    ReplaceText := (WebBroker.Application as THelloWorldApacheApp).FMyCommandValue

except on E: Exception do
    begin
    ReplaceText := Format( 'ERROR! &lt;#%s&gt; raised %s: %s', [TagString, E.ClassName, E.Message])
    end
  end
end;


end.

