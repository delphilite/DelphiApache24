object PageProducerPage1: TPageProducerPage1
  OldCreateOrder = False
  PageProducer = PageProducer
  AppServices = WebAppComponents
  Height = 344
  Width = 215
  object PageProducer: TPageProducer
    HTMLDoc.Strings = (
      '<!DOCTYPE html>'
      '<html>'
      '<head>'
      '<style type="text/css">'
      'body {background-color: #ffffff; color: #000000;}'
      'body, td, th, h1, h2 {font-family: sans-serif;}'
      'pre {margin: 0px; font-family: monospace;}'
      
        'a:link {color: #000099; text-decoration: none; background-color:' +
        ' #ffffff;}'
      'a:hover {text-decoration: underline;}'
      'table {border-collapse: collapse;}'
      '.center {text-align: center;}'
      
        '.center table { margin-left: auto; margin-right: auto; text-alig' +
        'n: left;}'
      '.center th { text-align: center !important; }'
      
        'td, th { border: 1px solid #000000; font-size: 75%; vertical-ali' +
        'gn: baseline;}'
      'h1 {font-size: 150%;}'
      'h2 {font-size: 125%;}'
      '.p {text-align: left;}'
      
        '.e {background-color: #ccccff; font-weight: bold; color: #000000' +
        ';}'
      
        '.h {background-color: #9999cc; font-weight: bold; color: #000000' +
        ';}'
      '.v {background-color: #cccccc; color: #000000;}'
      
        '.vr {background-color: #cccccc; text-align: right; color: #00000' +
        '0;}'
      'img {float: right; border: 0px;}'
      
        'hr {width: 600px; background-color: #cccccc; border: 0px; height' +
        ': 1px; color: #000000;}'
      '</style>'
      ''
      '<title>Delphi on Apache 2.4</title>'
      '</head>'
      ''
      '<body>'
      '<table border="0" cellpadding="3" width="600">'
      
        '  <tr class="h"><td><h1 class="p">Hello World web app</h1></td><' +
        '/tr>'
      '</table><br />'
      ''
      '<table border="0" cellpadding="3" width="600">'
      ''
      '<tr>'
      '<td class="e">User_Agent</td><td class="v"><#UserAgent></td>'
      '</tr>'
      ''
      '<tr>'
      
        '<td class="e">SERVER_SOFTWARE</td><td class="v"><#SERVER_SOFTWAR' +
        'E></td>'
      '</tr>'
      ''
      '<tr>'
      '<td class="e">Method</td><td class="v"><#Method></td>'
      '</tr>'
      ''
      ''
      '<tr>'
      
        '<td class="e">ProtocolVersion</td><td class="v"><#ProtocolVersio' +
        'n></td>'
      '</tr>'
      ''
      ''
      '<tr>'
      '<td class="e">PathInfo</td><td class="v"><#PathInfo></td>'
      '</tr>'
      ''
      '<tr>'
      '<td class="e">Query</td><td class="v"><#Query></td>'
      '</tr>'
      ''
      '<tr>'
      
        '<td class="e">PathTranslated</td><td class="v"><#PathTranslated>' +
        '</td>'
      '</tr>'
      ''
      ''
      '<tr>'
      '<td class="e">Accept</td><td class="v"><#Accept></td>'
      '</tr>'
      ''
      ''
      '<tr>'
      '<td class="e">Host</td><td class="v"><#Host></td>'
      '</tr>'
      ''
      ''
      '<tr>'
      '<td class="e">Connection</td><td class="v"><#Connection></td>'
      '</tr>'
      ''
      ''
      '<tr>'
      '<td class="e">RemoteAddr</td><td class="v"><#RemoteAddr></td>'
      '</tr>'
      ''
      ''
      '<tr>'
      '<td class="e">ServerPort</td><td class="v"><#ServerPort></td>'
      '</tr>'
      ''
      '<tr>'
      '<td class="e">Handler</td><td class="v"><#Handler></td>'
      '</tr>'
      ''
      '<tr>'
      
        '<td class="e">pascali-setconfigroot</td><td class="v"><#pascali_' +
        'setconfigroot></td>'
      '</tr>'
      ''
      '</table>'
      '</body>'
      '</html>')
    OnHTMLTag = PageProducerHTMLTag
    ScriptEngine = 'JScript'
    Left = 48
    Top = 8
  end
  object WebAppComponents: TWebAppComponents
    DispatchActions = WebDispatcher
    Left = 48
    Top = 56
  end
  object WebDispatcher: TWebDispatcher
    OldCreateOrder = True
    Actions = <
      item
        Default = True
        Name = 'WebActionItem1'
        PathInfo = '/PageProducer'
        Producer = PageProducer
      end>
    Left = 48
    Top = 112
    Height = 0
    Width = 0
  end
end
