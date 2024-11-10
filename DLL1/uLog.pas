unit uLog;

interface

uses
  System.IOUtils,
  System.SysUtils,
  System.SyncObjs,
  System.Classes;

procedure ToLog(const LogName,Text: string);

implementation

var
  Lock: TCriticalSection;
  DirectoryCreated: Boolean=False;

procedure ToLog(const LogName,Text: string);
begin

  {$IFDEF MSWINDOWS}

  if not DirectoryCreated then
  begin
    DirectoryCreated:=True;
    TDirectory.CreateDirectory('logs');
  end;

  Lock.Enter;
  try
    TFile.AppendAllText(TPath.Combine('logs',LogName+'.txt'),FormatDateTime('[dd.mm.yyyy hh:nn:ss.zzz] ',Now)+Text+#13#10,TEncoding.UTF8);
  finally
    Lock.Leave;
  end;

  {$ENDIF}

end;

initialization

  Lock:=TCriticalSection.Create;

finalization

  Lock.Free;

end.

