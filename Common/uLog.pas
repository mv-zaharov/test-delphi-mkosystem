unit uLog;

interface

uses
  System.IOUtils,
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  uWinModule;



procedure ToLog(const LogName,Text: string; FlagTime: Boolean = True);

implementation

var
  Lock: TCriticalSection;
  DirectoryCreated: Boolean=False;
  DirectoryResult: string;

procedure ToLog(const LogName,Text: string; FlagTime: Boolean = True);
var
  Msg: string;
begin

  {$IFDEF MSWINDOWS}


  if not DirectoryExists(DirectoryResult) then
  begin
    DirectoryCreated:=True;
    TDirectory.CreateDirectory(DirectoryResult);
  end;

  Lock.Enter;
  try
    if FlagTime then Msg:= FormatDateTime('[dd.mm.yyyy hh:nn:ss.zzz] ',Now) + #13#10;

    TFile.AppendAllText(TPath.Combine(DirectoryResult,LogName+'.txt'),Msg+Text+#13#10,TEncoding.UTF8);
  finally
    Lock.Leave;
  end;

  {$ENDIF}

end;

initialization

  Lock:=TCriticalSection.Create;
  DirectoryResult := GetResultDir;

finalization

  Lock.Free;

end.

