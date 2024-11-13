library lib2;
//������������� ������ � zip

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  uTypeModule in '..\Common\uTypeModule.pas',
  uWinModule in '..\Common\uWinModule.pas',
  uLog in '..\Common\uLog.pas';

function GetCurrentProgress: Integer;
begin
  Result := 0; //�������� ��� ������� ���������
end;

procedure CreateZipArchive(ID: Integer; AppPath, SourceDirectory, OutputArchive: PAnsiChar; Callback: TProgressCallback); stdcall;
var
  si: TStartupInfo;
  pi: TProcessInformation;
  ExitCode: DWORD;
  CommandLine: string;
  Progress: Integer;
  resultFN: string;
begin
  //AppPath := 'C:\Program Files\7-Zip\7z.exe'; // ���� � 7-Zip
  //CommandLine := Format('"%s" a "%s" "%s\*" -r', [AppPath, OutputArchive, SourceDirectory]);

  CommandLine := Format('"%s" a -ep1 "%s" "%s\*"', [AppPath, OutputArchive, SourceDirectory]);

  resultFN := ID.ToString;
  ToLog(resultFN,'������ ����������: '+#13#10+CommandLine, false);
  //CommandLine := Format('"%s"', ['C:\Program Files (x86)\WinRAR\Rar.exe x "D:\Projects\Delphi\TestAndSobes\01\1.rar" "D:\Projects\Delphi\TestAndSobes\01\55"']);

  // ������������� �������� ��������
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);

  if CreateProcess(nil, PChar(CommandLine), nil, nil, False, CREATE_NO_WINDOW, nil, nil, si, pi) then
  begin
    try
      ToLog(resultFN,'����� �������� ������������� ');
      repeat
        sleep(500);
        Progress := GetCurrentProgress(); // ���� ����� �������� ��� �������, ��������������� ��������
        if Assigned(Callback) then
          Callback(ID, Progress, 'Running...');
      until WaitForSingleObject(pi.hProcess, 500) <> WAIT_TIMEOUT;

      GetExitCodeProcess(pi.hProcess, ExitCode);
      ToLog(resultFN,'���������� ��������... ');

      if ExitCode = 0 then
      begin
        if Assigned(Callback) then
          Callback(ID, 100, 'Completed');
        ToLog(resultFN,'������� ��������.');
      end
      else
      begin
        if Assigned(Callback) then
          Callback(ID, 0, 'Error');
          ToLog(resultFN,'������ ��� ���������� ��������');
      end;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  end
  else
  begin
    if Assigned(Callback) then
      Callback(ID, 0, 'Failed to start process');
    ToLog(resultFN,'������ ��� ������ �������� �������������', false);
  end;
end;


exports
  CreateZipArchive;

begin
end.
