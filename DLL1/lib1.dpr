library lib1;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  uFindModule in 'uFindModule.pas',
  uWinModule in 'uWinModule.pas',
  uLog in '..\Common\uLog.pas',
  uTypeModule in '..\Common\uTypeModule.pas';

{$R *.res}


function FindFiles(ID: Integer; Data: PPAnsiCharArray; PLengths: PIntegerArray; Count: Integer): Integer; stdcall;
  // ID - ������������� ����������� ������, ����� ��������� ��������� ������
  // Data - ��������� �� ������ �� ��������
  // PLengths - ��������� �� ������ � ������� �����
  // Count - ���������� ������������ �����
  // Result = ���������� ��������� ������.
var
  str,resultFileName: string;
  StartDir: string;
  Masks: array of string;
  tmpText: TStrings;
  rs: TFileSearchResult;
  fBytes: TBytes;
  i,Len: Integer;
begin
  Result := 0;

  StartDir := '';
  tmpText := TStringList.Create;

  resultFileName := ID.ToString; //��� �����, ���� ��������� ���������

  try

    //������������ ���������� ����� � ������
    SetLength(Masks, Count-1); // �.�. � ������ ������ ��������� ���������� ��� ������

    for i := 0 to Count-1 do
    begin
      Len := PLengths^[i];
      SetLength(fBytes, Len);

      Move(Data^[i]^, fBytes[0], Len);
      str := TEncoding.ANSI.GetString(fBytes);

      if i<1                  // � ������ ������ ��������� ���������� ��� ������
        then StartDir := str
        else Masks[i-1] := str; //��������� ����� ��� ������

    end;


    if ( (Count < 1) or (StartDir.Length < 3) )
    then begin
      // ������ ������ ��������� ��� ��� �����������
      tmpText.Add('Number of files (directories) = 0');
      ToLog(resultFileName, '������: ������������ ���������');
    end
    else begin

      if (length(Masks)<1)
      then begin
        //�� ������ �� ����� ����� ��� ������
        SetLength(Masks,1);
        Masks[0] := '*.*';  //����� ���� ������
      end;

      rs := FFindFiles(Masks, StartDir);  //��������� ������ ������

      Result := rs.FileCount;

      //�������� ���������� ������
      tmpText.Add('Folder: ' + StartDir);
      tmpText.Add('Number of files (directories): ' + rs.FileCount.ToString);
      tmpText.AddStrings(rs.FilePaths);

      ToLog(resultFileName, tmpText.Text, False);

    end;


  finally

    SetLength(Masks,0);
    rs.FilePaths.Free;
    tmpText.Free;

  end;

end; // FindFiles(...)


function FindTextOccurrences(ID: Integer; Data: PPAnsiCharArray; PLengths: PIntegerArray; Count: Integer): Integer; stdcall;
  // ID - ������������� ����������� ������, ����� ��������� ��������� ������
  // Data - ��������� �� ������ �� ��������
  // PLengths - ��������� �� ������ � ������� �����
  // Count - ���������� ������������ �����
  // Result = ���������� ��������� �������������������.
var
  str,key,
  resultFileName,
  dataFileName: string;
  sequences: array of string;
  tmpText: TStrings;
  rs: TTextSearchResult;
  fBytes: TBytes;
  i,Len, value: Integer;
  Values: TList<Integer>;


begin
  Result := 0;


  dataFileName := '';
  tmpText := TStringList.Create;

  resultFileName := ID.ToString; //��� �����, ���� ��������� ���������

  try

    //������������ ���������� ����� � ������
    SetLength(sequences, Count-1); // �.�. � ������ ������ ��������� ��� ����� � ������� ��� ������

    for i := 0 to Count-1 do
    begin
      Len := PLengths^[i];
      SetLength(fBytes, Len);
      Move(Data^[i]^, fBytes[0], Len);
      str := TEncoding.ANSI.GetString(fBytes);

      if i<1                  // �.�. � ������ ������ ��������� ��� ����� � ������� ��� ������
        then dataFileName := str
        else sequences[i-1] := str; //��������� ������������������� ��� ������
    end;



    if ( (Count < 2) or (dataFileName.Length < 3) )
    then begin
      // ������ ������ ��������� ��� ��� �����������
      tmpText.Add('Number of sequences = 0');
      ToLog(resultFileName, '������: ������������ ���������');
    end
    else begin


      rs := FFindTextOccurrences(sequences, dataFileName);  //��������� ������ ����������

      Result := rs.TextCount;

      //�������� ���������� ������
      tmpText.Add('File: ' + dataFileName);
      tmpText.Add('Number of sequences: ' + rs.TextCount.ToString);


      for key in rs.TextPositions.Keys do
      begin
        tmpText.Add(key + ': ');
        Values := rs.TextPositions[key];
        for value in Values do
          tmpText.Add('     ' + value.ToString + ', ');
      end;

      ToLog(resultFileName, tmpText.Text, False);

    end;


  finally

    SetLength(sequences,0);
    if Assigned(rs.TextPositions)
    then begin
      for key in rs.TextPositions.Keys do rs.TextPositions[key].Free;
      rs.TextPositions.Free;
    end;
    tmpText.Free;

  end;

end; // FindTextOccurrences(...)


exports
  FindFiles,
  FindTextOccurrences;

begin
end.
