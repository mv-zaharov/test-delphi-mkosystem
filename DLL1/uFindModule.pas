unit uFindModule;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  uTypeModule,
  uLog;



function FFindTextOccurrences(const Sequences: array of string; const FilePath: string): TTextSearchResult;
function FFindFiles(const Masks: array of string; const StartDir: string): TFileSearchResult;

implementation

uses
  System.IOUtils;


function FFindFiles(const Masks: array of string; const StartDir: string): TFileSearchResult;
var
  SearchRec: TSearchRec;
  lowInd,
  highInd,
  l,h,
  I: Integer;
  Mask, FullPath: string;
  Found: Integer;

  procedure SearchInDirectory(const Directory: string);
  var
    SubDir: string;
  begin

    lowInd:= Low(Masks);
    highInd:= High(Masks);
    I := lowInd - 1;   //Сделал так, т.к. IDE попросила упростить индексы цикла FOR I := Low(Masks) to High(Masks) do

    while I<highInd do
    begin
      inc(I);
      Mask := IncludeTrailingPathDelimiter(Directory) + Masks[I];
      //ToLog('log', 'Mask[' + I.ToString + '] = "' + Mask + '".');
      Found := FindFirst(Mask, faAnyFile and not faDirectory, SearchRec);
      try
        while Found = 0 do
        begin
          FullPath := IncludeTrailingPathDelimiter(Directory) + SearchRec.Name;
          Result.FilePaths.Add(FullPath);
          Inc(Result.FileCount);
          Found := FindNext(SearchRec);
        end;
      finally
        FindClose(SearchRec);
      end;
    end;

    // Поиск субдиректорий
    //ToLog('log', 'Поиск субдир: ' + IncludeTrailingPathDelimiter(Directory) + '*');
    Found := FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SearchRec);
    try
      while Found = 0 do
      begin
        SubDir := SearchRec.Name;
        //ToLog('log', 'Dir: ' + IncludeTrailingPathDelimiter(Directory) + SubDir);
        if ( (SearchRec.Attr and faDirectory) <> 0) and (SubDir <> '.') and (SubDir <> '..')
          then begin
            SearchInDirectory(IncludeTrailingPathDelimiter(Directory) + SubDir);
          end;
        Found := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  end;

begin
  Result.FileCount := 0;  // Initialize file count
  Result.FilePaths := TStringList.Create;  // Initialize list for file paths
  SearchInDirectory(StartDir);  // Start recursive search
end;

function FFindTextOccurrences(const Sequences: array of string; const FilePath: string): TTextSearchResult;
var
  FileStream: TFileStream;
  Buffer: TBytes;
  FileSize, I, J, SeqLen, BufLen: Integer;
  Sequence: string;
  Matches: Boolean;
begin
  Result.TextCount := 0;
  Result.TextPositions := TDictionary<string, TList<Integer>>.Create;

  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    FileSize := FileStream.Size;
    SetLength(Buffer, FileSize);
    FileStream.Read(Buffer, 0, FileSize);
    BufLen := Length(Buffer);

    for Sequence in Sequences do
    begin
      Result.TextPositions.Add(Sequence, TList<Integer>.Create);
      SeqLen := Length(Sequence);

      for I := 0 to BufLen - SeqLen do
      begin
        Matches := True;
        for J := 0 to SeqLen - 1 do
        begin
          if Buffer[I + J] <> Byte(Sequence[J+1]) then
          begin
            Matches := False;
            Break;
          end;
        end;

        if Matches then
        begin
          Result.TextPositions[Sequence].Add(I);
          Inc(Result.TextCount);
        end;
      end;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
