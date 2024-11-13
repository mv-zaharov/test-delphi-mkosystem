unit uFindModule;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  uTypeModule,
  uLog;



function FFindFiles(const Masks: array of string; const StartDir: string): TFileSearchResult;
function FFindTextOccurrences(const Sequences: array of string; const FilePath: string): TTextSearchResult;

implementation

uses
  System.IOUtils;

function FFindFiles(const Masks: array of string; const StartDir: string): TFileSearchResult;
var
  SearchRec: TSearchRec;
  I: Integer;
  Mask: string;
  Found: Integer;
  FullPath: string;
begin
  Result.FileCount := 0;
  Result.FilePaths := TStringList.Create;

  for I := Low(Masks) to High(Masks) do
  begin
    Mask := IncludeTrailingPathDelimiter(StartDir) + Masks[I];

    Found := FindFirst(Mask, faAnyFile and not faDirectory, SearchRec);
    try
      while Found = 0 do
      begin
        FullPath := TPath.Combine(StartDir, SearchRec.Name);
        Result.FilePaths.Add(FullPath);
        Inc(Result.FileCount);
        Found := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  end;
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
