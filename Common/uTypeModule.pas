unit uTypeModule;

interface
uses
  System.Generics.Collections,
  System.Classes;

type
  TFileSearchResult = record
    FileCount: Integer;
    FilePaths: TStrings;
  end;

  TTextSearchResult = record
    TextCount: Integer;
    TextPositions: TDictionary<string, TList<Integer>>;
  end;

  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array of PAnsiChar;  // ќбъ€вл€ем открытый массив со строками
  //перва€ строка в массиве строк - это директори€ дл€ поиска или полное им€ файла

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array of Integer;  // ќткрытый массив с длинами строк

  TProgressCallback = procedure(ID, Progress: Integer; Status: PAnsiChar); stdcall;

implementation

end.
