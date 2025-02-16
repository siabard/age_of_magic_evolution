unit listutil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;


generic procedure RemoveFrommTail<T>(Src: specialize TList<T>; AItem: T);

implementation

generic procedure RemoveFrommTail<T>(Src: specialize TList<T>; AItem: T);
var
  Index, LastIndex: integer;
begin
  Index := Src.IndexOf(AItem);
  if Index <> -1 then
  begin
    LastIndex := Src.Count - 1;
    if Index <> LastIndex then
    begin
      // 마지막 요소와 교체
      Src.Exchange(LastIndex, Index);
    end;
    // 마지막 요소 삭제
    AItem.Free;
    Src.Delete(LastIndex);
  end;
end;

end.
