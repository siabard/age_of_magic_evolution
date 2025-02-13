unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Components, LogUtil;

type
  TEntity = class
  private
    FPosition: TPositionComponent;
    procedure SetPosition(CompPosition: TPositionComponent);
  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write SetPosition;
  end;

implementation

constructor TEntity.Create;
begin

end;

destructor TEntity.Destroy;
begin
  if Assigned(FPosition) then
    begin
    LogDebug('Remove Position Component');
    FreeAndNil(FPosition);

    end;
end;

procedure TEntity.SetPosition(CompPosition: TPositionComponent);
begin
  Self.FPosition := CompPosition;
end;

end.
